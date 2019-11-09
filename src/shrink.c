/*
 * shrink.c - compressor implementation
 *
 * Copyright (C) 2019 Emmanuel Marty
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

/*
 * Uses the libdivsufsort library Copyright (c) 2003-2008 Yuta Mori
 *
 * Inspired by cap by Sven-Åke Dahl. https://github.com/svendahl/cap
 * Also inspired by Charles Bloom's compression blog. http://cbloomrants.blogspot.com/
 * With ideas from LZ4 by Yann Collet. https://github.com/lz4/lz4
 * With help and support from spke <zxintrospec@gmail.com>
 *
 */

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "libapultra.h"
#include "matchfinder.h"
#include "shrink.h"
#include "format.h"

#define TOKEN_PREFIX_SIZE        1 /* literal/ match bit */

#define TOKEN_CODE_LARGE_MATCH   0 /* 0 */
#define TOKEN_SIZE_LARGE_MATCH   1

#define TOKEN_CODE_7BIT_MATCH    2 /* 10 */
#define TOKEN_SIZE_7BIT_MATCH    2

#define TOKEN_CODE_4BIT_MATCH    3 /* 11 */
#define TOKEN_SIZE_4BIT_MATCH    2

#define CountShift(N,bits)  if ((N)>>(bits)) { (N)>>=(bits); (n) += (bits); }

/** Code values for variable 8+gamma2 bits offset + gamma2 len match; 7 bits offset + 1 bit len match; and 4 bits offset + fixed 1 byte len match */
static const int _token_code[3] = { TOKEN_CODE_LARGE_MATCH, TOKEN_CODE_7BIT_MATCH, TOKEN_CODE_4BIT_MATCH };

/** Code sizes for variable 8+gamma2 bits offset + gamma2 len match; 7 bits offset + 1 bit len match; and 4 bits offset + fixed 1 byte len match */
static const int _token_size[3] = { TOKEN_SIZE_LARGE_MATCH, TOKEN_SIZE_7BIT_MATCH, TOKEN_SIZE_4BIT_MATCH };

/** Gamma2 bit counts for common values, up to 255 */
static char _gamma2_size[256] = {
   0, 0, 2, 2, 4, 4, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
};

/**
 * Write bit to output (compressed) buffer
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nBit value of bit to write (0 or 1)
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 *
 * @return updated write index into output buffer, or -1 in case of an error
 */
static int apultra_write_bit(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, const int nBit, int *nCurBitsOffset, int *nCurBitMask) {
   if (nOutOffset < 0) return -1;

   if ((*nCurBitsOffset) == INT_MIN) {
      /* Allocate a new byte in the stream to pack bits in */
      if (nOutOffset >= nMaxOutDataSize) return -1;
      (*nCurBitsOffset) = nOutOffset;
      (*nCurBitMask) = 1 << 7;
      pOutData[nOutOffset++] = 0;
   }

   if (nBit) {
      pOutData[*nCurBitsOffset] |= (*nCurBitMask);
   }

   (*nCurBitMask) >>= 1;
   if ((*nCurBitMask) == 0) {
      /* Current byte is full */
      (*nCurBitsOffset) = INT_MIN;
   }

   return nOutOffset;
}

/**
 * Get size of gamma2 encoded value
 *
 * @param nValue value of evaluate (2..n)
 *
 * @return number of bits required
 */
static int apultra_get_gamma2_size(int nValue) {
   if (nValue >= 0 && nValue < 256)
      return _gamma2_size[nValue];
   else {
      unsigned int n = 0;
      CountShift(nValue, 16);
      CountShift(nValue, 8);
      CountShift(nValue, 4);
      CountShift(nValue, 2);
      CountShift(nValue, 1);

      return n << 1;
   }
}

/**
 * Write gamma2 encoded value to output (compressed) buffer
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nValue value of write (2..n)
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 *
 * @return updated write index into output buffer, or -1 in case of an error
 */
static int apultra_write_gamma2_value(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int nValue, int *nCurBitsOffset, int *nCurBitMask) {
   int msb = 30;
   while ((nValue >> msb--) == 0);

   while (msb >= 0) {
      int bit = (nValue >> msb) & 1;

      nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, bit ? 1 : 0, nCurBitsOffset, nCurBitMask);
      msb--;
      nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, msb >= 0 ? 1 : 0, nCurBitsOffset, nCurBitMask);
   }

   return nOutOffset;
}

/**
 * Get the number of extra bits required to represent a literals length
 *
 * @param nLength literals length
 *
 * @return number of extra bits required
 */
static inline int apultra_get_literals_varlen_size(const int nLength) {
   return nLength;
}

/**
 * Get the number of extra bits required to represent a match offset
 *
 * @param nLength match length
 * @param nMatchOffset match offset
 * @param nFollowsLiteral non-zero if the match follows a literal, zero if it immediately follows another match
 *
 * @return number of extra bits required
 */
static inline int apultra_get_offset_varlen_size(const int nLength, const int nMatchOffset, const int nFollowsLiteral) {
   if (nLength == 1 && nMatchOffset < 16)
      return 4 + TOKEN_SIZE_4BIT_MATCH;
   else {
      if (nLength <= 3 && nMatchOffset < 128)
         return 8 + TOKEN_SIZE_7BIT_MATCH;
      else {
         if (nFollowsLiteral)
            return 8 + TOKEN_SIZE_LARGE_MATCH + apultra_get_gamma2_size((nMatchOffset >> 8) + 3);
         else
            return 8 + TOKEN_SIZE_LARGE_MATCH + apultra_get_gamma2_size((nMatchOffset >> 8) + 2);
      }
   }
}

/**
 * Get the number of extra bits required to represent a rep-match
 *
 * @return number of extra bits required
 */
static inline int apultra_get_rep_offset_varlen_size(void) {
   return TOKEN_SIZE_LARGE_MATCH + 2 /* apultra_get_gamma2_size(2) */;
}

/**
 * Get the number of extra bits required to represent a match length
 *
 * @param nLength match length
 * @param nMatchOffset match offset
 * @param nIsRepMatch non-zero if this is a rep-match, zero if it is a regular match
 *
 * @return number of extra bits required
 */
static inline int apultra_get_match_varlen_size(int nLength, const int nMatchOffset, const int nIsRepMatch) {
   if (nLength == 1 && nMatchOffset < 16)
      return 0;
   else {
      if (nLength <= 3 && nMatchOffset < 128 && !nIsRepMatch)
         return 0;
      else {
         if (nMatchOffset < 128 && !nIsRepMatch)
            nLength -= 2;
         if (nMatchOffset < MINMATCH3_OFFSET || nIsRepMatch)
            return apultra_get_gamma2_size(nLength);
         else if (nMatchOffset < MINMATCH4_OFFSET)
            return apultra_get_gamma2_size(nLength - 1);
         else
            return apultra_get_gamma2_size(nLength - 2);
      }
   }
}

/**
 * Write extra encoded match length bytes to output (compressed) buffer. The caller must first check that there is enough
 * room to write the bytes.
 *
 * @param pOutData pointer to output buffer
 * @param nOutOffset current write index into output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 * @param nLength match length
 * @param nMatchOffset match offset
 * @param nIsRepMatch non-zero if this is a rep-match, zero if it is a regular match
 *
 * @return updated write index into output buffer, or -1 in case of an error
 */
static inline int apultra_write_match_varlen(unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurBitsOffset, int *nCurBitMask, int nLength, const int nMatchOffset, const int nIsRepMatch) {
   if (nLength < 2)
      return -1;

   if (nMatchOffset < 128 && !nIsRepMatch)
      nLength -= 2;
   if (nMatchOffset < MINMATCH3_OFFSET || nIsRepMatch)
      return apultra_write_gamma2_value(pOutData, nOutOffset, nMaxOutDataSize, nLength, nCurBitsOffset, nCurBitMask);
   else if (nMatchOffset < MINMATCH4_OFFSET)
      return apultra_write_gamma2_value(pOutData, nOutOffset, nMaxOutDataSize, nLength - 1, nCurBitsOffset, nCurBitMask);
   else
      return apultra_write_gamma2_value(pOutData, nOutOffset, nMaxOutDataSize, nLength - 2, nCurBitsOffset, nCurBitMask);
}

/**
 * Insert forward rep candidate
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param i input data window position whose matches are being considered
 * @param nMatchOffset match offset to use as rep candidate
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 */
static void apultra_insert_forward_match(apultra_compressor *pCompressor, const unsigned char *pInWindow, const int i, const int nMatchOffset, const int nEndOffset, int nDepth) {
   apultra_arrival *arrival = pCompressor->arrival;
   int j;

   if (nDepth >= 10) return;

   for (j = 0; j < NMATCHES_PER_ARRIVAL && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].from_slot; j++) {
      int nRepOffset = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset;

      if (nMatchOffset != nRepOffset && nRepOffset && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_len >= MIN_MATCH_SIZE) {
         int nRepPos = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_pos;
         int nRepLen = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_len;

         if (arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].follows_literal &&
            nRepPos &&
            nRepPos > nMatchOffset &&
            (nRepPos - nMatchOffset + nRepLen) <= nEndOffset) {
            int nCurRepLen = 0;

            while (nCurRepLen < nRepLen && pInWindow[nRepPos - nRepOffset + nCurRepLen] == pInWindow[nRepPos - nMatchOffset + nCurRepLen])
               nCurRepLen++;

            if (nCurRepLen >= 2) {
               apultra_match *fwd_match = pCompressor->match + (nRepPos << MATCHES_PER_INDEX_SHIFT);
               int exists = 0;
               int r;

               for (r = 0; r < NMATCHES_PER_INDEX && fwd_match[r].length >= MIN_MATCH_SIZE; r++) {
                  if (fwd_match[r].offset == nMatchOffset) {
                     exists = 1;

                     if ((int)fwd_match[r].length < nCurRepLen) {
                        fwd_match[r].length = nCurRepLen;
                        apultra_insert_forward_match(pCompressor, pInWindow, nRepPos, nMatchOffset, nEndOffset, nDepth + 1);
                     }
                     break;
                  }
               }

               if (!exists && r < NMATCHES_PER_INDEX) {
                  fwd_match[r].offset = nMatchOffset;
                  fwd_match[r].length = nCurRepLen;

                  apultra_insert_forward_match(pCompressor, pInWindow, nRepPos, nMatchOffset, nEndOffset, nDepth + 1);
               }
            }
         }
      }
   }
}

/**
 * Attempt to pick optimal matches, so as to produce the smallest possible output that decompresses to the same input
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param nInsertForwardReps non-zero to insert forward repmatch candidates, zero to use the previously inserted candidates
 * @param nCurRepMatchOffset starting rep offset for this block
 * @param nBlockFlags bit 0: 1 for first block, 0 otherwise; bit 1: 1 for last block, 0 otherwise
 */
static void apultra_optimize_forward(apultra_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, const int nInsertForwardReps, const int *nCurRepMatchOffset, const int nBlockFlags) {
   apultra_arrival *arrival = pCompressor->arrival;
   int i, j, n;

   memset(arrival + (nStartOffset << MATCHES_PER_ARRIVAL_SHIFT), 0, sizeof(apultra_arrival) * ((nEndOffset - nStartOffset + 1) << MATCHES_PER_ARRIVAL_SHIFT));

   arrival[nStartOffset << MATCHES_PER_ARRIVAL_SHIFT].from_slot = -1;
   arrival[nStartOffset << MATCHES_PER_ARRIVAL_SHIFT].rep_offset = *nCurRepMatchOffset;

   for (i = (nStartOffset << MATCHES_PER_ARRIVAL_SHIFT); i != ((nEndOffset+1) << MATCHES_PER_ARRIVAL_SHIFT); i++) {
      arrival[i].cost = 0x40000000;
   }

   for (i = nStartOffset; i != nEndOffset; i++) {
      int m;
      
      if (pInWindow[i] != 0 || (i == nStartOffset && (nBlockFlags & 1))) {
         for (j = 0; j < NMATCHES_PER_ARRIVAL && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].from_slot; j++) {
            int nPrevCost = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].cost & 0x3fffffff;
            int nCodingChoiceCost = nPrevCost + 8 /* literal */;

            nCodingChoiceCost ++ /* Literal bit */;

            apultra_arrival *pDestSlots = &arrival[(i + 1) << MATCHES_PER_ARRIVAL_SHIFT];
            if (nCodingChoiceCost <= pDestSlots[NMATCHES_PER_ARRIVAL - 1].cost) {
               int nScore = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].score + 1;
               int exists = 0;

               for (n = 0;
                  n < NMATCHES_PER_ARRIVAL && pDestSlots[n].cost <= nCodingChoiceCost;
                  n++) {
                  if (pDestSlots[n].rep_offset == arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset) {
                     exists = 1;
                     break;
                  }
               }

               if (!exists) {
                  for (n = 0; n < NMATCHES_PER_ARRIVAL; n++) {
                     apultra_arrival *pDestArrival = &pDestSlots[n];
                     if (nCodingChoiceCost < pDestArrival->cost ||
                        (nCodingChoiceCost == pDestArrival->cost && nScore < pDestArrival->score)) {
                        int z;

                        for (z = n; z < NMATCHES_PER_ARRIVAL - 1; z++) {
                           if (pDestSlots[z].rep_offset == arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset)
                              break;
                        }

                        memmove(&pDestSlots[n + 1],
                           &pDestSlots[n],
                           sizeof(apultra_arrival) * (z - n));

                        pDestArrival->cost = nCodingChoiceCost;
                        pDestArrival->from_pos = i;
                        pDestArrival->from_slot = j + 1;
                        pDestArrival->follows_literal = 1;
                        pDestArrival->match_offset = 0;
                        pDestArrival->match_len = 0;
                        pDestArrival->score = nScore;
                        pDestArrival->rep_offset = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset;
                        pDestArrival->rep_pos = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_pos;
                        pDestArrival->rep_len = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_len;
                        break;
                     }
                  }
               }
            }
         }
      }
      else {
         for (j = 0; j < NMATCHES_PER_ARRIVAL && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].from_slot; j++) {
            int nPrevCost = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].cost & 0x3fffffff;
            int nCodingChoiceCost = nPrevCost + TOKEN_PREFIX_SIZE /* token */ /* the actual cost of the literals themselves accumulates up the chain */ + (4 + TOKEN_SIZE_4BIT_MATCH) /* command and offset cost; no length cost */;

            apultra_arrival *pDestSlots = &arrival[(i + 1) << MATCHES_PER_ARRIVAL_SHIFT];
            if (nCodingChoiceCost <= pDestSlots[NMATCHES_PER_ARRIVAL - 1].cost) {
               int nScore = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].score + 1;
               int exists = 0;

               for (n = 0;
                  n < NMATCHES_PER_ARRIVAL && pDestSlots[n].cost <= nCodingChoiceCost;
                  n++) {
                  if (pDestSlots[n].rep_offset == arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset) {
                     exists = 1;
                     break;
                  }
               }

               if (!exists) {
                  for (n = 0; n < NMATCHES_PER_ARRIVAL; n++) {
                     apultra_arrival *pDestArrival = &pDestSlots[n];

                     if (nCodingChoiceCost < pDestArrival->cost ||
                        (nCodingChoiceCost == pDestArrival->cost && nScore < pDestArrival->score)) {
                        int z;

                        for (z = n; z < NMATCHES_PER_ARRIVAL - 1; z++) {
                           if (pDestSlots[z].rep_offset == arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset)
                              break;
                        }

                        memmove(&pDestSlots[n + 1],
                           &pDestSlots[n],
                           sizeof(apultra_arrival) * (z - n));

                        pDestArrival->cost = nCodingChoiceCost;
                        pDestArrival->from_pos = i;
                        pDestArrival->from_slot = j + 1;
                        pDestArrival->match_offset = 0;
                        pDestArrival->match_len = 1;
                        pDestArrival->follows_literal = 1;
                        pDestArrival->score = nScore;
                        pDestArrival->rep_offset = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset;
                        pDestArrival->rep_pos = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_pos;
                        pDestArrival->rep_len = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_len;
                        break;
                     }
                  }
               }
            }
         }
      }

      if (i == nStartOffset && (nBlockFlags & 1)) continue;

      apultra_match *match = pCompressor->match + (i << MATCHES_PER_INDEX_SHIFT);
      const int nRepMatchOffsetCost = apultra_get_rep_offset_varlen_size();

      for (m = 0; m < NMATCHES_PER_INDEX && match[m].length; m++) {
         int nCurMinMatchSize = MIN_MATCH_SIZE;
         int nMatchLen = match[m].length;
         int nStartingMatchLen, k;
         int nMaxRepLen[NMATCHES_PER_ARRIVAL];

         if (match[m].offset >= 16)
            nCurMinMatchSize = 2;

         if (match[m].offset < 16 && i >= (int)match[m].offset && pInWindow[i - match[m].offset] == 0 && nCurMinMatchSize < 2) {
            /* This will already be covered by a 0-offset match */
            nCurMinMatchSize = 2;
         }

         if ((i + nMatchLen) > nEndOffset)
            nMatchLen = nEndOffset - i;

         for (j = 0; j < NMATCHES_PER_ARRIVAL && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].from_slot; j++) {
            int nRepOffset = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset;
            int nCurMaxLen = 0;

            if (arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].follows_literal &&
               match[m].offset != nRepOffset &&
               nRepOffset &&
               i > nRepOffset &&
               (i - nRepOffset + nMatchLen) <= nEndOffset) {
               
               while (nCurMaxLen < nMatchLen && pInWindow[i - nRepOffset + nCurMaxLen] == pInWindow[i - match[m].offset + nCurMaxLen])
                  nCurMaxLen++;
            }

            nMaxRepLen[j] = nCurMaxLen;
         }
         while (j < NMATCHES_PER_ARRIVAL)
            nMaxRepLen[j++] = 0;

         if (nInsertForwardReps)
            apultra_insert_forward_match(pCompressor, pInWindow, i, match[m].offset, nEndOffset, 0);

         if (nMatchLen >= LEAVE_ALONE_MATCH_SIZE && i >= nMatchLen)
            nStartingMatchLen = nMatchLen;
         else
            nStartingMatchLen = nCurMinMatchSize;

         if (nStartingMatchLen == 1) {
            apultra_arrival *pDestSlots = &arrival[(i + 1) << MATCHES_PER_ARRIVAL_SHIFT];

            for (j = 0; j < NMATCHES_PER_ARRIVAL && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].from_slot; j++) {
               int nRepOffset = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset;

               int nScore = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].score + 3;

               int nPrevCost = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].cost & 0x3fffffff;
               int nCodingChoiceCost = nPrevCost + TOKEN_PREFIX_SIZE /* token */ /* the actual cost of the literals themselves accumulates up the chain */ + (4 + TOKEN_SIZE_4BIT_MATCH) /* command and offset cost; no length cost */;

               if (nCodingChoiceCost <= pDestSlots[NMATCHES_PER_ARRIVAL - 1].cost) {
                  int exists = 0;

                  for (n = 0;
                     n < NMATCHES_PER_ARRIVAL && pDestSlots[n].cost <= nCodingChoiceCost;
                     n++) {
                     if (pDestSlots[n].rep_offset == nRepOffset) {
                        exists = 1;
                        break;
                     }
                  }

                  if (!exists) {
                     for (n = 0; n < NMATCHES_PER_ARRIVAL; n++) {
                        apultra_arrival *pDestArrival = &pDestSlots[n];

                        if (nCodingChoiceCost < pDestArrival->cost ||
                           (nCodingChoiceCost == pDestArrival->cost && nScore < pDestArrival->score)) {
                           int z;

                           for (z = n; z < NMATCHES_PER_ARRIVAL - 1; z++) {
                              if (pDestSlots[z].rep_offset == nRepOffset)
                                 break;
                           }

                           memmove(&pDestSlots[n + 1],
                              &pDestSlots[n],
                              sizeof(apultra_arrival) * (z - n ));

                           pDestArrival->cost = nCodingChoiceCost;
                           pDestArrival->from_pos = i;
                           pDestArrival->from_slot = j + 1;
                           pDestArrival->match_offset = match[m].offset;
                           pDestArrival->match_len = 1;
                           pDestArrival->follows_literal = 1;
                           pDestArrival->score = nScore;
                           pDestArrival->rep_offset = nRepOffset;
                           pDestArrival->rep_pos = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_pos;
                           pDestArrival->rep_len = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_len;
                           break;
                        }
                     }
                  }
               }
            }

            nStartingMatchLen++;
         }

         if (nStartingMatchLen <= nMatchLen) {
            int nNoRepMatchOffsetCostNoLit, nNoRepMatchOffsetCostFollowsLit;

            if (nStartingMatchLen <= 3) {
               nNoRepMatchOffsetCostNoLit = apultra_get_offset_varlen_size(2, match[m].offset, 0);
               nNoRepMatchOffsetCostFollowsLit = apultra_get_offset_varlen_size(2, match[m].offset, 1);
            }
            else {
               nNoRepMatchOffsetCostNoLit = apultra_get_offset_varlen_size(4, match[m].offset, 0);
               nNoRepMatchOffsetCostFollowsLit = apultra_get_offset_varlen_size(4, match[m].offset, 1);
            }

            for (k = nStartingMatchLen; k <= nMatchLen; k++) {
               int nNoRepMatchMatchLenCost = apultra_get_match_varlen_size(k, match[m].offset, 0);
               int nRepMatchMatchLenCost = apultra_get_match_varlen_size(k, match[m].offset, 1);
               apultra_arrival *pDestSlots = &arrival[(i + k) << MATCHES_PER_ARRIVAL_SHIFT];

               for (j = 0; j < NMATCHES_PER_ARRIVAL && arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].from_slot; j++) {
                  int nRepOffset = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].rep_offset;
                  int nIsRepMatch = (match[m].offset == nRepOffset &&
                     arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].follows_literal) ? 1 : 0;
                  int nCannotEncode = 0;

                  int nMatchOffsetCost, nMatchLenCost, nScore;
                  if (nIsRepMatch == 0) {
                     if (match[m].offset >= MINMATCH3_OFFSET && k < 3)
                        nCannotEncode = 1;
                     if (match[m].offset >= MINMATCH4_OFFSET && k < 4)
                        nCannotEncode = 1;

                     nMatchOffsetCost = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].follows_literal ?
                        nNoRepMatchOffsetCostFollowsLit : nNoRepMatchOffsetCostNoLit;
                     nMatchLenCost = nNoRepMatchMatchLenCost;
                     nScore = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].score + 3;
                  }
                  else {
                     nMatchOffsetCost = nRepMatchOffsetCost;
                     nMatchLenCost = nRepMatchMatchLenCost;
                     nScore = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].score + 2;
                  }

                  int nPrevCost = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].cost & 0x3fffffff;
                  int nCodingChoiceCost = nPrevCost + TOKEN_PREFIX_SIZE /* token */ /* the actual cost of the literals themselves accumulates up the chain */ + nMatchOffsetCost + nMatchLenCost;

                  if (!nCannotEncode && nCodingChoiceCost <= pDestSlots[NMATCHES_PER_ARRIVAL - 1].cost) {
                     int exists = 0;
                     const unsigned int nDestMatchOffset = match[m].offset;

                     for (n = 0;
                        n < NMATCHES_PER_ARRIVAL && pDestSlots[n].cost <= nCodingChoiceCost;
                        n++) {
                        if (pDestSlots[n].rep_offset == nDestMatchOffset) {
                           exists = 1;
                           break;
                        }
                     }

                     if (!exists) {
                        for (n = 0; n < NMATCHES_PER_ARRIVAL; n++) {
                           apultra_arrival *pDestArrival = &pDestSlots[n];

                           if (nCodingChoiceCost < pDestArrival->cost ||
                              (nCodingChoiceCost == pDestArrival->cost && nScore < pDestArrival->score)) {
                              int z;
                              
                              for (z = n; z < NMATCHES_PER_ARRIVAL - 1; z++) {
                                 if (pDestSlots[z].rep_offset == nDestMatchOffset)
                                    break;
                              }

                              memmove(&pDestSlots[n + 1],
                                 &pDestSlots[n],
                                 sizeof(apultra_arrival) * (z - n));

                              pDestArrival->cost = nCodingChoiceCost;
                              pDestArrival->from_pos = i;
                              pDestArrival->from_slot = j + 1;
                              pDestArrival->match_offset = match[m].offset;
                              pDestArrival->match_len = k;
                              pDestArrival->follows_literal = 0;
                              pDestArrival->score = nScore;
                              pDestArrival->rep_offset = nDestMatchOffset;
                              pDestArrival->rep_pos = i;
                              pDestArrival->rep_len = k;
                              break;
                           }
                        }
                     }
                  }

                  /* If this coding choice doesn't rep-match, see if we still get a match by using the current repmatch offset for this arrival. This can occur (and not have the
                   * matchfinder offer the offset in the first place, or have too many choices with the same cost to retain the repmatchable offset) when compressing regions
                   * of identical bytes, for instance. Checking for this provides a big compression win on some files. */

                  if (nMaxRepLen[j] >= k) {
                     /* A match is possible at the rep offset; insert the extra coding choice. */

                     nMatchLenCost = apultra_get_match_varlen_size(k, nRepOffset, 1);
                     nCodingChoiceCost = nPrevCost + TOKEN_PREFIX_SIZE /* token */ /* the actual cost of the literals themselves accumulates up the chain */ + nRepMatchOffsetCost + nMatchLenCost;
                     nScore = arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + j].score + 2;

                     if (nCodingChoiceCost <= pDestSlots[NMATCHES_PER_ARRIVAL - 1].cost) {
                        int exists = 0;

                        for (n = 0;
                           n < NMATCHES_PER_ARRIVAL && pDestSlots[n].cost <= nCodingChoiceCost;
                           n++) {
                           if (pDestSlots[n].rep_offset == nRepOffset) {
                              exists = 1;
                              break;
                           }
                        }

                        if (!exists) {
                           for (n = 0; n < NMATCHES_PER_ARRIVAL; n++) {
                              apultra_arrival *pDestArrival = &pDestSlots[n];

                              if (nCodingChoiceCost < pDestArrival->cost ||
                                 (nCodingChoiceCost == pDestArrival->cost && nScore < pDestArrival->score)) {
                                 int z;

                                 for (z = n; z < NMATCHES_PER_ARRIVAL - 1; z++) {
                                    if (pDestSlots[z].rep_offset == nRepOffset)
                                       break;
                                 }

                                 memmove(&pDestSlots[n + 1],
                                    &pDestSlots[n],
                                    sizeof(apultra_arrival) * (z - n));

                                 pDestArrival->cost = nCodingChoiceCost;
                                 pDestArrival->from_pos = i;
                                 pDestArrival->from_slot = j + 1;
                                 pDestArrival->match_offset = nRepOffset;
                                 pDestArrival->match_len = k;
                                 pDestArrival->follows_literal = 0;
                                 pDestArrival->score = nScore;
                                 pDestArrival->rep_offset = nRepOffset;
                                 pDestArrival->rep_pos = i;
                                 pDestArrival->rep_len = k;
                                 break;
                              }
                           }
                        }
                     }
                  }
               }

               if (k == 3) {
                  nNoRepMatchOffsetCostNoLit = apultra_get_offset_varlen_size(4, match[m].offset, 0);
                  nNoRepMatchOffsetCostFollowsLit = apultra_get_offset_varlen_size(4, match[m].offset, 1);
               }
            }
         }
      }
   }
   
   apultra_arrival *end_arrival = &arrival[(i << MATCHES_PER_ARRIVAL_SHIFT) + 0];
   
   int nEndCost = end_arrival->cost;
   
   while (end_arrival->from_slot > 0 && end_arrival->from_pos >= 0 && (int)end_arrival->from_pos < nEndOffset) {
      pCompressor->best_match[end_arrival->from_pos].length = end_arrival->match_len;
      pCompressor->best_match[end_arrival->from_pos].offset = end_arrival->match_offset;
      
      end_arrival = &arrival[(end_arrival->from_pos << MATCHES_PER_ARRIVAL_SHIFT) + (end_arrival->from_slot-1)];
   }
}

/**
 * Attempt to replace matches by literals when it makes the final bitstream smaller, and merge large matches
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param pBestMatch optimal matches to evaluate and update
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param nCurRepMatchOffset starting rep offset for this block
 *
 * @return non-zero if the number of tokens was reduced, 0 if it wasn't
 */
static int apultra_reduce_commands(apultra_compressor *pCompressor, const unsigned char *pInWindow, apultra_final_match *pBestMatch, const int nStartOffset, const int nEndOffset, const int *nCurRepMatchOffset) {
   int i;
   int nNumLiterals = 0;
   int nRepMatchOffset = *nCurRepMatchOffset;
   int nFollowsLiteral = 0;
   int nDidReduce = 0;

   for (i = nStartOffset; i < nEndOffset; ) {
      apultra_final_match *pMatch = pBestMatch + i;

      if (pMatch->length >= MIN_MATCH_SIZE) {
         if (pMatch->length < 8 && /* Don't waste time considering large matches, they will always win over literals */
             (i + pMatch->length) < nEndOffset /* Don't consider the last match in the block, we can only reduce a match inbetween other tokens */) {
            int nNextIndex = i + pMatch->length;
            int nNextLiterals = 0;
            int nNextFollowsLiteral = (pMatch->length >= 2) ? 0 : 1;
            int nCannotEncode = 0;

            while (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length < 2) {
               nNextLiterals++;
               nNextIndex++;
               nNextFollowsLiteral = 1;
            }

            if (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length >= 2) {
               /* This command is a match, is followed by 'nNextLiterals' literals and then by another match. Calculate this command's current cost (excluding 'nNumLiterals' bytes) */

               int nCurCommandSize = TOKEN_PREFIX_SIZE /* token */ + apultra_get_literals_varlen_size(nNumLiterals);
               if (pMatch->offset == nRepMatchOffset && nFollowsLiteral && pMatch->length >= 2) {
                  nCurCommandSize += apultra_get_rep_offset_varlen_size() + apultra_get_match_varlen_size(pMatch->length, pMatch->offset, 1);
               }
               else {
                  nCurCommandSize += apultra_get_offset_varlen_size(pMatch->length, pMatch->offset, nFollowsLiteral) + apultra_get_match_varlen_size(pMatch->length, pMatch->offset, 0);
               }

               /* Calculate the next command's current cost */
               int nNextCommandSize = TOKEN_PREFIX_SIZE /* token */ + apultra_get_literals_varlen_size(nNextLiterals) + (nNextLiterals << 3);
               int nCurRepOffset = (pMatch->length >= 2) ? pMatch->offset : nRepMatchOffset;
               if (pBestMatch[nNextIndex].offset == nCurRepOffset && nNextFollowsLiteral && pBestMatch[nNextIndex].length >= 2) {
                  nNextCommandSize += apultra_get_rep_offset_varlen_size() + apultra_get_match_varlen_size(pBestMatch[nNextIndex].length, pBestMatch[nNextIndex].offset, 1);
               }
               else {
                  nNextCommandSize += apultra_get_offset_varlen_size(pBestMatch[nNextIndex].length, pBestMatch[nNextIndex].offset, nNextFollowsLiteral) + apultra_get_match_varlen_size(pBestMatch[nNextIndex].length, pBestMatch[nNextIndex].offset, 0);
               }

               int nOriginalCombinedCommandSize = nCurCommandSize + nNextCommandSize;

               /* Calculate the cost of replacing this match command by literals + the next command with the cost of encoding these literals (excluding 'nNumLiterals' bytes) */
               int nReducedFollowsLiteral = (nNumLiterals + pMatch->length) ? 1 : 0;
               int nReducedCommandSize = TOKEN_PREFIX_SIZE /* token */ + apultra_get_literals_varlen_size(nNumLiterals + pMatch->length + nNextLiterals) + (nNextLiterals << 3);

               for (int j = 0; j < pMatch->length; j++) {
                  if (pInWindow[i + j] == 0)
                     nReducedCommandSize += TOKEN_PREFIX_SIZE + TOKEN_SIZE_4BIT_MATCH + 4;
                  else
                     nReducedCommandSize += 8;
               }

               if (pBestMatch[nNextIndex].offset == nRepMatchOffset && nReducedFollowsLiteral && pBestMatch[nNextIndex].length >= 2) {
                  nReducedCommandSize += apultra_get_rep_offset_varlen_size() + apultra_get_match_varlen_size(pBestMatch[nNextIndex].length, pBestMatch[nNextIndex].offset, 1);
               }
               else {
                  if ((pBestMatch[nNextIndex].length < 3 && pBestMatch[nNextIndex].offset >= MINMATCH3_OFFSET) ||
                     (pBestMatch[nNextIndex].length < 4 && pBestMatch[nNextIndex].offset >= MINMATCH4_OFFSET)) {
                     /* This match length can only be encoded with a rep-match */
                     nCannotEncode = 1;
                  }
                  else {
                     nReducedCommandSize += apultra_get_offset_varlen_size(pBestMatch[nNextIndex].length, pBestMatch[nNextIndex].offset, nReducedFollowsLiteral) + apultra_get_match_varlen_size(pBestMatch[nNextIndex].length, pBestMatch[nNextIndex].offset, 0);
                  }
               }

               if (!nCannotEncode && nOriginalCombinedCommandSize > nReducedCommandSize) {
                  /* Reduce */
                  int nMatchLen = pMatch->length;
                  int j;

                  for (j = 0; j < nMatchLen; j++) {
                     pBestMatch[i + j].offset = 0;
                     pBestMatch[i + j].length = pInWindow[i + j] ? 0 : 1;
                  }

                  nDidReduce = 1;
                  continue;
               }
            }
         }

         if ((i + pMatch->length) < nEndOffset && pMatch->offset > 0 && pMatch->length >= LEAVE_ALONE_MATCH_SIZE &&
            pBestMatch[i + pMatch->length].offset > 0 &&
            pBestMatch[i + pMatch->length].length >= 2 &&
            (pMatch->length + pBestMatch[i + pMatch->length].length) <= MAX_VARLEN &&
            (i + pMatch->length) > pMatch->offset &&
            (i + pMatch->length) > pBestMatch[i + pMatch->length].offset &&
            (i + pMatch->length + pBestMatch[i + pMatch->length].length) <= nEndOffset &&
            !memcmp(pInWindow + i + pMatch->length - pMatch->offset,
               pInWindow + i + pMatch->length - pBestMatch[i + pMatch->length].offset,
               pBestMatch[i + pMatch->length].length)) {
            int nMatchLen = pMatch->length;

            /* Join large matches */

            int nNextIndex = i + pMatch->length + pBestMatch[i + pMatch->length].length;
            int nCannotEncode = 0;

            while (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length < 2) {
               nNextIndex++;
            }

            if (nNextIndex < nEndOffset && pBestMatch[nNextIndex].length >= 2 &&
               pBestMatch[nNextIndex].offset == pBestMatch[i + pMatch->length].offset) {
               if ((pBestMatch[nNextIndex].offset >= MINMATCH3_OFFSET && pBestMatch[nNextIndex].length < 3) ||
                  (pBestMatch[nNextIndex].offset >= MINMATCH4_OFFSET && pBestMatch[nNextIndex].length < 4)) {
                  nCannotEncode = 1;
               }
            }

            if (!nCannotEncode) {
               pMatch->length += pBestMatch[i + nMatchLen].length;
               pBestMatch[i + nMatchLen].offset = 0;
               pBestMatch[i + nMatchLen].length = -1;
               nDidReduce = 1;
               continue;
            }
         }

         if (pMatch->offset == nRepMatchOffset && nFollowsLiteral && pMatch->length >= 2) {
            /* Rep-match */
            nRepMatchOffset = pMatch->offset;
            nFollowsLiteral = 0;
         }
         else {
            if (pMatch->length == 1 && pMatch->offset < 16) {
               /* 4 bits offset */
               nFollowsLiteral = 1;
            }
            else if (pMatch->length <= 3 && pMatch->offset < 128) {
               /* 7 bits offset + 1 bit length */
               nRepMatchOffset = pMatch->offset;
               nFollowsLiteral = 0;
            }
            else {
               /* 8+n bits offset */
               nRepMatchOffset = pMatch->offset;
               nFollowsLiteral = 0;
            }
         }

         i += pMatch->length;
         nNumLiterals = 0;
      }
      else {
         nNumLiterals++;
         i++;
         nFollowsLiteral = 1;
      }
   }

   return nDidReduce;
}

/**
 * Emit a block of compressed data
 *
 * @param pCompressor compression context
 * @param pBestMatch optimal matches to emit
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 * @param nFollowsLiteral non-zero if the next command to be issued follows a literal, 0 if not
 * @param nCurRepMatchOffset starting rep offset for this block, updated after the block is compressed successfully
 * @param nBlockFlags bit 0: 1 for first block, 0 otherwise; bit 1: 1 for last block, 0 otherwise
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int apultra_write_block(apultra_compressor *pCompressor, apultra_final_match *pBestMatch, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, int *nCurBitsOffset, int *nCurBitMask, int *nFollowsLiteral, int *nCurRepMatchOffset, const int nBlockFlags) {
   int i, j;
   int nInFirstLiteralOffset = 0;
   int nRepMatchOffset = *nCurRepMatchOffset;

   if (nBlockFlags & 1) {
      if (nOutOffset < 0 || nOutOffset >= nMaxOutDataSize)
         return -1;
      pOutData[nOutOffset++] = pInWindow[nStartOffset];
      *nFollowsLiteral = 1;
   }

   for (i = nStartOffset + ((nBlockFlags & 1) ? 1 : 0); i < nEndOffset; ) {
      const apultra_final_match *pMatch = pBestMatch + i;

      if (pMatch->length >= MIN_MATCH_SIZE) {
         int nMatchOffset = pMatch->offset;
         int nMatchLen = pMatch->length;
         int nTokenOffsetMode;
         int nOffsetSize;

         if (nMatchOffset == nRepMatchOffset && *nFollowsLiteral && nMatchLen >= 2) {
            /* Rep-match */
            nTokenOffsetMode = 3;
            nOffsetSize = TOKEN_SIZE_LARGE_MATCH + apultra_get_gamma2_size(2);
         }
         else {
            if (nMatchLen == 1 && nMatchOffset < 16) {
               /* 4 bits offset */
               nTokenOffsetMode = 2;
               nOffsetSize = 4 + TOKEN_SIZE_4BIT_MATCH;
            }
            else if (nMatchLen <= 3 && nMatchOffset < 128) {
               /* 7 bits offset + 1 bit length */
               nTokenOffsetMode = 1;
               nOffsetSize = 8 + TOKEN_SIZE_7BIT_MATCH;
            }
            else {
               /* 8+n bits offset */
               nTokenOffsetMode = 0;
               if (*nFollowsLiteral)
                  nOffsetSize = 8 + TOKEN_SIZE_LARGE_MATCH + apultra_get_gamma2_size((nMatchOffset >> 8) + 3);
               else
                  nOffsetSize = 8 + TOKEN_SIZE_LARGE_MATCH + apultra_get_gamma2_size((nMatchOffset >> 8) + 2);
            }
         }

         int nCommandSize = TOKEN_PREFIX_SIZE /* token */ + nOffsetSize /* match offset */ + apultra_get_match_varlen_size(nMatchLen, nMatchOffset, (nTokenOffsetMode == 3) ? 1 : 0);

         if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
            return -1;
         if (nMatchOffset < ((nMatchLen == 1) ? 0 : MIN_OFFSET) || nMatchOffset > MAX_OFFSET)
            return -1;

         int nActualTokenOffsetMode = nTokenOffsetMode;
         if (nActualTokenOffsetMode == 3)
            nActualTokenOffsetMode = 0;
         nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, 1 /* match */, nCurBitsOffset, nCurBitMask);
         for (j = _token_size[nActualTokenOffsetMode] - 1; j >= 0; j--)
            nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (_token_code[nActualTokenOffsetMode] & (1 << j)) ? 1 : 0, nCurBitsOffset, nCurBitMask);

         int nEmitMatchLength = 0;

         if (nTokenOffsetMode == 0) {
            /* 8+n bits offset */

            if (nOutOffset < 0 || nOutOffset >= nMaxOutDataSize)
               return -1;
            if (*nFollowsLiteral)
               nOutOffset = apultra_write_gamma2_value(pOutData, nOutOffset, nMaxOutDataSize, (nMatchOffset >> 8) + 3, nCurBitsOffset, nCurBitMask);
            else
               nOutOffset = apultra_write_gamma2_value(pOutData, nOutOffset, nMaxOutDataSize, (nMatchOffset >> 8) + 2, nCurBitsOffset, nCurBitMask);
            pOutData[nOutOffset++] = nMatchOffset & 0xff;

            if (nMatchOffset < 128 && nMatchLen <= 3) {
               /* A shorter match must have been encoded as a 4 bits offset or a 7 bits offset + 1 bit match length command */
               return -1;
            }

            nEmitMatchLength = 1;
            *nFollowsLiteral = 0;
            nRepMatchOffset = nMatchOffset;

            pCompressor->stats.num_variable_matches++;
         }
         else if (nTokenOffsetMode == 1) {
            /* 7 bits offset + 1 bit length */

            if (nOutOffset < 0 || nOutOffset >= nMaxOutDataSize)
               return -1;
            pOutData[nOutOffset++] = ((nMatchOffset) & 0x7f) << 1 | (nMatchLen - 2);

            *nFollowsLiteral = 0;
            nRepMatchOffset = nMatchOffset;

            pCompressor->stats.num_7bit_matches++;
         }
         else if (nTokenOffsetMode == 2) {
            /* 4 bits offset */

            nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (nMatchOffset & 0x08) ? 1 : 0, nCurBitsOffset, nCurBitMask);
            nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (nMatchOffset & 0x04) ? 1 : 0, nCurBitsOffset, nCurBitMask);
            nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (nMatchOffset & 0x02) ? 1 : 0, nCurBitsOffset, nCurBitMask);
            nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (nMatchOffset & 0x01) ? 1 : 0, nCurBitsOffset, nCurBitMask);
            if (nOutOffset < 0) return -1;

            *nFollowsLiteral = 1;

            pCompressor->stats.num_4bit_matches++;
         }
         else {
            /* rep match */
            nOutOffset = apultra_write_gamma2_value(pOutData, nOutOffset, nMaxOutDataSize, 2, nCurBitsOffset, nCurBitMask);

            nEmitMatchLength = 1;
            *nFollowsLiteral = 0;

            pCompressor->stats.num_rep_matches++;
         }

         if (nEmitMatchLength) {
            /* The match length isn't encoded in the command, emit elias gamma value */
            nOutOffset = apultra_write_match_varlen(pOutData, nOutOffset, nMaxOutDataSize, nCurBitsOffset, nCurBitMask, nMatchLen, nMatchOffset, (nTokenOffsetMode == 3) ? 1 : 0);
            if (nOutOffset < 0) return -1;
         }

         if (nMatchOffset < pCompressor->stats.min_offset || pCompressor->stats.min_offset == -1)
            pCompressor->stats.min_offset = nMatchOffset;
         if (nMatchOffset > pCompressor->stats.max_offset)
            pCompressor->stats.max_offset = nMatchOffset;
         pCompressor->stats.total_offsets += (long long)nMatchOffset;

         if (nMatchLen < pCompressor->stats.min_match_len || pCompressor->stats.min_match_len == -1)
            pCompressor->stats.min_match_len = nMatchLen;
         if (nMatchLen > pCompressor->stats.max_match_len)
            pCompressor->stats.max_match_len = nMatchLen;
         pCompressor->stats.total_match_lens += nMatchLen;
         pCompressor->stats.match_divisor++;

         if (nMatchOffset == 1) {
            if (nMatchLen < pCompressor->stats.min_rle1_len || pCompressor->stats.min_rle1_len == -1)
               pCompressor->stats.min_rle1_len = nMatchLen;
            if (nMatchLen > pCompressor->stats.max_rle1_len)
               pCompressor->stats.max_rle1_len = nMatchLen;
            pCompressor->stats.total_rle1_lens += nMatchLen;
            pCompressor->stats.rle1_divisor++;
         }
         else if (nMatchOffset == 2) {
            if (nMatchLen < pCompressor->stats.min_rle2_len || pCompressor->stats.min_rle2_len == -1)
               pCompressor->stats.min_rle2_len = nMatchLen;
            if (nMatchLen > pCompressor->stats.max_rle2_len)
               pCompressor->stats.max_rle2_len = nMatchLen;
            pCompressor->stats.total_rle2_lens += nMatchLen;
            pCompressor->stats.rle2_divisor++;
         }

         i += nMatchLen;

         pCompressor->stats.commands_divisor++;
      }
      else {
         nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, 0 /* literal */, nCurBitsOffset, nCurBitMask);

         if (nOutOffset < 0 || nOutOffset >= nMaxOutDataSize)
            return -1;
         pOutData[nOutOffset++] = pInWindow[i];

         pCompressor->stats.num_literals++;
         pCompressor->stats.commands_divisor++;
         i++;
         *nFollowsLiteral = 1;
      }
   }

   if (nBlockFlags & 2) {
      nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, 1 /* match */, nCurBitsOffset, nCurBitMask);

      /* 8 bits offset */

      for (j = TOKEN_SIZE_7BIT_MATCH - 1; j >= 0; j--)
         nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (_token_code[1] & (1 << j)) ? 1 : 0, nCurBitsOffset, nCurBitMask);

      if (nOutOffset < 0 || nOutOffset >= nMaxOutDataSize)
         return -1;
      pOutData[nOutOffset++] = 0x00;   /* Offset: EOD */
      pCompressor->stats.commands_divisor++;
   }

   *nCurRepMatchOffset = nRepMatchOffset;
   return nOutOffset;
}

/**
 * Emit raw block of uncompressible data
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nStartOffset current offset in input window (typically the number of previously compressed bytes)
 * @param nEndOffset offset to end finding matches at (typically the size of the total input window in bytes
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nBlockFlags bit 0: 1 for first block, 0 otherwise; bit 1: 1 for last block, 0 otherwise
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 * @param nFollowsLiteral non-zero if the next command to be issued follows a literal, 0 if not
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int apultra_write_raw_uncompressed_block_v3(apultra_compressor *pCompressor, const unsigned char *pInWindow, const int nStartOffset, const int nEndOffset, unsigned char *pOutData, int nOutOffset, const int nMaxOutDataSize, const int nBlockFlags, int *nCurBitsOffset, int *nCurBitMask, int *nFollowsLiteral) {
   int nNumLiterals = nEndOffset - nStartOffset;
   int j;
   int nInOffset = nStartOffset;

   int nCommandSize = apultra_get_literals_varlen_size(nNumLiterals) + (nNumLiterals << 3) + TOKEN_PREFIX_SIZE + TOKEN_SIZE_7BIT_MATCH /* token */ + 8 /* match offset */;
   if ((nOutOffset + ((nCommandSize + 7) >> 3)) > nMaxOutDataSize)
      return -1;

   pCompressor->stats.commands_divisor = 0;
   *nFollowsLiteral = 1;

   for (j = 0; j < nNumLiterals; j++) {
      nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, 0 /* literal */, nCurBitsOffset, nCurBitMask);
      pOutData[nOutOffset++] = pInWindow[nInOffset + j];
   }

   nNumLiterals = 0;

   nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, 1 /* match */, nCurBitsOffset, nCurBitMask);

   /* 8 bits offset */
   for (j = TOKEN_SIZE_7BIT_MATCH - 1; j >= 0; j--)
      nOutOffset = apultra_write_bit(pOutData, nOutOffset, nMaxOutDataSize, (_token_code[1] & (1 << j)) ? 1 : 0, nCurBitsOffset, nCurBitMask);

   if (nOutOffset < 0 || nOutOffset >= nMaxOutDataSize)
      return -1;
   pOutData[nOutOffset++] = 0x00;   /* Offset: EOD */

   pCompressor->stats.commands_divisor++;

   return nOutOffset;
}

/**
 * Select the most optimal matches, reduce the token count if possible, and then emit a block of compressed data
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nPreviousBlockSize number of previously compressed bytes (or 0 for none)
 * @param nInDataSize number of input bytes to compress
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 * @param nCurFollowsLiteral non-zero if the next command to be issued follows a literal, 0 if not
 * @param nCurRepMatchOffset starting rep offset for this block, updated after the block is compressed successfully
 * @param nBlockFlags bit 0: 1 for first block, 0 otherwise; bit 1: 1 for last block, 0 otherwise
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int apultra_optimize_and_write_block(apultra_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize, int *nCurBitsOffset, int *nCurBitMask, int *nCurFollowsLiteral, int *nCurRepMatchOffset, const int nBlockFlags) {
   int nResult;
   int nOutOffset = 0;

   apultra_optimize_forward(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, 1 /* nInsertForwardReps */, nCurRepMatchOffset, nBlockFlags);

   /* Pick optimal matches */
   apultra_optimize_forward(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, 0 /* nInsertForwardReps */, nCurRepMatchOffset, nBlockFlags);

   /* Apply reduction and merge pass */
   int nDidReduce;
   int nPasses = 0;
   do {
      nDidReduce = apultra_reduce_commands(pCompressor, pInWindow, pCompressor->best_match, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, nCurRepMatchOffset);
      nPasses++;
   } while (nDidReduce && nPasses < 20);

   /* Write compressed block */

   nResult = apultra_write_block(pCompressor, pCompressor->best_match, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nOutOffset, nMaxOutDataSize, nCurBitsOffset, nCurBitMask, nCurFollowsLiteral, nCurRepMatchOffset, nBlockFlags);
   if (nResult < 0) {
      /* Try to write block as all literals */
      *nCurRepMatchOffset = 0;
      nResult = apultra_write_raw_uncompressed_block_v3(pCompressor, pInWindow, nPreviousBlockSize, nPreviousBlockSize + nInDataSize, pOutData, nOutOffset, nMaxOutDataSize, nBlockFlags, nCurBitsOffset, nCurBitMask, nCurFollowsLiteral);
   }

   return nResult;
}

/* Forward declaration */
static void apultra_compressor_destroy(apultra_compressor *pCompressor);

/**
 * Initialize compression context
 *
 * @param pCompressor compression context to initialize
 * @param nMaxWindowSize maximum size of input data window (previously compressed bytes + bytes to compress)
 * @param nFlags compression flags
 *
 * @return 0 for success, non-zero for failure
 */
static int apultra_compressor_init(apultra_compressor *pCompressor, const int nMaxWindowSize, const int nFlags) {
   int nResult;

   nResult = divsufsort_init(&pCompressor->divsufsort_context);
   pCompressor->intervals = NULL;
   pCompressor->pos_data = NULL;
   pCompressor->open_intervals = NULL;
   pCompressor->match = NULL;
   pCompressor->best_match = NULL;
   pCompressor->arrival = NULL;
   pCompressor->flags = nFlags;

   memset(&pCompressor->stats, 0, sizeof(pCompressor->stats));
   pCompressor->stats.min_match_len = -1;
   pCompressor->stats.min_offset = -1;
   pCompressor->stats.min_rle1_len = -1;
   pCompressor->stats.min_rle2_len = -1;

   if (!nResult) {
      pCompressor->intervals = (unsigned int *)malloc(nMaxWindowSize * sizeof(unsigned int));

      if (pCompressor->intervals) {
         pCompressor->pos_data = (unsigned int *)malloc(nMaxWindowSize * sizeof(unsigned int));

         if (pCompressor->pos_data) {
            pCompressor->open_intervals = (unsigned int *)malloc((LCP_MAX + 1) * sizeof(unsigned int));

            if (pCompressor->open_intervals) {
               pCompressor->arrival = (apultra_arrival *)malloc((nMaxWindowSize + 1) * NMATCHES_PER_ARRIVAL * sizeof(apultra_arrival));

               if (pCompressor->arrival) {
                  pCompressor->best_match = (apultra_final_match *)malloc(nMaxWindowSize * sizeof(apultra_final_match));

                  if (pCompressor->best_match) {
                     pCompressor->match = (apultra_match *)malloc(nMaxWindowSize * NMATCHES_PER_INDEX * sizeof(apultra_match));
                     if (pCompressor->match)
                        return 0;
                  }
               }
            }
         }
      }
   }

   apultra_compressor_destroy(pCompressor);
   return 100;
}

/**
 * Clean up compression context and free up any associated resources
 *
 * @param pCompressor compression context to clean up
 */
static void apultra_compressor_destroy(apultra_compressor *pCompressor) {
   divsufsort_destroy(&pCompressor->divsufsort_context);

   if (pCompressor->match) {
      free(pCompressor->match);
      pCompressor->match = NULL;
   }

   if (pCompressor->arrival) {
      free(pCompressor->arrival);
      pCompressor->arrival = NULL;
   }

   if (pCompressor->best_match) {
      free(pCompressor->best_match);
      pCompressor->best_match = NULL;
   }

   if (pCompressor->open_intervals) {
      free(pCompressor->open_intervals);
      pCompressor->open_intervals = NULL;
   }

   if (pCompressor->pos_data) {
      free(pCompressor->pos_data);
      pCompressor->pos_data = NULL;
   }

   if (pCompressor->intervals) {
      free(pCompressor->intervals);
      pCompressor->intervals = NULL;
   }
}

/**
 * Compress one block of data
 *
 * @param pCompressor compression context
 * @param pInWindow pointer to input data window (previously compressed bytes + bytes to compress)
 * @param nPreviousBlockSize number of previously compressed bytes (or 0 for none)
 * @param nInDataSize number of input bytes to compress
 * @param pOutData pointer to output buffer
 * @param nMaxOutDataSize maximum size of output buffer, in bytes
 * @param nCurBitsOffset write index into output buffer, of current byte being filled with bits
 * @param nCurBitMask bit shifter
 * @param nCurFollowsLiteral non-zero if the next command to be issued follows a literal, 0 if not
 * @param nCurRepMatchOffset starting rep offset for this block, updated after the block is compressed successfully
 * @param nBlockFlags bit 0: 1 for first block, 0 otherwise; bit 1: 1 for last block, 0 otherwise
 *
 * @return size of compressed data in output buffer, or -1 if the data is uncompressible
 */
static int apultra_compressor_shrink_block(apultra_compressor *pCompressor, const unsigned char *pInWindow, const int nPreviousBlockSize, const int nInDataSize, unsigned char *pOutData, const int nMaxOutDataSize, int *nCurBitsOffset, int *nCurBitMask, int *nCurFollowsLiteral, int *nCurRepMatchOffset, const int nBlockFlags) {
   int nCompressedSize;

   if (apultra_build_suffix_array(pCompressor, pInWindow, nPreviousBlockSize + nInDataSize))
      nCompressedSize = -1;
   else {
      if (nPreviousBlockSize) {
         apultra_skip_matches(pCompressor, 0, nPreviousBlockSize);
      }
      apultra_find_all_matches(pCompressor, NMATCHES_PER_INDEX, nPreviousBlockSize, nPreviousBlockSize + nInDataSize);

      nCompressedSize = apultra_optimize_and_write_block(pCompressor, pInWindow, nPreviousBlockSize, nInDataSize, pOutData, nMaxOutDataSize, nCurBitsOffset, nCurBitMask, nCurFollowsLiteral, nCurRepMatchOffset, nBlockFlags);
   }

   return nCompressedSize;
}

/**
 * Get maximum compressed size of input(source) data
 *
 * @param nInputSize input(source) size in bytes
 *
 * @return maximum compressed size
 */
size_t apultra_get_max_compressed_size(size_t nInputSize) {
   return ((nInputSize * 9 /* literals + literal bits */ + 1 /* match bit */ + 2 /* 7+1 command bits */ + 8 /* EOD offset bits */) + 7) >> 3;
}

/**
 * Compress memory
 *
 * @param pInputData pointer to input(source) data to compress
 * @param pOutBuffer buffer for compressed data
 * @param nInputSize input(source) size in bytes
 * @param nMaxOutBufferSize maximum capacity of compression buffer
 * @param nFlags compression flags (set to 0)
 * @param progress progress function, called after compressing each block, or NULL for none
 * @param pStats pointer to compression stats that are filled if this function is successful, or NULL
 *
 * @return actual compressed size, or -1 for error
 */
size_t apultra_compress(const unsigned char *pInputData, unsigned char *pOutBuffer, size_t nInputSize, size_t nMaxOutBufferSize,
      const unsigned int nFlags, void(*progress)(long long nOriginalSize, long long nCompressedSize), apultra_stats *pStats) {
   apultra_compressor compressor;
   size_t nOriginalSize = 0;
   size_t nCompressedSize = 0L;
   int nResult;
   int nError = 0;
   int nMaxOutBlockSize = (int)apultra_get_max_compressed_size(BLOCK_SIZE);

   nResult = apultra_compressor_init(&compressor, BLOCK_SIZE * 2, nFlags);
   if (nResult != 0) {
      return -1;
   }

   int nPreviousBlockSize = 0;
   int nNumBlocks = 0;
   int nCurBitsOffset = INT_MIN, nCurBitMask = 0, nCurFollowsLiteral = 0;
   int nBlockFlags = 1;
   int nCurRepMatchOffset = 0;

   while (nOriginalSize < nInputSize && !nError) {
      int nInDataSize;

      nInDataSize = (int)(nInputSize - nOriginalSize);
      if (nInDataSize > BLOCK_SIZE)
         nInDataSize = BLOCK_SIZE;

      if (nInDataSize > 0) {
         int nOutDataSize;
         int nOutDataEnd = (int)(nMaxOutBufferSize - nCompressedSize);

         if (nOutDataEnd > nMaxOutBlockSize)
            nOutDataEnd = nMaxOutBlockSize;

         if ((nOriginalSize + nInDataSize) >= nInputSize)
            nBlockFlags |= 2;
         nOutDataSize = apultra_compressor_shrink_block(&compressor, pInputData + nOriginalSize - nPreviousBlockSize, nPreviousBlockSize, nInDataSize, pOutBuffer + nCompressedSize, nOutDataEnd,
            &nCurBitsOffset, &nCurBitMask, &nCurFollowsLiteral, &nCurRepMatchOffset, nBlockFlags);
         nBlockFlags &= (~1);

         if (nOutDataSize >= 0) {
            /* Write compressed block */

            if (!nError) {
               nOriginalSize += nInDataSize;
               nCompressedSize += nOutDataSize;
               if (nCurBitsOffset != INT_MIN)
                  nCurBitsOffset -= nOutDataSize;
            }
         }
         else {
            nError = -1;
         }

         nPreviousBlockSize = nInDataSize;
         nNumBlocks++;
      }

      if (!nError && nOriginalSize < nInputSize) {
         if (progress)
            progress(nOriginalSize, nCompressedSize);
      }
   }

   if (progress)
      progress(nOriginalSize, nCompressedSize);
   if (pStats)
      *pStats = compressor.stats;

   apultra_compressor_destroy(&compressor);

   if (nError) {
      return -1;
   }
   else {
      return nCompressedSize;
   }
}
