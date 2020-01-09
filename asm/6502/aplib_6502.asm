; ***************************************************************************
; ***************************************************************************
;
; aplib_6502.s
;
; NMOS 6502 decompressor for data stored in Jorgen Ibsen's aPLib format.
;
; Includes support for Emmanuel Marty's enhancements to the aPLib format.
;
; The code is 252 bytes long for standard format, 270 for enhanced format.
;
; This code is written for the ACME assembler.
;
; Copyright John Brandwood 2019.
;
; Distributed under the Boost Software License, Version 1.0.
; (See accompanying file LICENSE_1_0.txt or copy at
;  http://www.boost.org/LICENSE_1_0.txt)
;
; ***************************************************************************
; ***************************************************************************



; ***************************************************************************
; ***************************************************************************
;
; Decompression Options & Macros
;

                ;
                ; Use the enhanced format from Emmanuel Marty's APULTRA?
                ;
                ; The enhancements speed up decompression on an 8-bit CPU.
                ;
                ; This gives an 11% improvement in decompresison speed, but
                ; breaks compatibility with standard aPLib encoders.
                ;

;APL_ENHANCED   =       1

                ;
                ; Assume that we're decompessing from a large multi-bank
                ; compressed data file, and that the next bank may need to
                ; paged in when a page-boundary is crossed.
                ;

;APL_FROM_BANK  =       1

                ;
                ; Macro to increment the source pointer to the next page.
                ;

                !ifdef  APL_FROM_BANK {
                        !macro  APL_INC_PAGE {
                        jsr     .next_page
                        }
                } else {
                        !macro  APL_INC_PAGE {
                        inc     <apl_srcptr + 1
                        }
                }

                ;
                ; Macro to read a byte from the compressed source data.
                ;

                !macro   APL_GET_SRC {
                lda     (apl_srcptr),y
                inc     <apl_srcptr + 0
                bne     .skip
                +APL_INC_PAGE
.skip:
                }



; ***************************************************************************
; ***************************************************************************
;
; Data usage is last 12 bytes of zero-page.
;

                !ifdef  APL_ENHANCED {
apl_nibflg      =       $F4                     ; 1 byte.
apl_nibble      =       $F5                     ; 1 byte.
apl_egamma      =       $F6                     ; 1 byte.
                }

apl_bitbuf      =       $F7                     ; 1 byte.
apl_offset      =       $F8                     ; 1 word.
apl_winptr      =       $FA                     ; 1 word.
apl_srcptr      =       $FC                     ; 1 word.
apl_dstptr      =       $FE                     ; 1 word.
apl_length      =       apl_winptr


; ***************************************************************************
; ***************************************************************************
;
; apl_decompress - Decompress data stored in Jorgen Ibsen's aPLib format.
;
; Args: apl_srcptr = ptr to compessed data
; Args: apl_dstptr = ptr to output buffer
; Uses: lots!
;
; If compiled with APL_FROM_BANK, then apl_srcptr should be within the bank
; window range.
;
; As an optimization, the code to handle window offsets > 64768 bytes has
; been removed, since these don't occur with a 16-bit address range.
;
; As an optimization, the code to handle window offsets > 32000 bytes can
; be commented-out, since these don't occur in typical 8-bit computer usage.
;

apl_decompress: ldy     #0                      ; Initialize source index.

                lda     #$80                    ; Initialize an empty
                sta     <apl_bitbuf             ; bit-buffer.

                !ifdef  APL_ENHANCED {
                sta     <apl_egamma             ; Bit-buffer for gamma pairs.
                sty     <apl_nibflg             ; Reset the flag.
                }

                ;
                ; 0 bbbbbbbb - One byte from compressed data, i.e. a "literal".
                ;

.literal:       +APL_GET_SRC

.write_byte:    ldx     #0                      ; LWM=0.

                sta     (apl_dstptr),y          ; Write the byte directly to
                inc     <apl_dstptr + 0         ; the output.
                bne     .next_tag
                inc     <apl_dstptr + 1

.next_tag:      asl     <apl_bitbuf             ; 0 bbbbbbbb
                bne     .skip0
                jsr     .load_bit
.skip0:         bcc     .literal

.skip1:         asl     <apl_bitbuf             ; 1 0 <offset> <length>
                bne     .skip2
                jsr     .load_bit
.skip2:         bcc     .copy_large

                asl     <apl_bitbuf             ; 1 1 0 dddddddn
                bne     .skip3
                jsr     .load_bit
.skip3:         bcc     .copy_normal

                ; 1 1 1 dddd - Copy 1 byte within 15 bytes (or zero).

                !ifdef  APL_ENHANCED {

.copy_short:    lsr     <apl_nibflg             ; Is there a nibble waiting?
                lda     <apl_nibble             ; Extract the lo-nibble.
                bcs     .skip4

                inc     <apl_nibflg             ; Reset the flag.
                +APL_GET_SRC
                sta     <apl_nibble             ; Preserve for next time.
                lsr                             ; Extract the hi-nibble.
                lsr
                lsr
                lsr

.skip4:         and     #$0F                    ; Current nibble.
                beq     .write_byte             ; Offset=0 means write zero.

                } else {

.copy_short:    lda     #$10
.nibble_loop:   asl     <apl_bitbuf
                bne     .skip4
                pha
                jsr     .load_bit
                pla
.skip4:         rol
                bcc     .nibble_loop
                beq     .write_byte             ; Offset=0 means write zero.

                }

                eor     #$FF                    ; Read the byte directly from
                tay                             ; the destination window.
                iny
                dec     <apl_dstptr + 1
                lda     (apl_dstptr),y
                inc     <apl_dstptr + 1
                ldy     #0
                beq     .write_byte

                ;
                ; 1 1 0 dddddddn - Copy 2 or 3 within 128 bytes.
                ;

.copy_normal:   +APL_GET_SRC                    ; 1 1 0 dddddddn
                lsr
                beq     .finished               ; Offset 0 == EOF.

                sta     <apl_offset + 0         ; Preserve offset.
                sty     <apl_offset + 1
                tya                             ; Y == 0.
                tax                             ; Bits 8..15 of length.
                adc     #2                      ; Bits 0...7 of length.
                bne     .do_match               ; NZ from previous ADC.

                ;
                ; Subroutines for byte & bit handling.
                ;

                !ifdef  APL_ENHANCED {

.get_gamma:     lda     #1                      ; Get a gamma-coded value.
.gamma_loop:    asl     <apl_egamma
                bne     .rotate_gamma
                pha
                +APL_GET_SRC                    ; Reload an empty bit-buffer
                rol                             ; from the compressed source.
                sta     <apl_egamma
                pla
.rotate_gamma:  rol
                bcs     .big_gamma              ; Got 8 bits, now read rest.
                asl     <apl_egamma
                bcc     .gamma_loop
                rts                             ; Always returns CS.

.big_gamma:     pha                             ; Read remaining bits of length
                tya                             ; larger than 255. This is very
                jsr     .rotate_gamma           ; rare, so it saves cycles on
                tax                             ; the 6502 to do it this way.
                pla

.finished:      rts                             ; All decompressed!

                } else {

.get_gamma:     lda     #1                      ; Get a gamma-coded value.
.gamma_loop:    asl     <apl_bitbuf
                bne     .skip5
                pha
                jsr     .load_bit
                pla
.skip5:         rol
                rol     <apl_length + 1
                asl     <apl_bitbuf
                bne     .skip6
                pha
                jsr     .load_bit
                pla
.skip6:         bcs     .gamma_loop

.finished:      rts                             ; All decompressed!

                }

                ;
                ; 1 0 <offset> <length> - gamma-coded LZSS pair.
                ;

                !ifdef  APL_ENHANCED {

.copy_large:    jsr     .get_gamma              ; Bits 8..15 of offset (min 2).

                cpx     #1                      ; CC if LWM==0, CS if LWM==1.
                ldx     #0                      ; Clear hi-byte of length.
                sbc     #2                      ; -3 if LWM==0, -2 if LWM==1.
                bcs     .normal_pair            ; CC if LWM==0 && offset==2.

                jsr     .get_gamma              ; Get length (A=lo-byte & CS).
                bcs     .do_match               ; Use previous Offset.

.normal_pair:   sta     <apl_offset + 1         ; Save bits 8..15 of offset.

                +APL_GET_SRC
                sta     <apl_offset + 0         ; Save bits 0...7 of offset.

                jsr     .get_gamma              ; Get length (A=lo-byte & CS).

                } else {

.copy_large:    jsr     .get_gamma              ; Bits 8..15 of offset (min 2).
                sty     <apl_length + 1         ; Clear hi-byte of length.

                cpx     #1                      ; CC if LWM==0, CS if LWM==1.
                sbc     #2                      ; -3 if LWM==0, -2 if LWM==1.
                bcs     .normal_pair            ; CC if LWM==0 && offset==2.

                jsr     .get_gamma              ; Get length (A=lo-byte & CC).
                ldx     <apl_length + 1
                bcc     .do_match               ; Use previous Offset.

.normal_pair:   sta     <apl_offset + 1         ; Save bits 8..15 of offset.

                +APL_GET_SRC
                sta     <apl_offset + 0         ; Save bits 0...7 of offset.

                jsr     .get_gamma              ; Get length (A=lo-byte & CC).
                ldx     <apl_length + 1

                }

                ldy     <apl_offset + 1         ; If offset <    256.
                beq     .lt256
                cpy     #$7D                    ; If offset >= 32000, length += 2.
                bcs     .match_plus2
                cpy     #$05                    ; If offset >=  1280, length += 1.
                bcs     .match_plus1
                bcc     .do_match
.lt256:         ldy     <apl_offset + 0         ; If offset <    128, length += 2.
                bmi     .do_match

                !ifdef  APL_ENHANCED {
                } else {
                sec                             ; aPLib gamma returns with CC.
                }

.match_plus2:   adc     #1                      ; CS, so ADC #2.
                bcs     .match_plus256

.match_plus1:   adc     #0                      ; CS, so ADC #1, or CC if fall
                bcc     .do_match               ; through from .match_plus2.

.match_plus256: inx

.do_match:      eor     #$FF                    ; Negate the lo-byte of length
                tay                             ; and check for zero.
                iny
                beq     .calc_addr
                eor     #$FF

                inx                             ; Increment # of pages to copy.

                clc                             ; Calc destination for partial
                adc     <apl_dstptr + 0         ; page.
                sta     <apl_dstptr + 0
                bcs     .calc_addr
                dec     <apl_dstptr + 1

.calc_addr:     sec                             ; Calc address of match.
                lda     <apl_dstptr + 0
                sbc     <apl_offset + 0
                sta     <apl_winptr + 0
                lda     <apl_dstptr + 1
                sbc     <apl_offset + 1
                sta     <apl_winptr + 1

.copy_page:     lda     (apl_winptr),y
                sta     (apl_dstptr),y
                iny
                bne     .copy_page
                inc     <apl_winptr + 1
                inc     <apl_dstptr + 1
                dex                             ; Any full pages left to copy?
                bne     .copy_page

                inx                             ; LWM=1.
                jmp     .next_tag

                ;
                ; Subroutines for byte & bit handling.
                ;

.load_bit:      +APL_GET_SRC                    ; Reload an empty bit-buffer
                rol                             ; from the compressed source.
                sta     <apl_bitbuf
                rts
