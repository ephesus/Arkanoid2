;use this when you need a counter

UserCounter             equ             $817e

;===========================================================
; RLE picture displayer v1.1
; Decodes a RLE picture made by RLE2PIC
;
; written by David Phillips <electrum@tfs.net>
; started: 8/19/98
; last update: 11/5/98
;
; input: HL = RLE encoded picture, DE = where to display
; output: 1024 byte decoded picture
; destroys: AF, BC, DE, HL
; current size: 32 bytes
;===========================================================
DispRLE:
 ld bc,1024			; we need to copy 
DispRLEL:
 ld a,(hl)			; get the next byte
 cp $91				; is it a run?
 jr z,DispRLERun	; then we need to decode the run
 ldi				; copy the byte, and update counters
DispRLEC:
 ld a,b				; check the low byte and
 or c				; the high byte for 0
 jr nz,DispRLEL		; if not, then we're not done either
 ret				; if it's zero, we're done
DispRLERun:
 inc hl				; move to the run value
 ld a,(hl)			; get the run value
 inc hl				; move to the run count
 push hl			; save source pointer
 ld h,(hl)			; get the run count
 ex de,hl			; swap source and destination pointers
DispRLERunL:
 ld (hl),a			; copy the byte
 inc hl				; increase destination pointer
 dec bc				; decrease byte count
 dec d				; decrease run count
 jr nz,DispRLERunL	; if we're not done, then loop
 ex de,hl			; swap pointers back
 pop hl				; recover source pointer
 inc hl				; advance the source pointer
 jr DispRLEC		; loop

