;86 Gray Scale routine by Dux Gregis
;Edited by James Rubingh to stay off swapable pages
;This makes it so that you can have a grayscale routine running
;load a picture from a seperate 
;string file. rst 20h \ rst 10h will not cause this routine to 
;crash like most other routines that use areas between 8000-BFFF

Gray1 = $Fc00
Gray2 = $cA00 


OpenGray:
 di

 ld a,$3c
 ld (_b),a        ;set up counters
 ld a,3
 ld (_d),a

 ld hl,int_start     ;copy interrupt to where it'll exec
 ld de,$FaFa
 ld bc,int_end-int_start
 ldir

 ld hl,$F900      ;make 256 byte vector
 ld (hl),$FA      ;table pointing to int
 ld de,$F901
 ld bc,256
 ldir
 
 ld a,$F9
 ld i,a      ;load interrupt register with vector table
 im 2        ;set interrupt mode
 ei          ;start the interrupt
 ret

CloseGray:
 im 1                 ;reset interrupt mode for OS
 ld a,$3C             ;make sure video mem is active
 out (0),a
 ret

int_start:    ;this is what is exec when user int is triggered
 push hl
; push de
 push bc
 push af
 in a,(3)
 bit 1,a
 jr z,leave_int     ;skip when LCD is updating
; inc (hl)          ;inc user counter
 ld a,(_b)
 out (0),a
 ld hl,_d
 ld a,(hl)
 dec (hl)
 dec a                 
 call z,reset_int_counter
 ld a,(_d)
 cp 1
 call z,change_pages    ;flip pages every once and a while
leave_int:
 in a,(3)    ;this stuff must be done or calc crashes
 rra         ;mysterious stuff from the ROM
 ld a,0
 adc a,9
 out (3),a   ;i beleive this updates the lcd
 ld a,11
 out (3),a   ;and tells that it was updated
			 ;its some DMA hw thing
 pop af
 pop bc
; pop de
 pop hl
 ei
 reti
int_end:

reset_int_counter:  ;reset int counter _d when it is 0
 ld a,3
 ld (_d),a
change_pages:
 ld a,(_b)
 xor %00110110        
 ld (_b),a          ;swtich betwwen the visible screen areas
 ret

_b:
.db 0
_c:
.db 0
_d:
.db 0
_e:
.db 0
