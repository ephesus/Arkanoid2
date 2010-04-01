Arkanoid_2_v.0
by_James_Rubingh__james@wrive.com
http://wrive.com

;Made with Asm Studio 4.0b (unreleased to the public)
;November 14, 1999

#include "rpg.h"			;header file

;temporary variables
nolvl 		= _textShadow
selection	= _textShadow+1
menuTop		= _textShadow+2
level		= _textShadow+3	;[1]
score		= _textShadow+4	;[2]
number_blocks = _textShadow+6
number_levels = _textShadow+7
siddhartha = _textShadow+8;Siddhartha is a book by Herman Hesse.
X = _textShadow+9
Y = _textShadow+11
pX = _textShadow+13		;position of the paddle
p = _textShadow+14
oldX = _textShadow+15
oldY = _textShadow+17
hits = _textShadow+19
lives = _textShadow+20
bonusflag = _textShadow+22
bonus_address = _textShadow+23
bonusTimer = _textShadow+25
Dostoevsky = _textShadow+26		;if not null, protector bar over bottom
aczbonuzflag = _textShadow+28
worldvariable = _textShadow+158

;page $D addresses
INC_AHL   	= $4637
GETB_AHL  	= $46C3
SWAP_RAM7 	= $47F3

.org _asm_exec_ram

 .db 0
 jp ProgStart
  nop
  nop
 .dw txtMenuTop
ProgStart:

 ld (NextLevel+1),sp
 call _runindicoff
 ld hl,ScreenName-1
 rst 20h
 rst 10h
 jr c,skiptitlescreendisplaying
   ld a,b
   ex de,hl
   call _conv_ahl
   ld de,$C002
   add hl,de
   set 6,a
   out (5),a
   inc a
   out (6),a
   ld de,Gray2
   call DispRLE
   ld de,Gray1
   inc hl
   call DispRLE
   call OpenGray
   ld a,$d
   out (5),a
   ld a,%01000001
   out (6),a
   call key
   call CloseGray

skiptitlescreendisplaying:
;A Bit of Initialization
 xor a
 ld hl,_textShadow
 ld b,168
setupformenu:
 ld (hl),a
 inc hl
 djnz setupformenu
 ld a,(savedgame)
 or a
 jr z,DontLoadSavedGame

  xor a
  ld (savedgame),a
  ld hl,savedgamelevelstitle
  ld de,worldvariable
  push de
  ld bc,9
  ldir
  pop hl
  dec hl
  call LoadLevel
  ld a,(savedlives)
  ld (lives),a
  ld hl,(savedscore)
  ld (score),hl
  ld a,(savedlevel)
  ld (level),a
  ld a,(saveddelay)
  ld (delaytime),a

  ld hl,savedfield
  ld de,LevelBuffer
  ld b,24
UCL2:
  push bc
  ld b,4
  ld c,(hl)
UnCompressLevelLoop:
  rlc c
  rlc c
  ld a,c
  and 3
  ld (de),a
  inc de
  djnz UnCompressLevelLoop
  pop bc
  inc hl
  djnz UCL2

  jr LoadedSavedGame
DontLoadSavedGame:
 call TableSelect		;select which level to play and load it to $8000
 ld a,2
 ld (lives),a
LoadedSavedGame:
;this routine is Mathew Shepcar's, get it at http://icarus.ticalc.org
BuildTrigTables:
 ld hl,TrigPrecalc
 ld de,SinCosTable
 push de
 ld bc,65
 ldir
 dec hl
 ld b,63
MSW
 dec hl
 ld a,(hl)
 ld (de),a
 inc de
 djnz MSW
 pop hl
 ld b,128+64
NSW
 xor a
 sub (hl)
 ld (de),a
 inc hl
 inc de
 djnz NSW

 jp SetUpNewLife

mainloop:
 
   ld a,%00111110
   out (1),a
   nop
   in a,(1)
 bit 6,a
 jp z,_EXIT_
  push af
 bit 7 ,a
 call z,gamepaused
  pop af
  push af
 bit 1,a
 call z,movepaddleleft
  pop af
  push af
 bit 2,a
 call z,movepaddleright
  pop af
 bit 4,a
 jp z,SaveGameAndJet

 call MoveBall

 call DisplayBlocks
 ld hl,$fc00+512
 xor a
 ld bc,(28*16)-1
 call Fill
 ld hl,Gray2+512
 ld bc,(28*16)-1
 call Fill

   call DrawBonus	;if it exists
   call DrawSaveBar	;if it exists

 ld a,(X+1)			;draw Ball
 ld b,a
 dec b
 ld a,(Y+1)
 ld c,a
 dec c
 call FindPixel
 push hl
 ld de,Gray2
 call CopyPlane
 pop hl
 ld de,Gray1
 call CopyPlane 

 ld hl,delaytime
 ld b,(hl)
delay:
 halt
 djnz delay

 ld a,(Y+1)
 cp 58
 jp c,mainloop
  call ClearPaddleAndShow
  ld a,(siddhartha)
  cp 128
  jp nc,mainloop

  ;test paddlehitball from left
  ld a,(X+1)
  ld hl,pX
  cp (hl)
  jp c,testDeath
  
 ;test from right
 ld hl,X+1
 ld a,(p)
 or a
 jr z,testrightonlittlepaddle 
  ld a,(pX)
  add a,32
  cp (hl)
  jr c,testDeath
  jp PaddleHitBall

testrightonlittlepaddle:
  ld a,(pX)
  add a,16
  cp (hl)
  jr c,testDeath
;  jp PaddleHitBall

PaddleHitBall:
 ld hl,pX
 ld a,(X+1)
 sub (hl)
 ld b,a
 ld a,(p)
 or a
 jr nz,PaddleHitLit
  rl b
PaddleHitLit:
 ld a,b
 add a,a
 sub 32
 ld b,a
 ld hl,siddhartha
 ld a,(hl)
 neg
 add a,b
 ld (hl),a 
 
 ld a,1
 ld (hits),a
 jp mainloop

testDeath:
  ld a,(Y+1)
  cp 60
  jr nc,TheyDied
  jp mainloop
TheyDied:
  	ld a,(Dostoevsky)		;test for Save Bar
    or a
    jr z,NoSaveBarEither
    dec a
    ld (Dostoevsky),a
    ld a,(siddhartha)
    neg
    ld (siddhartha),a
    jp mainloop

NoSaveBarEither: 
  ld a,(lives)
  or a
  jr z,ReallyDead
  dec a
  ld (lives),a
  jp SetUpNewLife

ReallyDead:
 ld a,(lives)
 or a
 jr z,HeDidntWin_heDiedhaha
 ld b,a
 ld hl,(score)
dwgloop:
 ld de,1200
 add hl,de
 djnz dwgloop
 ld (score),hl

HeDidntWin_heDiedhaha:
 call ShowStats

 call CLEARSCREEN
 call CloseGray
 ld hl,FinalScoreText
 ld bc,$1515
 call displaytext
 call _pause
 jp _EXIT_

DrawBonus:
 ld a,(bonusflag)
 or a
 ret z

 add a,a
 add a,a
 sub 4
 ld de,BonusImages
 add a,e
 ld e,a
 adc a,d
 sub e
 ld d,a		;de equals 

 ld hl,(bonus_address)
 ld bc,Gray2
 push de
 push hl
 add hl,bc
 call DrawBonux
 pop hl
 pop de
 ld bc,Gray1
 add hl,bc
 call DrawBonux

  ld a,(bonusTimer)
  xor 1
  ld (bonusTimer),a
  ret z

 ld hl,(bonus_address)
 ld bc,16
 add hl,bc
 ld (bonus_address),hl

  srl h
  rr l
   srl h
   rr l
  srl h
  rr l
   srl h
   rr l
 ld a,l
 cp 58
 ret c
;test if the bonus hit the paddle when it went out
 ld hl,pX
 ld a,(bonus_address)
 and 15
 ld c,(hl)
 srl c
 srl c
 srl c
 cp c
 jr z,hitbonus
 inc c
 cp c
 jr z,hitbonus
 inc c
 cp c
 jr z,hitbonus
 ld e,a
 ld a,(p)
 or a
 jr z,TheBonusDied
 ld a,e
 cp c
 jr z,hitbonus
 inc c
 cp c
 jr z,hitbonus
 inc c
 cp c
 jr z,hitbonus
TheBonusDied:
 xor a
 ld (bonusflag),a
	call ClearPaddleArea
    call ShowPaddle
 jp ShowStats

hitbonus:
 ld a,(bonusflag)
 dec a
 jr z,DoBonux1
 dec a
 jr z,DoBonux2
 dec a
 jr z,DoBonux3
 dec a
 jr z,DoBonux4
 dec a
 jr z,DoBonux5
 dec a
 jr z,DoBonux6
 dec a

;The ACZ bonus is a jewel of kick ass programming...
;It rages against the machine.

aczbonux:
 or %11111000
 rlca
 rlca
 rlca
 ld (Self_Modifying_Dope_Code_for_Your_Mom+1),a
 ld a,(aczbonuzflag)
Self_Modifying_Dope_Code_for_Your_Mom:
 set 0,a
 ld (aczbonuzflag),a
 and 7
 cp 7
 jr nz,DoneActingBonus
 xor a
 ld (aczbonuzflag),a		;clear the flag
 ld (bonusflag),a
 call NextLevel

DoBonux6:
 ld a,(lives)
 or a
 jr z,LastLife
   dec a
   ld (lives),a
 jr DoneActingBonus
LastLife:
 ld hl,420
 ld (score),hl
 jr DoneActingBonus

DoBonux5:
 ld hl,(score)
 ld de,501
 add hl,de
 ld (score),hl
 jr DoneActingBonus

DoBonux4:
 ld hl,(score)
 ld de,300
 add hl,de
 ld (score),hl
   ld a,1
   ld (Dostoevsky),a
 jr DoneActingBonus
DoBonux3:
 ld a,3
 ld (Dostoevsky),a		;signify protector bar over bottom
 jr DoneActingBonus
DoBonux1:
 ld a,(lives)
 inc a
 ld (lives),a
 jr DoneActingBonus
DoBonux2:
 ld a,69
 ld (p),a		;set p to anything but null
  ld a,(pX)		;make sure a big paddle isn't off the side
  cp 96
  jp c,DoneActingBonus
  ld a,95
  ld (pX),a

DoneActingBonus:
 ld hl,(score)
 ld de,100
 add hl,de
 ld (score),hl

 call InvertScreen
 jp TheBonusDied

DrawBonux:
 ld b,4
DrawBonux2:
 ld a,(de)
 ld (hl),a
 inc de
   ld a,l
   add a,16
   ld l,a
   adc a,h
   sub l
   ld h,a
 djnz DrawBonux2
 ret

InvertScreen:
 ld hl,Gray2+$80
 call IS2
 ld hl,$fc80
IS2:
 ld bc,$400-$80
InvertScreen_:
 ld a,(hl)
 cpl
 ld (hl),a
 inc hl
 dec bc
 ld a,b
 or c
 jr nz,InvertScreen_
 halt
 ret
 


movepaddleleft:
 ld a,(pX) 
 dec a
 dec a
 cp 0
 jp p,donewithpaddle
  ret
movepaddleright:
 ld a,(p)
 or a
 jr z,littlepaddle
 ld a,(pX)
 inc a 
 inc a
 cp 97
 ret nc
 jp donewithpaddle

littlepaddle:
 ld a,(pX)
 inc a
 inc a
 cp 113
 ret nc
donewithpaddle:
  ld (pX),a
ClearPaddleAndShow:
  call ClearPaddleArea
  jp ShowPaddle
;  ret  

SetUpNewLife:
 xor a					;count how many blocks are left
 ld hl,number_blocks
 ld (hl),a
 ld b,96
 ld de,LevelBuffer
countblocksinthislevel:
 ld a,(de)
 dec a
 jr z,counttheblock
 dec a
 jr nz,dontcounttheblock
counttheblock:
 inc (hl)
dontcounttheblock:
 inc de
 djnz countblocksinthislevel

 call ShowStats		;update the Lives Text
 call CloseGray
 call CLEARSCREEN
 call DisplayShell 
 call Displaylivesthing
 call OpenGray

  call CLEARSCREEN

 ld a,$2a
 ld (pX),a
 ld a,1
 ld (hits),a
 ld hl,$3700
 ld (X),hl
 ld (Y),hl
 ld a,-32
 ld (siddhartha),a
 call ShowStats
 call DisplayBlocks
 ld hl,$FC70
 ld a,-1
 ld bc,15
 call Fill
 xor a
 ld (p),a		;make sure paddle is normal
 ld (Dostoevsky),a	;clear any protection bars
 ld (aczbonuzflag),a		;clear the flag
 call ShowPaddle
 call DrawBonus
 call paused
 jp mainloop

ShowStats:
;---==-=-- display the top infos

 ld hl,(score)
 ld de,ScoreUnpackPointer
 ld b,5
unpackingscoreloop:
 call $4044
 add a,48
 ld (de),a
 dec de
 djnz unpackingscoreloop

 ld hl,lives
 ld l,(hl)
 ld h,0
 inc hl
 ld b,2
 ld de,LivesUnpackPointer
unpacklivesloop:
 call $4044
 add a,48
 ld (de),a
 dec de
 djnz unpacklivesloop

 ld hl,msgScore
 ld bc,0
 call displaytext
 ld hl,msglives
 ld bc,$0040
 jp displaytext

MoveBall:
 ld hl,(X)
 ld (oldX),hl
 ld hl,(Y)
 ld (oldY),hl 
 ld a,(siddhartha)
 ld hl,SinCosTable
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld c,(hl)
 call extendBC
 ld hl,(Y)
 add hl,bc
 add hl,bc
 add hl,bc
 ld (Y),hl
  ld a,h
  cp 9
  jp c,HitTOB
     call TestHitTOB
MoveTheXcoordinate:
 ld hl,siddhartha
 ld b,(hl)
 ld a,64
 sub b
 ld hl,SinCosTable
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld c,(hl)
 call extendBC
 ld hl,(X)
 add hl,bc
 add hl,bc
 add hl,bc
 ld (X),hl
  ld a,h
  cp 125
  jp nc,HitSideOfSomething
  cp 1
  jr c,HitSideOfSomething
  call TestIfHitSides
  ret
HitSideOfSomething:
 ld hl,siddhartha
 ld a,128
 sub (hl)
 ld (siddhartha),a
 ld hl,(oldX)
 ld (X),hl
 ret

TestIfHitSides:
 call GetBlock
 or a
 ret z
 call ChangeBlock
 call EditScoreCheckLevel
 jp HitSideOfSomething
 
TestHitTOB:
 call GetBlock
 or a
 ret z
 call ChangeBlock
 call EditScoreCheckLevel
HitTOB:
 ld hl,siddhartha
 ld a,(hl)
 neg
 ld (hl),a
 ld hl,(oldY)
 ld (Y),hl
 ret

EditScoreCheckLevel:
 ld hl,(score)
 ld a,(hits)
 add a,a
 add a,l
 ld l,a 
 adc a,h
 sub l
 ld h,a
 ld (score),hl 

 ld hl,hits
 inc (hl)
 inc (hl)
 jp ShowStats

GetBlock:
 ld a,(Y+1)
 cp 32
 jp nc,BallIsOffBlocks
 
 sub 8
 and 252
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 ld a,(X+1)
 srl a
 srl a
 srl a
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld de,LevelBuffer
 add hl,de
 ld a,(hl)
 ret
BallIsOffBlocks
 xor a
 ret

NextLevel:
 ld sp,0	;otherwise use 3 pops
 ld a,(level)
 ld hl,number_levels
 cp (hl)
 jp z,ReallyDead

 inc a
 ld (level),a
 ld hl,worldvariable-1
 call LoadLevel
 ld hl,(score)
 ld bc,500
 add hl,bc
 ld (score),hl
 jp SetUpNewLife

ChangeBlock:
 ld a,(Y+1)
 sub 8
 and 252
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 ld a,(X+1)
 srl a
 srl a
 srl a
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld de,LevelBuffer
 add hl,de
 ld a,(hl)
 cp 3
 ret z
Decrease_Block:
 dec (hl)
 ret nz

  ld a,(number_blocks)
  dec a
  or a
  jr z,NextLevel
  ld (number_blocks),a

   ld a,(bonusflag)	;make sure there isn't already a bonus
   or a
   ret nz

  ld a,r		;invoke bonus, refresh register = pseudo random number
  and %00011110
  srl a
  cp 9
  ret nc
 inc a
 ld (bonusflag),a		;store bonus type
 ld a,(X+1)
 srl a
 srl a
 srl a
 ld hl,Y+1
 ld l,(hl)
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 add hl,hl
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld (bonus_address),hl
 ret

CopyPlane:
 add hl,de
 ld c,3
 ld d,a
__CopyPlane:
 ld b,3
 push hl
_CopyPlane:
 ld e,a
 or (hl)
 ld (hl),a
 ld a,e
 rrca
 jr nc,SkipIncrease
   inc hl
SkipIncrease:
 djnz _CopyPlane
 pop hl
  ld a,16
  add a,l
  ld l,a
  adc a,h
  sub l
  ld h,a
 ld a,d
 dec c
 jr nz,__CopyPlane

 ret

ClearPaddleArea:
  ld hl,Gray2+($3c*16)
  xor a
  ld bc,(4*16)-1
  call Fill
  ld hl,Gray1+($3c*16)
  ld bc,(4*16)-1
  jp Fill

gamepaused:
 call InvertScreen
 set 3,(iy+5)
 call paused
 res 3,(iy+5)
 jp ClearPaddleAndShow

paused:
 ld bc,$2a2a
 ld hl,msgPaused
 call displaytext
 ld b,40
paused2
 halt
 djnz paused2
 jp key

DisplayBlocks:
  ld hl,128		;starts one row down.
  ld de,LevelBuffer
  ld c,6
DisplayBlocks3
  ld b,16
DisplayBlocks2
 push bc
   ld a,(de)
  push de
    call DrawBlockStyleAatHL
  pop de
   inc de
   inc hl
 pop bc
 djnz DisplayBlocks2
   ld a,48
   add a,l
   ld l,a
   adc a,h
   sub l
   ld h,a
 dec c
 jp nz,DisplayBlocks3
 ret

DrawBlockStyleAatHL
 ld de,16
 dec a
 jp z,DrawType1
 dec a
 jp z,DrawType2
 dec a
 jp nz,DrawType0
DrawType3:
 push hl
 ld bc,Gray2
 add hl,bc
 ld a,-1
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 pop hl
 push hl
 ld b,$fc
 add hl,bc
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 pop hl
 ret
 
DrawType2:
 push hl
 ld bc,Gray2
 add hl,bc
 xor a
 ld (hl),a
 add hl,de
 ld a,%01111110
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 xor a
 ld (hl),a
 add hl,de
 pop hl
 push hl
 ld b,$fc
 add hl,bc
 ld a,-1
 ld (hl),a
 add hl,de
 ld a,%10000001
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld a,-1
 ld (hl),a
 add hl,de
 pop hl
 ret
DrawType1:
 push hl
 ld bc,Gray2
 add hl,bc
 ld a,$7F
 ld (hl),a
 add hl,de
 ld a,$01
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 xor a
 ld (hl),a
 add hl,de
 pop hl
 push hl
 ld b,$fc
 add hl,bc
 ld a,$80
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld a,-1
 ld (hl),a
 add hl,de
 pop hl
 ret

DrawType0:
 push hl
 ld bc,Gray2
 add hl,bc
 xor a
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 pop hl
 push hl
 ld b,$fc
 add hl,bc
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 ld (hl),a
 add hl,de
 pop hl
 ret

DrawSaveBar:
   ld a,(Dostoevsky)
   or a
   ret z
 dec a
 jr z,DrawSaveCheckeredLess
 dec a
 jr z,DrawSaveCheckered
 ld a,-1
 jr DrawSaveDoneLoad
DrawSaveCheckeredLess:
 ld a,%10001000
 jr DrawSaveDoneLoad
DrawSaveCheckered:
 ld a,%10101010
DrawSaveDoneLoad:
 ld hl,$fFF0
 call DrawSaveMainLoop
 ld hl,Gray2+$3F0
DrawSaveMainLoop:
 ld b,8
DrawSaveLoop:
 ld (hl),a
 inc hl
 ld (hl),a
 inc hl
 djnz DrawSaveLoop
 ret

CLEARSCREEN:
 xor a
 ld hl,Gray2
 ld bc,$3ff
 call Fill
 ld hl,Gray1
 ld bc,$3ff
 jp Fill

key:
  halt
 ld a,%00111101
 out (1),a
    nop
 in a,(1)
 bit 5,a
    ret z
 bit 6,a
    ret z
 bit 1,a
    ret z
 jr key

ShowPaddle:
   ld a,(pX)
   ld b,a
   ld c,$3c
  push bc
   ld a,(p)
   or a
 jr nz,ShowBigAssPaddle
   ld hl,LittlePaddle 
   ld de,Gray2
   call PutSprite
  pop bc
   ld hl,LittlePaddle2
   ld de,Gray1
   jp PutSprite
ShowBigAssPaddle:
   ld hl,BigPaddle
   ld de,Gray2
   call PutSprite 
  pop bc
   ld hl,BigPaddle2
   ld de,Gray1
   jp PutSprite

DisplayShell:
 ;---
;Display the detected Tables
;---

  ld hl,$Fc00 ;_plotSScreen
  ld bc,$3ff
  ld a,-1
  call Fill
  set 3,(iy+5)
  ld bc,4
  ld hl,txtMenuTop
  call displaytext
  ld bc,$3904
  ld hl,txtAuthor
  call displaytext
  res 3,(iy+5)
 ret
 
TableSelect:
 xor a
 ld (nolvl),a     ;clear the number of levels byte 
 call SWAP_RAM7

 ld ix,_plotSScreen
 ld hl,$BFFF
search: 
 ld de,($D298)
 inc de
 or a
 sbc hl,de
 jr c,Searchend

 add hl,de
 ld a,(hl)
 and %00011111
 dec hl
 ld e,(hl)        ;Load the absolute pointer into ADE, not A yet though 
 dec hl           ;because the type is still in there! 
 ld d,(hl) 
 dec hl 
 cp $0c
 ld a,(hl)
 dec hl
 dec hl
 jr nz,Not
 push hl
 ex de,hl
 call $4c3f



 ld c,a
 push hl
 call GETB_AHL
 cp $c9
 pop hl
 ld a,c            
 jr nz,BHNot
 call INC_AHL
 call GETB_AHL
 or a
 jr nz,BHNot
 
  call SWAP_RAM7
  pop hl
  push hl
  ld a,(hl)
  ld (ix),a

  ld b,a 
  push ix 
copytitle: 
  inc ix
  dec hl
  ld a,(hl) 
  ld (ix),a 
  djnz copytitle 
  pop ix        ;use 9 bytes for MY table of levels 8 for name 
  ld de,9 
  add ix,de 
  ld a,(nolvl)
  inc a 
  ld (nolvl),a 
 
 
BHNot:            ; swap the vat back in, because a different RAM page 
 call SWAP_RAM7   ; might have been swapped in other than 7 
 pop hl           ; pop the vat pointer 
Not: 
 ld b,(hl)        ;skip the name, and then youre pointing at the next 
 inc b            ;Vat entry's sign byte!! 
SkipName: 
 dec hl 
 djnz SkipName 
 jr search        ;loop again and check the next variable 
 
Searchend: 
 ld a,(nolvl) 
 or a 
 jr z,NOLEVELS                  ;check for no levels 

 call DisplayShell

  xor a				;current level highlighted
  ld (selection),a
  ld (menuTop),a
  call DrawLevels
  call InvertLine

LevelMenuKeyLoop:
  halt
  call GET_KEY
  cp K_ENTER
  jr z,LoadChosenTable
  cp K_EXIT
  jp z,_EXIT2
  cp K_UP
  call z,menuUp
  cp K_DOWN
  call z,menuDown 

  jr LevelMenuKeyLoop

NOLEVELS:
 call CLEARSCREEN
 ld hl,msgNoLevels
 ld bc,$1818
 call displaytext
 call GET_KEY		;if this is removed. It causes a double press bug
 call _pause
 jp _EXIT2
msgNoLevels:  .db "No Levels Detected",0  

menuDown:
 ld a,(selection)
 ld hl,nolvl
 ld b,(hl)
 ld hl,menuTop
 add a,(hl)
 dec b
 cp b
 ret z
 sub (hl)
 inc a
 cp 6
 jr nz,dontgoforpageflip
  ld a,(menuTop)
  inc a 
  ld (menuTop),a
  ld a,5
dontgoforpageflip: 
 ld (selection),a
 call DrawLevels
 jp InvertLine

menuUp:
 ld a,(selection)
 or a
 jr nz,skipspecialsituationcheck
  ld a,(menuTop)
  or a
  ret z	;if that was the first file
  dec a
  ld (menuTop),a
  ld a,1
skipspecialsituationcheck:
 dec a
 ld (selection),a
 call DrawLevels
 jp InvertLine

LoadChosenTable:	;to $8000
 ld a,(menuTop)
 ld hl,selection
 add a,(hl)
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld de,_plotSScreen 
 add hl,de
 push hl
 ld de,worldvariable
 ld bc,9
 ldir
 pop hl 
 dec hl 
 call LoadLevel

 call ClearLevelTextArea
 ld bc,$2020
 ld hl,msgSelectSpeed
 call displaytext
 
speedloopwait:
 call GET_KEY
 cp K_EXIT
 jp z,_EXIT2
 cp K_F1
 jr z,speed1
 cp K_F2
 jr z,speed2
 cp K_F3
 jr z,speed3
 cp K_F4
 jr z,speed4
 cp K_F5
 jr z,speed5
 jr speedloopwait

speed1:
 ld a,5
 jr donewithspeedselect
speed2:
 ld a,4
 jr donewithspeedselect
speed3:
 ld a,3
 jr donewithspeedselect
speed4:
 ld a,2
 jr donewithspeedselect
speed5:
 ld a,1
donewithspeedselect:
 ld (delaytime),a
 ret

LoadLevel:		;hl is level data in FIND_SYM format
 rst 20h
 rst 10h
 ex de,hl
 ld a,b 
 call _conv_ahl
 set 6,a
 out (5),a
 inc a
 out (6),a
 ld de,$c004
 add hl,de
 ld a,(hl)
 dec a
 ld (number_levels),a
 inc hl
						 push hl
 ld a,(level)
 ld e,a
 ld d,0
 ex de, hl
 call HLtimes24
 add hl,de
 
UnpackTheFuckingLevel:	;the cause of my expletives: this routine didn't
					;seem to work for the first billion times I wrote it
 push hl
 ld hl,LevelBuffer
 ld bc,167
 xor a
 call Fill
 pop hl

 ld de,LevelBuffer
 ld c,24			;24 bytes
SecondFuckOfALoop:
 ld a,(hl)
 ex de,hl
 ld b,4
LoopInsideThisFuckingFuck:
 rl a
 rl (hl)
 rl a
 rl (hl)
 inc hl
 djnz LoopInsideThisFuckingFuck
 ex de,hl
 inc hl
 dec c
 jr nz,SecondFuckOfALoop
							pop hl
 							push hl
  dec hl
  ld l,(hl)
  ld h,0
  call HLtimes24
  							pop de
  add hl,de
  ld de,EmbeddedText
copythemessageshit
  ld a,(hl)
  ld (de),a
  or a
  jr z,donecopyingmessage
   inc hl
   inc de
   jr copythemessageshit
donecopyingmessage:
 ld a,%01000001
 out (6),a
 ld a,$d
 out (5),a
 ret		;end of table select

displaytext:
 ld (_penCol),bc
 jp _vputs 

Displaylivesthing:
 call ClearLevelTextArea
 ld hl,EmbeddedText
 ld bc,$1005
 call displaytext
 ld bc,$1905
 ld hl,msglives
 call displaytext
 call paused
 ret

HLtimes24:
 add hl,hl
 add hl,hl
 add hl,hl
 ld c,l
 ld b,h
 add hl,bc
 add hl,bc
 ret

extendBC:
;this is kindof like the motorola 68k command EXT
 ld b,0
 bit 7,c
 ret z
   ld b,-1
   ret

InvertLine:	;highlight the level indicated by "selection"
  ld de,$fc00+(16*8)
  ld a,(selection)
  ld h,a
  ld l,0
  srl h
  rr l
  add hl,de
  ld bc,7*16
ILloop:
 ld a,(hl)
 cpl 
 ld (hl),a
 inc hl
 dec bc
 ld a,c
 or b
 jr nz,ILloop  
  ret

DrawLevels:
 call ClearLevelTextArea
 ld a,(menuTop)
 ld de,9
 ld hl,_plotSScreen
DLloops:
 or a
 jr z,DLPrintingLoop 
   add hl,de  
   dec a
   jr DLloops
DLPrintingLoop:
 ld a,(nolvl)		;(nolvls) is always > 0
 ld b,a
 ld a,6
 cp b
 jr nc,Drawwithoutswitch
   ld b,a
Drawwithoutswitch
 ld de,$0802
DrawNamesLoop:
 push bc
 push hl
 ld b,(hl)
 inc hl
 ld (_penCol),de
 call _vputsn
 pop hl
 ld a,9
 add a,l
 ld l,a
 adc a,h
 sub l
 ld h,a
 ld a,d
 add a,8
 ld d,a
 pop bc
 djnz DrawNamesLoop
 ret

ClearLevelTextArea:
 ld hl,$fc00+(16*7)
 ld bc,$3ff-(16*14)
 xor a
 jp Fill

Fill:
 ld e,l
 ld d,h
 inc de
 ld (hl),a
 ldir
 ret

SaveGameAndJet:		; Rage, Rage against the dying of the light
 ld a,-1
 ld (savedgame),a
 jr _EXIT_
 
_EXIT2:
 pop hl
_EXIT_: 
 call CloseGray

 ld bc,9
 ld hl,worldvariable
 ld de,savedgamelevelstitle
 ldir
 ld a,(level)
 ld (savedlevel),a
 ld hl,(score)
 ld (savedscore),hl
 ld a,(lives)
 ld (savedlives),a
 ld a,(delaytime)
 ld (saveddelay),a
;compress the field data
 
 ld hl,LevelBuffer
 ld de,savedfield
 ld b,24
CompressLoop2:
 push bc
 ld c,0
 ld b,4
CompressLoop:
 rlc c
 rlc c
 ld a,(hl)
 inc hl
 or c
 ld c,a
 djnz CompressLoop
 ld a,c
 ld (de),a
 inc de
 pop bc
 djnz CompressLoop2
 
 ld hl,FileName-1
 rst 20h
 rst 10h
 ex de,hl
 ld a,b
 ld de,savedgame-_asm_exec_ram+4
 add hl,de
 adc a,0		;ahl points to savedgame
 ld de,savedgame
 ld b,SavedEnd-savedgame
CopySavedDataLoop:
 push af		;save ahl
 push hl
 call GETB_AHL	;load in the correct page
 ld a,(de)		;take byte
 ld (hl),a		;load it to program data (self modifying)
 pop hl			;pop ahl
 pop af
 inc de
 call $4637		;inc ahl
 djnz CopySavedDataLoop	;copy B bytes

 res 3,(iy+5)
 call _clrScrn
 ret

FindPixel:
 ld h,0
 ld a,c
 add a,a
 add a,a
 ld l,a
 ld a,b
 rra
 add hl,hl
 rra
 add hl,hl
 rra
 or l
 ld l,a
 ld a,b
 and 7
 ld bc,FP_Bits
 add a,c
 ld c,a
 adc a,b
 sub c
 ld b,a
 ld a,(bc)
 ret
FP_Bits:    .db $80,$40,$20,$10,$08,$04,$02,$01

txtMenuTop:	
  .db "Arkanoid 2 v1.02"
  nop
txtLevelSelect:
  .db "Select World"
  nop
txtAuthor:
  .db "By James Rubingh"
  nop
FileName:
  .db 8
  .db "Arknoid2"
ScreenName:
  .db 8
  .db "Ark2Scrn"
;msgHighScore:
; .db "High: "
; nop
msgSelectSpeed:
 .db "F1 - F5 for speed"
 nop
msgPaused:
  .db "Press Second"
  nop
msglives
  .db "Lives: 04"
LivesUnpackPointer = $ - 1
  nop
FinalScoreText:
  .db "Final",32

msgScore
  .db "Score: 00000"
ScoreUnpackPointer = $ - 1
  nop

LittlePaddle:
 .db 2
 .db %01111111,%11111110
 .db %10111111,%11111101
 .db %10000000,%00000001
 .db %01111111,%11111110
LittlePaddle2
 .db 2
 .db %01111111,%11111110
 .db %11000000,%00000011
 .db %11111111,%11111111
 .db %01111111,%11111110

BigPaddle:
 .db 4
 .db %01111111,-1,-1,%11111110
 .db %10111111,-1,-1,%11111101
 .db %10000000,%00000000,%00000000,%00000001
 .db %01111111,-1,-1,%11111110
BigPaddle2:
 .db 4
 .db %01111111,-1,-1,%11111110
 .db %11000000,%00000000,%00000000,%00000011
 .db %11111111,%11111111,%11111111,%11111111
 .db %01111111,-1,-1,%11111110

BonusImages:
 .db %00000000
 .db %01111110
 .db %10000001
 .db %01111110

 .db %01000010
 .db %10000001
 .db %11000011
 .db %01000010

 .db %00000000
 .db %11111111
 .db %11111111
 .db %11111111

 .db %10000001
 .db %10000001
 .db %11111111
 .db %11111111

 .db %10011001
 .db %10100101
 .db %10100101
 .db %10011001

 .db %00100000
 .db %00111000
 .db %00100100
 .db %00111000

 .db %00010000
 .db %00101000
 .db %01111100
 .db %10000010

 .db %00111000
 .db %01000000
 .db %01000000
 .db %00111000

 .db %01111100
 .db %00011000
 .db %00100000
 .db %01111100



savedgame:		;i could organize these to do one big ldir, but i'm too lazy
 nop
savedlevel:
 nop	;level
savedscore:
 .dw 0	;score
savedlives:
 nop
saveddelay:
 nop
savedfield:
 .db 0,0,0,0,0,0,0,0,0,0,0,0
 .db 0,0,0,0,0,0,0,0,0,0,0,0
savedgamelevelstitle:
 .db 0,0,0,0,0,0,0,0,0
;highscore:
; .dw 0
SavedEnd:

#include gray.h 	;This is my routine. It's just like any other one
					;it uses the graph mem for plane 2, and the end of the
					;_asm_exec_ram for the IM 2 "jump table", i stole much from
					;dux though...

#include rle.asm	;David Phillips routine

#include trigdat.asm;Data from a gameboy program called "roller coaster" which will
					;make sine table data for use in games. 

#include sdr16.h	;Edited by me for grayscale. (and not using shadow registers)
					;and personalized for my specific needs. Go to macross.calc.org
					;to get a copy of public generic code

delaytime:
 nop
LevelBuffer:
SinCosTable = $8000
EmbeddedText = $+120

.end
