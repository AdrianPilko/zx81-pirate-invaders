; Copyright (c) 2024 Adrian Pilkington

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

;;; Classic shootem up game "pirate invaders"
;;;
;;; https://youtube.com/@byteforever7829

;;; Known bug(s)
;;;


;some #defines for compatibility with other assemblers
#define         DEFB .byte 
#define         DEFW .word
#define         EQU  .equ
#define         ORG  .org
CLS				EQU $0A2A
;;;;;#define DEBUG_NO_SCROLL
;#define DEBUG_PLAYER_XY
;;#define DEBUG_SPRITE_ADDRESS 1
;;#define DEBUG_PRINT_ROOM_NUMBER 1
;#define DEBUG_MULTIRATECOUNT 1
;#define DEBUG_START_IN_ROOM_X   1
;#define DEBUG_ROOM_TO_START_IN 7
;#define DEBUG_COLLISION_DETECT_1 1
;#define DEBUG_COLLISION_DETECT_2 1

#define KEYBOARD_READ_PORT_P_TO_Y	$DF
; for start key 
#define KEYBOARD_READ_PORT_A_TO_G	$FD
; keyboard port for shift key to v
#define KEYBOARD_READ_PORT_SHIFT_TO_V $FE
; keyboard space to b
#define KEYBOARD_READ_PORT_SPACE_TO_B $7F 
; keyboard q to t
#define KEYBOARD_READ_PORT_Q_TO_T $FB

; starting port numbner for keyboard, is same as first port for shift to v
#define KEYBOARD_READ_PORT $FE 
#define SCREEN_WIDTH 32
#define SCREEN_HEIGHT 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
#define MISSILE_COUNTDOWN_INIT 9


VSYNCLOOP       EQU      2

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt 
_SD:			EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F


;;;; this is the whole ZX81 runtime system and gets assembled and 
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address
                                                                
VERSN:          DEFB 0
E_PPC:          DEFW 2
D_FILE:         DEFW Display
DF_CC:          DEFW Display+1                  ; First character of display
VARS:           DEFW Variables
DEST:           DEFW 0
E_LINE:         DEFW BasicEnd 
CH_ADD:         DEFW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DEFW 0
STKBOT:         DEFW BasicEnd+5
STKEND:         DEFW BasicEnd+5                 ; Empty stack
BREG:           DEFB 0
MEM:            DEFW MEMBOT
UNUSED1:        DEFB 0
DF_SZ:          DEFB 2
S_TOP:          DEFW $0002                      ; Top program line number
LAST_K:         DEFW $fdbf
DEBOUN:         DEFB 15
MARGIN:         DEFB 55
NXTLIN:         DEFW Line2                      ; Next line address
OLDPPC:         DEFW 0
FLAGX:          DEFB 0
STRLEN:         DEFW 0
T_ADDR:         DEFW $0c8d
SEED:           DEFW 0
FRAMES:         DEFW $f5a3
COORDS:         DEFW 0
PR_CC:          DEFB $bc
S_POSN:         DEFW $1821
CDFLAG:         DEFB $40
MEMBOT:         DEFB 0,0 ;  zeros
UNUNSED2:       DEFW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has 
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DEFB $00,$0a                    ; Line 10
                DEFW Line1End-Line1Text         ; Line 10 length
Line1Text:      DEFB $ea                        ; REM



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
intro_title
	call CLS  ; clears screen and sets the boarder
  
preinit
;; initialise variables that are once per game load/start

initVariables
    xor a
    ld a, (MissileInFlightFlag)
    
    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl 
    ld hl, Display+1 
    ld de, 538
    add hl, de 
    ld (currentPlayerLocation), hl
    ld hl, Display+1 
    ld de, 6
    add hl, de     
    ld (jollyRogerLocation), hl
    ld hl, 1
    ld (jollyRogerDirUpdate),hl
    ld a, 5
    ld (jollyRogerXPos),a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
gameLoop    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld b,VSYNCLOOP
waitForTVSync	
	call vsync
	djnz waitForTVSync
    
    ld de, (currentPlayerLocation)
    ld hl, blankSprite
    ld c, 8
    ld b, 8 
    call drawSprite
    
    ld hl, blankSprite
    ld de, (previousJollyRogerLocation)
    ld c, 8
    ld b, 8    
    call drawSprite    
    call updateJollyRoger        
    ; xor a
    ; ld (evenOddLoopFlag), a    ; used for multi rate enemies
    
    ; ld a, (evenOddLoopCount)
    ; inc a
    ; ld (evenOddLoopCount), a
    ; cp 8    
    ; jr z, resetEvenOddAndSetFlag
    
    jr continueWithGameLoop
resetEvenOddAndSetFlag    
    ; xor a
    ; ld (evenOddLoopCount), a
    ; ld a, 1
    ; ld (evenOddLoopFlag), a    ; used for multi rate enemies
    ; call drawTreasureSub    
    
continueWithGameLoop          
    ; call printLives   
    
        
    ld a, (gameOverRestartFlag)
    cp 1
    jp z, intro_title
    
    ;call blankEnemySprites
    ;call drawEnemySprites        
    ;call updateEnemySpritePositions
       
   
; keyboard layout for reading keys on ZX81
; BIT   left block      right block  BIT
; off                                off in <port>, when ld a, <port>
;       0  1 2 3 4     4 3 2 1 0                 <<< bit to check for each column after in a, $fe 
; 3   ( 1  2 3 4 5 ) ( 6 7 8 9 0 )     4
; 2   ( Q  W E R T ) ( Y U I O P )     5
; 1   ( A  S D F G ) ( H I K L n/l)    6
; 0   (sft Z X C V ) ( B N M . spc)    7
;
; to read keys 1 2 3 4 5
; set all bits except bit 3 of register A = 1 1 1 1 0 1 1 1= f7, then execute in a, $fe  (fe is the "keyboard read port")
; now register a will contain a bit pattern to check for which key in that block was set, eg Key "1" = bit 0 of a
; ld a, $f7    
; in a, $fe    
; similarly for the rest, to read from block A S D F G, set a to 1 1 1 1 1 1 1 0 1 = $fd

    
    ;; read keys
    ld a, KEYBOARD_READ_PORT_P_TO_Y			
    in a, (KEYBOARD_READ_PORT)					; read from io port	
    bit 1, a                            ; O
    jp z, moveLeft


    ld a, KEYBOARD_READ_PORT_P_TO_Y			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a					        ; P
    jp z, moveRight


    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doFireMissile
    
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a						    ; Z
    jp z, doFireMissile    
skipFireKeyDetect_1    
    jp updateRestOfScreen                       ; if no key pressed continue

moveLeft         
    ld a, (playerXPos)
    dec a
    cp 0      ;;; this prevents the player moving past edge, but if it's a door
              ;; trigger seperate code to move to new room
    jp z, updateRestOfScreen   
    ld (playerXPos), a
    
        
    ld hl, (currentPlayerLocation)
    dec hl
    ld (currentPlayerLocation), hl  

     
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doFireMissile
    
    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a						    ; Z
    jp z, doFireMissile        
    jp updateRestOfScreen 
    
moveRight       
    ld a, (playerXPos)
    inc a
    cp 24          ;;; this prevents the player moving past edge, but if it's a door
                   ;; trigger seperate code to move to new room
    
    jp z, updateRestOfScreen   
    ld (playerXPos), a
    
    
    
    ld hl, (currentPlayerLocation)    
    inc hl
    ld (currentPlayerLocation), hl     
  
    ld a, KEYBOARD_READ_PORT_SPACE_TO_B			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 0, a						    ; SPACE
    jp z, doFireMissile

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V			
    in a, (KEYBOARD_READ_PORT)					; read from io port		
    bit 1, a						    ; Z
    jp z, doFireMissile         

    jp updateRestOfScreen 
    
doFireMissile      ; triggered when jump key pressed just sets the       
    ld a, (MissileInFlightFlag)
    cp 1
    jp z, skipLaunchMissile
    ;; we first need to work out where the missiles should fire from based on current player location
    ;; unless we're in power up mode then just fires form middle of nose of ship
    
    ;; in power up mode we'll fire from nose and wing tips :) (note: not yet implemented) 
    ld hl, (currentPlayerLocation)
    ld de, -264  
    add hl, de    
    ld (currentMissilePosition), hl
    ;;; setup the missile "Time To Live"  (like ethernet TTL right :)
    ld a, MISSILE_COUNTDOWN_INIT
    ld (missileCountDown), a
    ld a, 1
    ld (MissileInFlightFlag), a
          

skipLaunchMissile
updateRestOfScreen 
    
    ld hl, (playerSpritePointer)    
    ld de, (currentPlayerLocation)
    ld c, 8
    ld b, 8    
    call drawSprite
    
    ld hl, jollyRoger
    ld de, (jollyRogerLocation)
    ld c, 8
    ld b, 8    
    call drawSprite    
    call updateJollyRoger    
    
    ld a, (MissileInFlightFlag)
    cp 0
    jp z, skipMissileDraw
    
    ld hl, missileData
    ld de, (currentMissilePosition)        
    ld c, 8
    ld b, 8    
    call drawSprite
       
    call updateMissilePosition
skipMissileDraw
    
    jp gameLoop
    
updateMissilePosition
      ld a, (missileCountDown)
      dec a
      cp 0
      jp z, noMissileUpClearMissile
      
      ld (missileCountDown), a
      
      ld hl, (currentMissilePosition)    
      ld de, -33
      add hl, de
      ld (currentMissilePosition), hl
      jr noMissileUpdate
noMissileUpClearMissile
      xor a
      ld (MissileInFlightFlag), a 
noMissileUpdate      
      ret
      
updateJollyRoger   
    ld a, (jollyRogerXPos)        
    cp 23  
    jr z, reverseDirToNeg
    cp 1
    jr z, reverseDirToPos
        
    jr endOfUpdateJollyRoger    
    
reverseDirToNeg
    ld hl, -1 
    ld (jollyRogerDirUpdate), hl
    jr endOfUpdateJollyRoger 
    
reverseDirToPos    
    ld hl, 1 
    ld (jollyRogerDirUpdate), hl
    jr endOfUpdateJollyRoger 
    
endOfUpdateJollyRoger    
    
    ld hl, (jollyRogerLocation)
    ld (previousJollyRogerLocation), hl
    ld de, (jollyRogerDirUpdate)
    add hl, de
    ld (jollyRogerLocation), hl

    ld hl, (jollyRogerDirUpdate)    
    ld a, (jollyRogerXPos)
    add a, l
    ld (jollyRogerXPos), a 
    
    ret
    

;;;; sprite code
;;;; our sprites are custom 8 by 8 charactor blocks - so will look fairly big (maybe too big)
;;;; the generic routines will look at an area of memory stored in hl before the call

;;;; on the zx81 each block is 2 "pixels" horizontally and 2 vertically pre encoded in the sprite memory
;;;; size of sprite in memory using bit pack is 16 * 16 = 256bits ==>>> 32bytes


;;; hl = start of sprite memory
;;; de = offset position in screen memory top left of sprite - no limit check done (yet)
;;; c  = width of sprite (normally 8 to keep things "simple")
;;; b  = rows in sprite (normally 8 to keep things "simple")
drawSprite         
    push bc    
    push de
    ld b, 0               ;; just doing columns in c so zero b
    ldir                  ;; ldir repeats ld (de), (hl) until bc = 0 and increments hl and de
    pop de
    ex de, hl    
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite    
    ret


;;; work in progrerss currently crashes - 
;; if this could be made to work then the platforms would appear in blank bits of sprite
;; which would made game play better
drawSprite_OR_BACKGROUND         
    push bc    
    push de
    
    ld b, c    ; get column loop counter in b 
drawSprite_OR_ColLoop
    ld a, (hl)
    inc hl
    or d
    or e
    ld (de), a
    inc de
    djnz drawSprite_OR_ColLoop

    pop de
    ex de, hl    
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite_OR_BACKGROUND    
    ret  
    
; printLives
    ; ld bc, 46
    ; ld de, LivesText
    ; call printstring
    
    ; ld a, (playerLives)
    ; ld de, 51    
    ; call print_number8bits        
    ; ret
    

      
; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    push de ; preserve de
    ld hl,Display
    add hl,bc	
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end	
    pop de  ; preserve de
    ret  
    
print_number16bits    ; bc stores the 16bits, print b then c, de stores offset from Display
    ld a, b
    call print_number8bits
    ld a, c
    inc de  ; move de over by 2
    inc de
    call print_number8bits
    ret

    
print_number8bits
    ld hl, (DF_CC)    
    add hl, de    
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a  
    
    ret

printNumber
    ld hl,Display
    add hl,bc	
printNumber_loop
    ld a,(de)
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a      
    ret  
    

;check if TV synchro (FRAMES) happend
vsync	
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync   
endOfVsync        
	ret

    
                DEFB $76                        ; Newline        
Line1End
Line2			DEFB $00,$14
                DEFW Line2End-Line2Text
Line2Text     	DEFB $F9,$D4                    ; RAND USR
				DEFB $1D,$22,$21,$1D,$20        ; 16514                
                DEFB $7E                        ; Number
                DEFB $8F,$01,$04,$00,$00        ; Numeric encoding
                DEFB $76                        ; Newline
Line2End            
endBasic
                                                                
Display        	DEFB $76                                                 				
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76                     
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           
                DEFB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76           

Variables: 

playerSpriteData
     ; DEFB   $00, $00, $00, $85, $05, $00, $00, $00, $00, $00, $00, $81, commented out but nice space fighhter ship
     ; DEFB	$82, $00, $00, $00, $00, $00, $00, $05, $85, $00, $00, $00,
     ; DEFB	$00, $00, $85, $80, $80, $05, $00, $00, $05, $87, $80, $80,
     ; DEFB	$80, $80, $04, $85, $82, $80, $80, $82, $81, $80, $80, $81,
     ; DEFB	$07, $03, $84, $82, $81, $07, $03, $84, $00, $00, $00, $84,
     ; DEFB	$07, $00, $00, $00     
     DEFB	$00, $00, $00, $00, $81, $04, $00, $00, $00, $00, $00, $06,
     DEFB	$85, $00, $00, $00, $00, $00, $06, $87, $80, $82, $00, $00,
     DEFB	$00, $06, $87, $80, $80, $80, $82, $00, $06, $00, $03, $03,
     DEFB	$84, $00, $87, $83, $03, $82, $07, $03, $03, $03, $84, $80,
     DEFB	$00, $02, $04, $01, $01, $01, $86, $01, $00, $00, $02, $80,
     DEFB	$80, $80, $01, $00   
missileData     
     DEFB	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
     DEFB	$00, $00, $00, $00, $00, $00, $00, $87, $04, $00, $00, $00,
     DEFB	$00, $00, $00, $02, $01, $00, $00, $00, $00, $00, $00, $00,
     DEFB	$00, $00, $00, $00, $00, $00, $00, $85, $05, $00, $00, $00,
     DEFB	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
     DEFB	$00, $00, $00, $00     
jollyRoger     
     DEFB	$87, $03, $00, $00, $00, $00, $03, $04, $05, $86, $00, $83,
     DEFB	$83, $00, $06, $85, $00, $00, $06, $04, $87, $86, $00, $00,
     DEFB	$00, $00, $05, $87, $04, $85, $00, $00, $00, $00, $02, $83,
     DEFB	$83, $01, $00, $00, $00, $00, $04, $07, $84, $87, $00, $00,
     DEFB	$05, $06, $00, $01, $02, $00, $86, $85, $02, $83, $00, $00,
     DEFB	$00, $00, $83, $01     

; used to clear current location before move    
blankSprite
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0    
    DEFB   0,  0,  0,  0,  0,  0,  0,  0
    DEFB   0,  0,  0,  0,  0,  0,  0,  0      
    DEFB   8,  8,  0,  0,  0,  0,  0,  0      
blockFilled    ;8*10
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8
    DEFB   8,  8,  8,  8,  8,  8,  8,  8     
    DEFB   8,  8,  8,  8,  8,  8,  8,  8    

playerXPos
    DEFB 0

enemySpriteZeroPos_ST  
    DEFW 0
enemySpriteOnePos_ST    
    DEFW 0
enemySpriteZeroPos_END
    DEFW 0
enemySpriteOnePos_END   
    DEFW 0
enemySpriteZeroPos_DIR
    DEFW 0
enemySpriteOnePos_DIR  
    DEFW 0
enemySpriteZeroPos_CUR
    DEFW 0
enemySpriteOnePos_CUR
    DEFW 0
enemySpriteZeroPos_RATE
    DEFB 0
enemySpriteOnePos_RATE
    DEFB 0    
TEMP_enemySpritePointer
    DEFW 0
TEMP_enemySpritePos_CUR
    DEFW 0
enemySpriteZero_HorizVert
    DEFB 0
enemySpriteOne_HorizVert    
    DEFB 0
TEMP_enemySpriteFrame
    DEFB 0
enemySpriteFrameZero
    DEFB 0
enemySpriteFrameOne    
    DEFB 0
enemySprites   ;; keeping these to 4*4 for speed and size
enemySprite4by4BlankPointer
    DEFW 0
YSpeed   
    DEFB 0
currentPlayerLocation 
    DEFW 0
MissileInFlightFlag
    DEFB 0
missileCountDown
    DEFB 0
currentMissilePosition    
    DEFW 0
enemySprite4by4Blank
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0
    DEFB 0, 0, 0 ,0, 0, 0 
deadPlayerSpritePointer
    DEFW 0
playerSpritePointer
    DEFW 0 

jollyRogerDirUpdate
    DEFW 1
jollyRogerXPos
    DEFB 0
jollyRogerLocation
    DEFW 0
previousJollyRogerLocation    
    DEFW 0
evenOddLoopCount
    DEFB 0
gameOverRestartFlag    
    DEFB 0    
LivesText
    DEFB _L,_I,_V,_E,_S,_EQ,$ff    
TopLineText
    DEFB _S,_P,_A,_C,_E,__,_O,_U,_T,_V, _A, _D, _E, _R, _S, $ff

title_screen_txt
	DEFB	_Z,_X,_8,_1,__,_S,_P,_A,_C,_E,_O,_U,_T,_V,_A,_D,_E,_R,_S,$ff
keys_screen_txt_1
	DEFB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_O,__,_L,_E,_F,_T,26,__,_P,__,_R,_I,_G,_H,_T,$ff
keys_screen_txt_2
	DEFB	__,__,__,__,__,__,__,_Z,__,_O,_R,__,_S,_P,_A,_C,_E,__,_EQ,__,_F,_I,_R,_E,,$ff    

game_objective_txt
	DEFB	_T,_O,__,_W,_I,_N,__,_S,_U,_R,_V,_I,_V,_E,__, _A,_L,_L,__,_A,_L,_I,_E,_N,_S,$ff
	
last_Score_txt
	DEFB 21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff	
high_Score_txt
	DEFB 21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff		
credits_and_version_1
	DEFB __,_B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,__, _2,_0,_2,_4,$ff
credits_and_version_2
	DEFB __,__,_V,_E,_R,_S,_I,_O,_N,__,_V,_0,_DT,_0,_DT,_1,$ff    
credits_and_version_3
	DEFB __,__,__,_Y,_O,_U,_T,_U,_B,_E,_CL, _B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,$ff       
    
   
VariablesEnd:   DEFB $80
BasicEnd: 
#END
