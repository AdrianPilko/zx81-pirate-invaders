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
;;; 1) sometimes the pirates don't move for a short time, either on level restart or new level.
;;;    after a delay they start moving again. The rest of the gaem loop appears to be working
;;;    correctly as you can still move left right and fire (and hit) the pirates?????
;;; 2) the high score doesn't register properly for number >= 100  FIXED
;;; 3) untested - what happens when the level goes past 8 (the fastest left rights pirate movement)
;;; 4) if you are shooting then die and game over that next game you immediately lose a live

;;; Potential Improvements (+extra features)
;;; 1) If the pirates second row reaches the players mid ship then the player loses
;;;    a life even if that last row was all shot, improve but making it possible to
;;;    only die if the bottom row is shot and the top row pirates moved down to players ship
;;; 2) When both rows of pirates are shot on left or right edges then the whole remaining
;;;    "block" of pirates should be able to move to the screen limits not just carry on as if
;;;    the edge pirates were still extant.
;;; 3) pirates should be able to fire back down
;;; 4) End scene with skull and cross bones - the sprite data has been designed and in the code
;;;    just no game logic to set it or do something special with it

;some #defines for compatibility with other assemblers
;pasmo only accepts DEFINE
CLS EQU $0A2A


;DEFINE DEBUG_PRINT_PIRATE_CYCLE
;DEFINE DEBUG_PIRATE_DIR
;DEFINE DEBUG_NO_MOVE_PIRATE  1
;DEFINE DEBUG_START_PIRATES_LOWER

KEYBOARD_READ_PORT_P_TO_Y	EQU $DF
; for start key
KEYBOARD_READ_PORT_A_TO_G	EQU $FD
; keyboard port for shift key to v
KEYBOARD_READ_PORT_SHIFT_TO_V EQU $FE
; keyboard space to b
KEYBOARD_READ_PORT_SPACE_TO_B EQU $7F
; keyboard q to t
KEYBOARD_READ_PORT_Q_TO_T EQU $FB

; starting port numbner for keyboard, is same as first port for shift to v
KEYBOARD_READ_PORT EQU $FE
SCREEN_WIDTH EQU 32
SCREEN_HEIGHT EQU 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
MISSILE_COUNTDOWN_INIT EQU 18
;#define PLAYER_START_POS 604
PLAYER_START_POS EQU 637
PLAYER_LIVES EQU 3
;PIRATE_START_POS EQU 366
PIRATE_START_POS EQU 36


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

VERSN
    DB 0
E_PPC:
    DW 2
D_FILE:
    DW Display
DF_CC:
    DW Display+1                  ; First character of display
VARS:
    DW Variables
DEST:           DW 0
E_LINE:         DW BasicEnd
CH_ADD:         DW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DW 0
STKBOT:         DW BasicEnd+5
STKEND:         DW BasicEnd+5                 ; Empty stack
BREG:           DB 0
MEM:            DW MEMBOT
UNUSED1:        DB 0
DF_SZ:          DB 2
S_TOP:          DW $0002                      ; Top program line number
LAST_K:         DW $fdbf
DEBOUN:         DB 15
MARGIN:         DB 55
NXTLIN:         DW Line2                      ; Next line address
OLDPPC:         DW 0
FLAGX:          DB 0
STRLEN:         DW 0
T_ADDR:         DW $0c8d
SEED:           DW 0
FRAMES:         DW $f5a3
COORDS:         DW 0
PR_CC:          DB $bc
S_POSN:         DW $1821
CDFLAG:         DB $40
MEMBOT:         DB 0,0 ;  zeros
UNUNSED2:       DW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DB $00,$0a                    ; Line 10
                DW Line1End-Line1Text         ; Line 10 length
Line1Text:      DB $ea                        ; REM



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry poitn to the code ships the memory definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

introWaitLoop
	ld b,64
introWaitLoop_1
    push bc
    pop bc
	djnz introWaitLoop_1
    jp read_start_key_1     ;; have to have 2 labels as not a call return

secondIntroWaitLoop

    ld b, 64
introWaitLoop_2
    push bc
    pop bc
    djnz introWaitLoop_2

	jp read_start_key_2

intro_title
	call CLS  ; clears screen and sets the boarder
;    ld a, (gameOverRestartFlag)
;    cp 1
;    call z, gameOverDeathScene


    xor a
    ld (goNextLevelFlag),a
    ld (restartLevelFlag),a
    ld (gameOverRestartFlag), a
    ld (last_score_mem_tens),a
    ld (last_score_mem_hund),a
    ld (sharkPosX), a
    ld (sharkValid), a
    ld (sharkBonusCountUp), a

    ;ld a, $00
    xor a
    ld (score_mem_tens),a
    ;ld a, $00
    ld (score_mem_hund),a


	ld bc,6
	ld de,title_screen_txt
	call printstring
	ld bc,6+33
	ld de,title_screen_txt
	call printstring
    ld bc,6+66
	ld de,title_screen_txt
	call printstring
	ld bc,202
	ld de,keys_screen_txt_1
	call printstring

    ld bc,235
	ld de,keys_screen_txt_2
	call printstring



	ld bc,301
	ld de,game_objective_boarder
	call printstring
	ld bc,334
	ld de,game_objective_txt
	call printstring
	ld bc,367
	ld de,game_objective_boarder
	call printstring

	ld bc,436
	ld de,high_Score_txt
	call printstring

    ld bc, 476
    ;ld de, last_score_mem_hund ; load address of hundreds
    ld de, high_score_hund
	call printNumber
	ld bc, 478			; bc is offset from start of display
	;ld de, last_score_mem_tens ; load address of  tens
	ld de, high_score_tens
	call printNumber
	ld bc,537
	ld de,credits_and_version_1
	call printstring
	ld bc,569
	ld de,credits_and_version_2
	call printstring
	ld bc,634
	ld de,credits_and_version_3
	call printstring
    ld de, 529
    ld hl, Display+1
    add hl, de
    ex de, hl
    ld hl, playerSpriteData
    ld c, 8
    ld b, 8
    call drawSprite



read_start_key_1
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, secondIntroWaitLoop
    ;; else drop into preinit then initVariables
    jr preinit

read_start_key_2
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop
    jr preinit  ; not really necessary


preinit
;; initialise variables that are once per game load/start
    call CLS

initVariables
    ld a, 0
    ld (bossLevelFlag), a

    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring

    xor a
    ld a, (MissileInFlightFlag)
    ld (evenOddLoopFlag), a    ; used for multi rate enemies and other things
    ld (nextPirateToFireIndex), a
    ld (restartLevelFlag), a

    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl
    call resetJollyRogerPos


    ld hl, 1
    ld (pirateDirUpdate),hl
    ld a, 5
    ld (pirateXPos),a

    ld a, PLAYER_LIVES
    ld (playerLives), a

    ld a, 4
    ld (levelCountDown), a

    ld a, $01
    daa
    ld (gameLevel), a

    xor a
    ld (gameOverRestartFlag), a
    ld a, (score_mem_tens)
    ld (last_score_mem_tens),a
    ld a, (score_mem_hund)
    ld (last_score_mem_hund),a



    ld hl, Display+1
    ld de, PIRATE_START_POS
    add hl, de
    ld (pirateTopLeftPosition), hl
    xor a
    ld (pirateSpriteCycleCount), a
    ;ld hl, pirate3sprites
    ld hl, pirate3sprites4x4
    ld (pirateSpritesPointer), hl
    ld hl, 1
    ld (pirateDirUpdate), hl
    ld a, $ff   ; every pirate is alive
    ;ld a, $01   ; for test only bottom right pirate is alive
    ;ld a, $80   ; for test only top left pirate is alive
    ;ld a, $55   ; for test every other pirate is alive
    ld (pirateValidBitMap), a

    xor a
    ld (goNextLevelFlag), a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gameLoop    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld b,VSYNCLOOP
waitForTVSync
	call vsync
	djnz waitForTVSync

    ld a, (levelCountDown)
    ld b, a
    ld a, (evenOddLoopCount)
    inc a
    cp b
    jr z, resetEvenOddAndSetFlag
    ld (evenOddLoopCount), a
    xor a
    ld (evenOddLoopFlag), a    ; used for multi rate enemies
    jr continueWithGameLoop

resetEvenOddAndSetFlag
    xor a
    ld (evenOddLoopCount), a
    ld a, 1
    ld (evenOddLoopFlag), a    ; used for multi rate enemies

continueWithGameLoop


    ld a, (goNextLevelFlag)
    cp 1
    call z, executeNextLevelStart

    ld a, (restartLevelFlag)
    cp 1
    call z, executeRestartLevel



    ld a, (gameOverRestartFlag)
    cp 1
    jp z, intro_title


    ld a, (evenOddLoopFlag)
    cp 0
    jr z, skipSharkInGameLoop

    ld a, (sharkValid)
    cp 1
    call z, drawSharkBonus
skipSharkInGameLoop
    call setRandomPirateToShoot   ; this sets nextPirateToFireIndex

    ld a, (bossLevelFlag)
    cp 0
    call z, drawMainInvaderGrid

    call pirateFire   ; this will use and nextPirateToFireIndex also check pirateFiringFlag


    ld de, (currentPlayerLocation)
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite


    ld a, (bossLevelFlag)
    cp 0
    jp z, noJollyRogerDraw
    ld hl, blankSprite
    ld de, (previousJollyRogerLocation)
    ld c, 8
    ld b, 8
    call drawSprite

    call updateJollyRoger
    ld hl, jollyRoger
    ld de, (jollyRogerLocation)
    ld c, 8
    ld b, 8
    call drawSprite

noJollyRogerDraw




    call printLivesAndScore

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
    ld de, -31
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
    ld b, 4
    call drawSprite

    ld a, (MissileInFlightFlag)
    cp 0
    jp z, skipMissileDraw

    ld hl, (currentMissilePosition)
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite

    ld hl, missileData
    ld de, (currentMissilePosition)
    ld c, 4
    ld b, 4
    call drawSprite

    call checkIfMissileHit

    call updateMissilePosition
skipMissileDraw

    jp gameLoop


pirateFire
    ret


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


updatePirateXPos
    ld a, (pirateXPos)
    cp 14
    jr z, reversePirateDirToNeg
    cp 3
    jr z, reversePirateDirToPos

    jr endOfUpdatePirateXPos

reversePirateDirToNeg
    ld a, (sharkBonusCountUp)
    inc a
    ld (sharkBonusCountUp), a
    cp 2
    jr z, triggerShark
    jr notriggerShark
triggerShark
    xor a
    ld (sharkBonusCountUp), a
    ld a, 24
    ld (sharkPosX), a
    ld a, 1
    ld (sharkValid), a

notriggerShark
    ld hl, -1
    ld (pirateDirUpdate), hl
    ;; also shove down one row
    ;before we do that we need to blank the line where the pirates "heads" were
    ld de, (pirateTopLeftPosition)
    ld hl, blankSprite
    ld c, 16
    ld b, 1
    call drawSprite
    ld hl, (pirateTopLeftPosition)
    ld de, 165
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 16
    ld b, 1
    call drawSprite
    ;; finally move one row down
    ld hl, (pirateTopLeftPosition)
    ld de, 33
    add hl, de
    ld (pirateTopLeftPosition),hl

    jr endOfUpdatePirateXPos

reversePirateDirToPos
    ld hl, 1
    ld (pirateDirUpdate), hl
    ;; also shove down one row
    ;before we do that we need to blank the line where the pirates "heads" were
    ld de, (pirateTopLeftPosition)
    ld hl, blankSprite
    ld c, 16
    ld b, 1
    call drawSprite
    ;; and blank the middle bit between the rows of pirates
    ld hl, (pirateTopLeftPosition)
    ld de, 165
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 16
    ld b, 1
    call drawSprite

    ;; finally move one row down
    ld hl, (pirateTopLeftPosition)
    ld de, 33
    add hl, de
    ld (pirateTopLeftPosition),hl
    jr endOfUpdatePirateXPos

endOfUpdatePirateXPos
;IF DEFINED DEBUG_PIRATE_DIR
;    ld a,(pirateXPos)
;    ld de, 760
;    call print_number8bits
;ENDIF
    ld hl, (pirateTopLeftPosition)
    ;ld (previousPirateLocation), hl
    ld de, (pirateDirUpdate)
    add hl, de
    ld (pirateTopLeftPosition), hl

    ld hl, (pirateDirUpdate)
    ld a, (pirateXPos)
    add a, l
    ld (pirateXPos), a

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
    ld de, 33
    ld hl, (jollyRogerLocation)
    add hl, de
    ld (jollyRogerLocation), hl
    ld a, (jollyRogerYPosCountDown)
    dec a
    cp 0
    jr z, setEndOfLevelFlagJR

    ld (jollyRogerYPosCountDown), a
    jr endOfUpdateJollyRoger
setEndOfLevelFlagJR
    ld a, 1
    ld (goNextLevelFlag),a
    ld hl, 1
    ld (pirateDirUpdate),hl
    xor a
    ld (bossLevelFlag),a

    ld a, 1
    ld (evenOddLoopFlag),a

    ld a, (playerLives)
    dec a
    cp 0
    jr z, setGameOverFlagJR
    ld (playerLives), a
    call resetJollyRogerPos
    ret
setGameOverFlagJR
    ld a, 1
    ld (gameOverRestartFlag), a
    call resetJollyRogerPos
    ret

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

blankToLAndROfInvader
    ld hl, (pirateTopLeftPosition)
    ld de, -1
    add hl, de
    ex de, hl
    ld hl, enemySprite5by8Blank
    ld c, 1
    ld b, 9
    call drawSprite
    ld hl, (pirateTopLeftPosition)
    ld de, 16
    add hl, de
    ex de, hl
    ld hl, enemySprite5by8Blank
    ld c, 1
    ld b, 9
    call drawSprite
   ret

drawMainInvaderGrid
    ;; first check if any piratres are left
    ld b, $ff     ; set a (and then next line b to all ones)
    ld a, (pirateValidBitMap)
    and b
    jr z, setWaveComplete
    jr checkIfPlayerHitPirates
setWaveComplete
    ld a, 1
    ld (goNextLevelFlag), a
    ret

checkIfPlayerHitPirates
    ;; second check if the bottom pirate has reached the lowest line
    ;; if so restart the level and decrease score by 1

    ;; TODO first version will only check the top left most has reach low point
    ;; need logic to check if any bottom row pirates left if so let it go lower
    ld hl, (pirateTopLeftPosition)
    ld (pirateRowLeftPositionTemp), hl
    ld hl, Display+1
    ld de, $018e   ; $018e is the offset to the lowest row the pirates should be able to get
    add hl, de
    ex de, hl
    ld hl, (pirateRowLeftPositionTemp) ;; reload hl with pirateRowLeftPositionTemp
    ld a, h
    cp d
    jr z, checkNextPirateLowest
    jr continueDrawPirates
checkNextPirateLowest
    ld a, l
    cp e
    jr z, pirateReachedLowest
    jr continueDrawPirates
pirateReachedLowest
    ld a, 1
    ld (restartLevelFlag), a
    jp updatePirateSpriteCycle

continueDrawPirates
;; we have an area of memory which will represent flags for if each of the grid of 5 rows of
;; 5 columnsn invaders is valid (ie not been killed). This code will loop round that and
;; display an invader sprite if required
    call blankToLAndROfInvader
    ld b, 2
    ld hl, (pirateTopLeftPosition)
    ld (pirateRowLeftPositionTemp), hl
    ld a, $80    ; setup a moving bit mask which we'll use to determine if the pirate is shot or not
    ld (pirateValidBitMapMaskTemp), a


pirateRowDrawLoop

   push bc

        ld b, 4
pirateColDrawLoop
            push bc
                ;; put some logic here to determine if the pirate was shot or not

                ld a, (pirateValidBitMapMaskTemp)
                ld b, a
                ld a, (pirateValidBitMap)
                and b
                push af
                rr b
                ld a, b
                ld (pirateValidBitMapMaskTemp),a
                pop af
                jr z, skipDrawThisPirate


                ld de, (pirateRowLeftPositionTemp)
                ld hl, (pirateSpritesPointer)
                ld c, 4
                ;ld b, 8
                ld b, 4
                call drawSprite
                jr continueWithPirateLoop
skipDrawThisPirate
                ;; but draw a blank
                ld de, (pirateRowLeftPositionTemp)
                ld hl, blankSprite
                ld c, 4
                ;ld b, 8
                ld b, 4
                call drawSprite
continueWithPirateLoop
                ld hl, 4
                ld de, (pirateRowLeftPositionTemp)
                add hl, de
                ld (pirateRowLeftPositionTemp), hl
            pop bc
            djnz pirateColDrawLoop

            ld hl, (pirateTopLeftPosition)
            ld de, 165
            add hl, de
            ld (pirateRowLeftPositionTemp), hl

   pop bc
   djnz pirateRowDrawLoop

   ld a, (evenOddLoopFlag)
   cp 1
   jr z, updatePirateSpriteCycle

   ;ld a,1
   ;ld de, 760
   ;call print_number8bits

   jr endOfPirateSpriteUpdate
   ; update the sprite to draw from the 3 cycles
updatePirateSpriteCycle

   ;ld a,2
   ;ld de, 760
   ;call print_number8bits

   ; update X position and reverse direction if reached end limits
   call updatePirateXPos

   ld a, (pirateSpriteCycleCount)
   inc a
   cp 2
   jr z, resetPirateSprite
   ld (pirateSpriteCycleCount), a
   ld hl, (pirateSpritesPointer)
   ld de, 32
   add hl, de
   ld (pirateSpritesPointer), hl
   ld a, (pirateSpriteCycleCount)     ;; currentPlayerLocation is already offset to
IF DEFINED DEBUG_PRINT_PIRATE_CYCLE
   ld de, 1
   call print_number8bits
ENDIF
   jr endOfPirateSpriteUpdate

resetPirateSprite
   xor a
   ld (pirateSpriteCycleCount), a
   ;ld hl, pirate3sprites
   ld hl, pirate3sprites4x4
   ld (pirateSpritesPointer), hl

endOfPirateSpriteUpdate
   ret

;; check if missile hit pirates


checkIfMissileHit
;;;; check if missile hit the shark, if the shark is valid
    ld a, (sharkValid)
    cp 0
    jr z, skipCheckSharkHit
    ld de, (currentMissilePosition)
    ld hl, (sharkAbsoluteScreenPos)
    sbc hl, de
    ld a, h
    cp 0
    jp nz, skipCheckSharkHit
    ld a, l
    cp 0
    jp nz, skipCheckSharkHit

    ; shark hit
    xor a
    ld (sharkValid), a
    ld b, 10
increaseScoreSharkHitLoop
    push bc
    call increaseScore
    pop bc
    djnz increaseScoreSharkHitLoop


skipCheckSharkHit


    ld a, (bossLevelFlag)
    cp 0
    jr z, skipCheckBossHit

    ; code for jolly roger hit detect

    ld de, (jollyRogerLocation)
    ld hl, 36
    add hl, de
    ex de, hl
    ld hl, (currentMissilePosition)

    ; now compare upper and lower bytes of hl and de
    ld a, h
    cp d
    jr z, checkNextRegMissileHit
    jr noHitMissileBoss
checkNextRegMissileHit
    ld a, l
    cp e
    jr z, MissileHitBoss
    jr noHitMissileBoss
MissileHitBoss
    ld b, 100
incScoreBossHitLoop
    push bc
       call increaseScore
    pop bc
    djnz incScoreBossHitLoop
    ld a, 1
    ld (goNextLevelFlag), a
    xor a
    ld (bossLevelFlag),a
    call resetJollyRogerPos

    ret
noHitMissileBoss
    ret

skipCheckBossHit
    ld hl, (pirateTopLeftPosition)

    ld (pirateRowLeftPositionTemp), hl
    ;becasue the whole loop is setup to count down, and because we want to check the
    ; lower row first we need to move the "Tope left position to be the bottom right
    ld de, 177
    add hl, de
    ld (pirateRowLeftPositionTemp), hl  ; this now has bottom right pirate

    ; setup a moving bit mask which we'll use to determine if the pirate
    ; is shot or not. this is basically all ones except the top bit is zero,
    ; this will get rotated round in the loop and used to and with the pirateValidBitMap
    ld a, $fe
    ld (pirateValidBitMapMaskTemp), a

    ; this is used to and with the current mask to check if missile collision check is needed
    ld a, $01
    ld (bitsetMaskPirateTemp), a
    ld b, 8
missileCheckHitLoop
    push bc
        ;; check if we even need to check this pirate, if not valid then skip
        ld a, (bitsetMaskPirateTemp)
        ld b, a
        ld a, (pirateValidBitMap)
        and b
        jr z, noHitMissile

        ld de, (pirateRowLeftPositionTemp)
        ld hl, (currentMissilePosition)

        ; now compare upper and lower bytes of hl and de
        ld a, h
        cp d
        jr z, checkNextPirateMissileHit
        jr noHitMissile
checkNextPirateMissileHit
        ld a, l
        cp e
        jr z, MissileHitPirate
        jr noHitMissile
MissileHitPirate
        ;; missile/cannon HIT!!!

        ld a, (pirateValidBitMapMaskTemp)
        ld b, a
        ld a, (pirateValidBitMap)
        and b
        ld (pirateValidBitMap), a

        ;also if we have hit then disable the missile now!!
        xor a
        ld (MissileInFlightFlag), a
        ld hl, 0
        ld (currentMissilePosition), hl
        call increaseScore
        pop bc   ; have to do this becasue we're exiting early out of loop

        ;; let's draw an explosion and tombstone breifly
        ld b, 3
        ld hl, explsion4x4
explosionDrawLoop
        push bc
            push hl
                ld de, (pirateRowLeftPositionTemp)
                ld c, 4
                ld b, 4
                call drawSprite
                ld b, 32
explosionDelayLoop
                push bc
                ld b, 64
explosionDelayLoop2

                    djnz explosionDelayLoop2

                pop bc
                djnz explosionDelayLoop
            pop hl
            ld de, 16
            add hl, de
        pop bc
        djnz explosionDrawLoop


        ret ;; exit early
noHitMissile
        ;; update mask which is the only bit not set we check next
        ;; e.g second pirate is 0x10111111
        ld a, (pirateValidBitMapMaskTemp)
        ;rra
        rlc a
        ld (pirateValidBitMapMaskTemp),a

        ;; update the mask which is the bit we're setting set all others z80
        ;; e.g second pirate is 0x01000000
        ld a, (bitsetMaskPirateTemp)
        ;rra
        rlc a
        ld (bitsetMaskPirateTemp), a

        ; now move the position to compare (ie a pirate)
        ld de, -4
        ld hl, (pirateRowLeftPositionTemp)
        add hl, de
        ld (pirateRowLeftPositionTemp), hl
    pop bc
        ld a, b  ; check the loop counter, if it's 3 then move the whole lot down by +33-16
        cp 5
        jr nz, endLoopLabelPriateCheck
        ld de, -149

        add hl, de
        ld (pirateRowLeftPositionTemp), hl

endLoopLabelPriateCheck

    ;djnz missileCheckHitLoop
    ld a, b
    dec a
    ld b, a
    cp 0
    jp nz, missileCheckHitLoop
    ret


executeRestartLevel
    call CLS
    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring
    call printLivesAndScore

    ld a, 1
    ld (evenOddLoopFlag),a
    ;; drew player death animation
    ld hl, playerHitSprite
    ld (deadPlayerSpritePointer), hl
    ld b,5
playerDeathLoop
    push bc
        ld de, (currentPlayerLocation)
        ld hl, (deadPlayerSpritePointer)
        push hl
            ld c, 8
            ld b, 4
            call drawSprite
        pop hl
        ld de, 32
        add hl, de
        ld (deadPlayerSpritePointer), hl

        ld b, 64
playerDeathDelayLoop
        push bc
        ld b, 255
playerDeathDelayLoop2
            djnz playerDeathDelayLoop2
        pop bc
        djnz playerDeathDelayLoop
    pop bc
    djnz playerDeathLoop

        ld b, 255
playerDeathDelayLoop3
        push bc
        ld b, 255
playerDeathDelayLoop4
            djnz playerDeathDelayLoop4
        pop bc
        djnz playerDeathDelayLoop3


    call CLS
    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring
    call printLivesAndScore

    ld a, (playerLives)
    dec a
    cp 0
    jr z, setGameOverFlag
    ld (playerLives), a
    jr skipGameOverFlagSet
setGameOverFlag
    ld a, 1
    ld (gameOverRestartFlag), a
    ret
skipGameOverFlagSet
    xor a
    ld a, (MissileInFlightFlag)
    ld (evenOddLoopFlag), a    ; used for multi rate enemies and other things

    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    ld hl, 1
    ld (pirateDirUpdate),hl
    ld a, 5
    ld (pirateXPos),a


    ld hl, Display+1
    ld de, PIRATE_START_POS
    add hl, de
    ld (pirateTopLeftPosition), hl
    xor a
    ld (pirateSpriteCycleCount), a
    ;ld hl, pirate3sprites
    ld hl, pirate3sprites4x4
    ld (pirateSpritesPointer), hl
    ld hl, 1
    ld (pirateDirUpdate), hl
    ld a, $ff   ; every pirate is alive
    ;ld a, $01   ; for test only bottom right pirate is alive
    ;ld a, $80   ; for test only top left pirate is alive
    ;ld a, $55   ; for test every other pirate is alive
    ld (pirateValidBitMap), a

    xor a
    ld (restartLevelFlag), a
    ld (sharkValid), a
    ld (sharkBonusCountUp), a
    ret


executeNextLevelStart
    call CLS
    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring

    ld a, 1
    ld (evenOddLoopFlag),a

    ld a, (gameLevel)
    inc a
    daa     ; convert to binary coded decimal to ease the display
    ld (gameLevel), a

    ld a, (levelCountDown)
    dec a
    cp 1
    jr z, setBossLevelFlag
    ;; could use this to start end end of level boss for now just hold at 1
    ld (levelCountDown), a
    jr continueGampLoop
setBossLevelFlag
    ld a, 1
    ld (bossLevelFlag), a
    ld a, 4
    ld a, (levelCountDown)
continueGampLoop
    xor a
    ld a, (MissileInFlightFlag)
    ld (evenOddLoopFlag), a    ; used for multi rate enemies and other things

    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    ld hl, 1
    ld (pirateDirUpdate),hl
    ld a, 5
    ld (pirateXPos),a


    ld hl, Display+1
    ld de, PIRATE_START_POS
    add hl, de
    ld (pirateTopLeftPosition), hl
    xor a
    ld (pirateSpriteCycleCount), a
    ;ld hl, pirate3sprites
    ld hl, pirate3sprites4x4
    ld (pirateSpritesPointer), hl
    ld hl, 1
    ld (pirateDirUpdate), hl
    ld a, $ff   ; every pirate is alive
    ;ld a, $01   ; for test only bottom right pirate is alive
    ;ld a, $80   ; for test only top left pirate is alive
    ;ld a, $55   ; for test every other pirate is alive
    ld (pirateValidBitMap), a

    xor a
    ld (goNextLevelFlag), a
    ld (sharkValid), a
    ld (sharkBonusCountUp), a
    ret

checkForSharkHit
    ret

drawSharkBonus
    xor a
    ld d, a
    ld a, (sharkPosX)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ld (sharkAbsoluteScreenPos), hl
    ex de, hl
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite


    ld a, (sharkPosX)
    dec a
    cp 1
    jr z, noDrawSharkAndSetInvalid
    ld (sharkPosX), a
    xor a
    ld d, a
    ld a, (sharkPosX)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, sharkBonusSprite
    ld c, 8
    ld b, 4
    call drawSprite
    jr endDrawSharkBonus
noDrawSharkAndSetInvalid
    xor a
    ld (sharkValid), a
    ld a, 1
    ld (sharkPosX), a
    xor a
    ld d, a
    ld a, (sharkPosX)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite


endDrawSharkBonus
    ret

resetJollyRogerPos
    ; reset the jolly roger for next boss level time
    ld hl, Display+1
    ld de, 39
    add hl, de
    ld (jollyRogerLocation), hl
    ld hl, 1
    ld (jollyRogerDirUpdate),hl
    ld a, 5
    ld (jollyRogerXPos),a
    ld a, 12
    ld (jollyRogerYPosCountDown),a
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

printLivesAndScore
    ld a, (playerLives)
    ld de, 29
    call print_number8bits

    ld bc, 11
    ld de, score_mem_tens
    call printNumber

    ld bc, 9
    ld de, score_mem_hund
    call printNumber

    ld a, (gameLevel)
    ld de, 20
    call print_number8bits

    ret

increaseScore
    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
	add a,1
	daa									; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_tens),a
	cp 153
	jr z, addOneToHund
	jr skipAddHund
addOneToHund
	ld a, 0
	ld (score_mem_tens), a
    ld a, (score_mem_hund)
	add a, 1
	daa                                   ; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_hund), a
skipAddHund

; compare with high score and set that if higher

    ld a, (score_mem_hund)
    ld b,a     ; load the second 8-bit number into register b (via a)
    ld a, (high_score_hund)   ; load the first 8-bit number into register a
    cp b            ; compare a with the second 8-bit number (in register b)
    jr c, setHighScore ; jump if carry flag is set (high_score_hund < score_mem_hund)

    ; check if equal, and if so then check the tens, could be 00 50, or 01 50 in high score and current score
    jr z, highScoreHundEqualCheckTens
    ; high_score_hund > score_mem_hund   so don't set
    jr skipCheckRestHighScore

highScoreHundEqualCheckTens
    ld a, (score_mem_tens)
    ld b, a
    ld a, (high_score_tens)
    cp b
    jp c, setHighScore ; jump if carry flag is set (a < b)

    jr skipCheckRestHighScore

setHighScore
    ld a, (score_mem_tens)
    ld (high_score_tens), a
    ld a, (score_mem_hund)
    ld (high_score_hund), a
    jr skipCheckRestHighScore

skipCheckRestHighScore
    ret

setRandomPirateToShoot
tryAnotherRCol                          ; generate random number between 0 and 3 inclusive
    ld a, r
    and %00000011
    cp 4
    jp nc, tryAnotherRCol               ; loop when nc flag set ie not less than 4 again
    inc a                               ; inc guarntees range 1 to 30 for col
    ld (nextPirateToFireIndex), a
    ret


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


                DB $76                        ; Newline
Line1End
Line2			DB $00,$14
                DW Line2End-Line2Text
Line2Text     	DB $F9,$D4                    ; RAND USR
				DB $1D,$22,$21,$1D,$20        ; 16514
                DB $7E                        ; Number
                DB $8F,$01,$04,$00,$00        ; Numeric encoding
                DB $76                        ; Newline
Line2End
endBasic

Display        	DB $76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76

Variables
playerSpriteData
     DB $00, $00, $81, $00, $00, $81, $04, $00, $00, $06, $85, $00
     DB $00, $85, $00, $00, $81, $83, $81, $83, $82, $81, $81, $80
     DB $00, $02, $81, $81, $81, $81, $82, $01, $00, $00, $00, $00
     DB $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     DB $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	 DB $00, $00, $00, $00
missileData
    ;; small cannon ball
    DB $00, $00, $00, $00, $00, $81, $86, $00, $00, $84, $07, $00
	DB $00, $00, $00, $00
sharkBonusSprite    ; 96 bytes , 8x4 characters times 3 frames
	DB $00, $00, $87, $80, $00, $00, $00, $87, $83, $07, $80, $80
	DB $80, $83, $83, $01, $80, $80, $80, $80, $80, $80, $03, $04
	DB $00, $03, $00, $02, $04, $00, $00, $02, $00, $00, $87, $80
	DB $00, $00, $00, $87, $83, $07, $80, $80, $80, $83, $83, $07
	DB $02, $80, $80, $80, $80, $80, $03, $82, $03, $03, $00, $84
	DB $00, $00, $00, $02, $00, $00, $87, $80, $00, $00, $00, $83
	DB $83, $80, $80, $80, $80, $83, $83, $05, $81, $80, $80, $80
	DB $80, $80, $03, $05, $00, $03, $00, $86, $00, $00, $00, $03


explsion4x4     ;4x4 and 5 frames total of animation (80bytes)
	DB $87, $87, $01, $04, $04, $04, $87, $87, $00, $06, $86, $00
	DB $87, $05, $02, $00, $01, $00, $02, $02, $04, $04, $00, $86
	DB $87, $00, $87, $00, $87, $01, $00, $01, $00, $85, $05, $00
	DB $85, $80, $80, $05, $00, $85, $05, $00, $00, $85, $05, $00
	DB $00, $85, $05, $00, $85, $80, $80, $05, $00, $85, $05, $00
	DB $00, $85, $05, $00, $00, $85, $05, $00, $85, $80, $80, $05
	DB $00, $85, $05, $00, $00, $85, $05, $00
jollyRoger
     DB	$87, $03, $00, $00, $00, $00, $03, $04, $05, $86, $00, $83
     DB	$83, $00, $06, $85, $00, $00, $06, $04, $87, $86, $00, $00
     DB	$00, $00, $05, $87, $04, $85, $00, $00, $00, $00, $02, $83
     DB	$83, $01, $00, $00, $00, $00, $04, $07, $84, $87, $00, $00
     DB	$05, $06, $00, $01, $02, $00, $86, $85, $02, $83, $00, $00
     DB	$00, $00, $83, $01

playerHitSprite        ; 16x8 "pixels" 8x4 characters (bytes) times 4 frames animation
    DB $04, $04, $83, $02, $87, $02, $00, $02, $04, $00, $87, $00
    DB $00, $01, $02, $00, $87, $87, $02, $87, $05, $06, $81, $86
    DB $00, $02, $81, $81, $81, $81, $82, $01, $01, $01, $04, $02
    DB $00, $00, $00, $87, $04, $87, $00, $00, $00, $01, $87, $00
    DB $87, $87, $00, $00, $00, $00, $00, $00, $00, $00, $06, $00
    DB $00, $01, $00, $87, $00, $01, $00, $01, $00, $87, $00, $87
    DB $00, $00, $00, $00, $00, $00, $00, $00, $04, $00, $00, $00
    DB $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00
    DB $07, $84, $85, $03, $04, $07, $86, $85, $05, $85, $85, $00
    DB $05, $05, $85, $85, $07, $84, $85, $03, $04, $07, $86, $02
    DB $01, $02, $02, $00, $01, $01, $02, $02, $00, $80, $00, $00
    DB $00, $85, $05, $00, $00, $00, $85, $84, $84, $00, $00, $00
    DB $00, $00, $06, $82, $07, $04, $00, $00, $00, $80, $00, $01
	DB $01, $85, $05, $00

pirate3sprites     ;; these are 4 by 8 bytes and is 3 in the animation = 96bytes
    DB $00, $84, $07, $85, $87, $81, $82, $06, $05, $80, $80, $00
    DB $01, $07, $84, $00, $87, $05, $85, $00, $00, $00, $02, $00
    DB $00, $00, $00, $00, $00, $00, $00, $00, $00, $84, $07, $00
    DB $87, $81, $82, $04, $05, $80, $80, $85, $01, $07, $84, $02
    DB $00, $05, $85, $00, $02, $01, $02, $00, $00, $00, $00, $00
    DB $00, $00, $00, $00, $05, $84, $07, $00, $86, $81, $82, $04
	DB $00, $80, $80, $85, $00, $07, $84, $02, $00, $05, $85, $00
	DB $02, $01, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00
pirate3sprites4x4       ; these are 16 bytes each 4 by 4)
	DB $05, $85, $05, $00, $02, $80, $80, $86, $00, $07, $84, $02
	DB $87, $05, $85, $00, $04, $85, $05, $00, $02, $80, $80, $84
	DB $00, $07, $84, $00, $02, $01, $85, $00, $00, $85, $05, $87
	DB $06, $80, $80, $01, $01, $07, $84, $00, $87, $05, $85, $00

; used to clear current location before move
blankSprite
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   8,  8,  0,  0,  0,  0,  0,  0
blockFilled    ;8*10
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8

pirateValidBitMap ;we've fixed on 4x2 grid of pirates so thats 8 bits to store if they are dead or not
    DB 0
nextPirateToFireIndex
    DB 0
pirateFiringFlag
    DB 0
playerXPos
    DB 0
evenOddLoopFlag
    DB 0
evenOddLoopCount
    DB 0
enemySpriteZeroPos_ST
    DW 0
enemySpriteOnePos_ST
    DW 0
enemySpriteZeroPos_END
    DW 0
enemySpriteOnePos_END
    DW 0
enemySpriteZeroPos_DIR
    DW 0
enemySpriteOnePos_DIR
    DW 0
enemySpriteZeroPos_CUR
    DW 0
enemySpriteOnePos_CUR
    DW 0
enemySpriteZeroPos_RATE
    DB 0
enemySpriteOnePos_RATE
    DB 0
TEMP_enemySpritePointer
    DW 0
TEMP_enemySpritePos_CUR
    DW 0
enemySpriteZero_HorizVert
    DB 0
enemySpriteOne_HorizVert
    DB 0
TEMP_enemySpriteFrame
    DB 0
enemySpriteFrameZero
    DB 0
enemySpriteFrameOne
    DB 0
enemySprites   ;; keeping these to 4*4 for speed and size
enemySprite4by4BlankPointer
    DW 0
YSpeed
    DB 0
currentPlayerLocation
    DW 0
MissileInFlightFlag
    DB 0
missileCountDown
    DB 0
currentMissilePosition
    DW 0
levelCountDown
    DB 0
gameLevel
    DB 0
restartLevelFlag
    DB 0
enemySprite5by8Blank
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
bossLevelFlag
    DB 0
sharkPosX
    DB 0
sharkAbsoluteScreenPos
    DW 0
sharkValid
    DB 0
sharkBonusCountUp
    DB 0
deadPlayerSpritePointer
    DW 0
playerSpritePointer
    DW 0
pirateTopLeftPosition
    DW 0
pirateRowLeftPositionTemp
    DW 0
pirateValidBitMapMaskTemp
    DB 0
bitsetMaskPirateTemp
    DB 0
pirateSpriteCycleCount
    DB 0
pirateSpritesPointer
    DW 0
pirateDirUpdate
    DW 1
pirateXPos
    DB 0
playerLives
    DB 0
score_mem_tens
    DB 0
score_mem_hund
    DB 0
high_score_tens
    DB 0
high_score_hund
    DB 0
last_score_mem_tens
    DB 0
last_score_mem_hund
    DB 0
jollyRogerDirUpdate
    DW 1
jollyRogerXPos
    DB 0
jollyRogerLocation
    DW 0
jollyRogerYPosCountDown
    DB 0
previousJollyRogerLocation
    DW 0
gameOverRestartFlag
    DB 0
goNextLevelFlag
    DB 0

LivesText
    DB _L,_I,_V,_E,_S,_EQ,$ff
TopLineText
    DB _S,_C,_O,_R,_E,_CL,__,__,__,__,__,__,__,_L,_E,_V,_E,_L,_CL,__,__,__,_L,_I,_V,_E,_S,_CL,__,__,__,$ff

title_screen_txt
	DB	_Z,_X,_8,_1,__,_P,_I,_R,_A,_T,_E,__,_I,_N,_V,_A,_D,_E,_R,_S,$ff
keys_screen_txt_1
	DB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_O,__,_L,_E,_F,_T,26,_P,__,_R,_I,_G,_H,_T,$ff
keys_screen_txt_2
	DB	__,__,__,__,__,__,__,_Z,__,_O,_R,__,_S,_P,_A,_C,_E,__,_EQ,__,_F,_I,_R,_E,$ff

game_objective_txt
	DB	_T,_O,__,_W,_I,_N,__,_S,_U,_R,_V,_I,_V,_E,__, _A,_L,_L,__,_P,_I,_R,_A,_T,_E,_S,$ff
game_objective_boarder   ; I know a bit wasteful but we have a whole 16K!
	DB	137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,$ff

last_Score_txt
	DB 21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff
high_Score_txt
	DB 21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff
credits_and_version_1
	DB __,_B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,__, _2,_0,_2,_4,$ff
credits_and_version_2
	DB __,__,_V,_E,_R,_S,_I,_O,_N,__,_V,_0,_DT,_5,_DT,_3,$ff
credits_and_version_3
	DB __,__,__,_Y,_O,_U,_T,_U,_B,_E,_CL, _B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,$ff


VariablesEnd:   DB $80
BasicEnd:


