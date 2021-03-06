      ; Feb 1, 2014
      ; UPDATES: Subroutine for second half of light info display
      ; Inactivates A, B, C, * and # during light stats display
      list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


	cblock	0x70
		COUNTH
		COUNTM
		COUNTL
		Table_Counter
		lcd_tmp
		lcd_d1
		lcd_d2
		com
		dat
        optime
        lights_total
        option_temp
        stats1
        stats2
        stats3
        stats4
        stats5
        stats6
        stats7
        stats8
        stats9
        stats_temp
        countdown
 	endc

	;Declare constants for pin assignments (LCD on PORTD)
		#define	RS 	PORTD,2
		#define	E 	PORTD,3

         ORG       0x0000     ;RESET vector must always be at 0x00
         goto      init       ;Just jump to the main code section.

;***************************************
; Look up table
;***************************************

Welcome_Msg1
		addwf	PCL,F
		dt		"Welcome!", 0
Welcome_Msg2
        addwf   PCL,F
        dt      "Press * to Start",0

OpMessage
        addwf   PCL,F
        dt      "Checking...",0

End_Message1
		addwf	PCL,F
		dt		"DONE! A: Op. Time, B: # of Lights",0

End_Message2
        addwf   PCL, F
        dt      "C: Light Info, D: Return to Standby",0

Op_time1
        addwf   PCL,F
        dt      "90 seconds",0

Return_Message
        addwf   PCL,F
        dt      "D: Back to Main",0

Lights_Tested
        addwf   PCL, F
        dt      " lights tested",0

Stats
        addwf   PCL, F
        dt      "Press 1-9 for light info",0

LBintoNum
        addwf   PCL, F
        dt      "0123456789"

KPBintoNum
        addwf   PCL,F
        dt      "123 456 789"

Pass
        addwf   PCL,F
        dt      " - Pass",0

LED_Fail
        addwf   PCL, F
        dt      " - LED Fail",0

Flicker_Fail
        addwf   PCL, F
        dt      " - Flicker Fail",0

NO_LIGHT
        addwf   PCL, F
        dt      " - N/A",0

;***************************************
; Delay: ~160us macro
;***************************************
LCD_DELAY macro
	movlw   0xFF
	movwf   lcd_d1
	decfsz  lcd_d1,f
	goto    $-1
	endm


;***************************************
; Display macro
;***************************************
Display macro	Message
		local	loop_
		local 	end_
		clrf	Table_Counter
		clrw
loop_	movf	Table_Counter,W
		call 	Message
		xorlw	B'00000000' ;check WORK reg to see if 0 is returned
		btfsc	STATUS,Z
			goto	end_
		call	WR_DATA
		incf	Table_Counter,F
		goto	loop_
end_
		endm



;***************************************
; Initialize LCD
;***************************************
init
         clrf      INTCON         ; No interrupts

         bsf       STATUS,RP0     ; select bank 1
         clrf      TRISA          ; All port A is output
         movlw     b'11110010'    ; Set required keypad inputs
         movwf     TRISB
         clrf      TRISC          ; All port C is output
         clrf      TRISD          ; All port D is output

         bcf       STATUS,RP0     ; select bank 0
         clrf      PORTA
         clrf      PORTB
         clrf      PORTC
         clrf      PORTD

         call      InitLCD  	  ;Initialize the LCD (code in lcd.asm; imported by lcd.inc)

;***************************************
; Main code
;***************************************
STANDBY_DISPLAY
        call        ClrLCD
    	Display		Welcome_Msg1    ;Display line 1 of the welcome message
		call		Switch_Lines    ; Switch lines
		Display		Welcome_Msg2    ; Display line 2 of the welcome message

test    
        btfss   PORTB,1             ; Check for input from the keypad
        goto    $-1     ; if no input, keep displaying the welcome message

        swapf   PORTB, W            ;When input is detected, read it in to W
        andlw   0x0F                ;Sample Code put this here...???
        xorlw   b'00001100'         ; Check to see if it is the 12th key (*)
        btfss   STATUS,Z            ; If status Z goes to 0, it is the 12th key, skip
        goto    STANDBY_DISPLAY     ; If it's not *, keep displaying the main message

        btfsc   PORTB,1             ;Wait for key to be released
        goto    $-1                 ;
        goto    STARTER

STARTER
        call    ClrLCD              ;Clear the LCD for the new message
        Display OpMessage           ;Display the operation message
        movlw   d'4'
        movwf   lights_total
        movlw   d'0'
        movwf   stats1
        movlw   d'1'
        movwf   stats2
        movlw   d'2'
        movwf   stats3
        movlw   d'3'
        movwf   stats4

       ;call    BEGIN_OPERATION     ;Call the operation

; Temporary countdown (while there is no operation)
COUNTDOWN
        call    HalfS
        call    HalfS
        call    ClrLCD
        movlw   "3"
        call    WR_DATA
        call    HalfS
        call    HalfS
        call    ClrLCD
        movlw   "2"
        call    WR_DATA
        call    HalfS
        call    HalfS
        call    ClrLCD
        movlw   "1"
        call    WR_DATA
        call    HalfS
        call    HalfS
        call    ClrLCD
;Temporary Countdown Ends

;Ending Menu Begins
ENDING
        call    ClrLCD  ;Clear the LCD to make space for the new message
        Display End_Message1    ;Display the first line of the end message
        call    Switch_Lines    ;Switch Lines
        Display End_Message2    ;Display the second line of the end message

END_DISPLAY                     ; Shifts the ending messages to the left
        movlw   b'00011000'
        call    WR_INS
        call    HalfS

        btfss   PORTB,1             ; Check for input from the keypad
        goto    END_DISPLAY         ; if no input, keep displaying the end message

        swapf   PORTB, W            ;When input is detected, read it in to W
        andlw   0x0F                
        goto    OPTIONA             ; When the input is detected, see if it's for A
        goto    END_DISPLAY
        
;        btfsc   PORTB,1             ;Wait for key to be released
;        goto    $-1                 ;

;        call    ClrLCD
        ;goto    $

OPTIONA
        movwf   option_temp
        xorlw   b'00000011'         ; Check to see if it is the 3rd key (A)
        btfss   STATUS,Z            ; If status Z goes to 0, it is the 3rd key, skip
        goto    OPTIONB         ; If not check if it's B
        call    ClrLCD              ; If it is, clear and display!

OPTIME
        Display Op_time1        ;Display time required for operation
        call    Switch_Lines
        Display Return_Message  ;D: Return

OPTIME_RETURN
        btfss   PORTB,1             ; Check for input from the keypad
        goto    $-1         ; if no input, keep displaying the message

        swapf   PORTB, W            ;When input is detected, read it in to W
        andlw   0x0F                
        xorlw   b'00001111'         ;Check to see if it's D
        btfss   STATUS, Z           ; If it's D skip
        goto    OPTIME_RETURN       ; If it's not D, wait.

        btfsc   PORTB,1             ;Wait for key to be released
        goto    $-1                 ;
        goto    ENDING



OPTIONB
        movf    option_temp, W
        xorlw   b'00000111'         ; Check to see if it is the 7th key (B)
        btfss   STATUS,Z            ; If status Z goes to 0, it is the 7th key, skip
        goto    OPTIONC         ; If not keep rotating
        call    ClrLCD              ; If it is, clear and display!

TESTED
        movf    lights_total, W     ;move the number of lights (var) into W
        call    LBintoNum           ;convert the number of lights (binary) into decimal
        call    WR_DATA             ; display the number on the LCD
        Display Lights_Tested       ; Display "_ lights tested"
        call    Switch_Lines
        Display Return_Message

TESTED_RETURN
        btfss   PORTB,1             ; Check for input from the keypad
        goto    $-1                 ; if no input, keep displaying the end message

        swapf   PORTB, W            ;When input is detected, read it in to W
        andlw   0x0F                ;Sample Code put this here...???
        xorlw   b'00001111'
        btfss   STATUS, Z
        goto    TESTED_RETURN   ; if it's not D, wait.

        btfsc   PORTB,1             ;Wait for key to be released
        goto    $-1                 ;
        goto    ENDING              ; if it is D, go back to the menu





OPTIONC
        movf    option_temp, W
        xorlw   b'0001011'         ; Check to see if it is the 11th key (C)
        btfss   STATUS,Z            ; If status Z goes to 0, it is the 11th key, skip
        goto    OPTIOND         ; If not keep rotating
  
LIGHTSTATS
        call    ClrLCD              ; If it is, clear and display!
        Display Stats
        call    Switch_Lines
        Display Return_Message

LIGHTSTATS_ROTATE
        movlw   b'00011000'
        call    WR_INS
        call    HalfS

LIGHTSTATS_RETURN
        btfss   PORTB,1             ; Check for input from the keypad
        goto    LIGHTSTATS_ROTATE   ; if no input, keep displaying the stats message

LIGHTSTATS_ANALYZE
        swapf   PORTB, W            ;When input is detected, read it in to W
        andlw   0x0F                
        movwf   option_temp
        xorlw   b'00000011'
        btfsc  STATUS, Z
        call    INPUT_LOOP
        movf    option_temp, W
        xorlw   b'00000111'
        btfsc   STATUS, Z
        goto    INPUT_LOOP
        movf   option_temp, W
        xorlw   b'00001011'
        btfsc   STATUS, Z
        goto    INPUT_LOOP
        movf   option_temp, W
        xorlw   b'00001111'         ; Check to see if D was pushed
        btfss   STATUS, Z
        goto    CHECK1  ;If D is not pressed, check if it was a number

        btfsc   PORTB,1             ;Wait for key to be released
        goto    $-1                 ;
        goto    ENDING              ; When key is released, return to Main Menu

INPUT_LOOP
        btfss   PORTB, 1
        goto    $-1
        return
CHECK1
        movf    option_temp,W
        xorlw   b'00000000'
        btfss   STATUS, Z
        goto    CHECK2
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats1, W
        call    DISPLAY_ERROR
        ;Display Pass
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

CHECK2
 
        movf    option_temp,W
        xorlw   b'00000001'
        btfss   STATUS, Z
        goto    CHECK3
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats2, W
        ;call    DISPLAY_ERROR
        Display LED_Fail
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE


CHECK3
        movf    option_temp,W
        xorlw   b'00000010'
        btfss   STATUS, Z
        goto    CHECK4
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats3, W
        Display Flicker_Fail
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE


CHECK4
        movf    option_temp,W
        xorlw   b'00000100'
        btfss   STATUS, Z
        goto    CHECK5
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats4, W
        Display NO_LIGHT
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

CHECK5
        movf    option_temp,W
        xorlw   b'00000101'
        btfss   STATUS, Z
        goto    CHECK6
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats5, W
        Display Flicker_Fail
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

CHECK6
        movf    option_temp,W
        xorlw   b'00000110'
        btfss   STATUS, Z
        goto    CHECK7
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats6, W
        Display NO_LIGHT
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

CHECK7
        movf    option_temp,W
        xorlw   b'00001000'
        btfss   STATUS, Z
        goto    CHECK8
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats7, W
        call    DISPLAY_ERROR
        ;Display Pass
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

CHECK8
        movf    option_temp,W
        xorlw   b'00001001'
        btfss   STATUS, Z
        goto    CHECK9
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats8, W
        Display LED_Fail
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

CHECK9
        movf    option_temp,W
        xorlw   b'00001010'
        btfss   STATUS, Z
        goto    LIGHTSTATS_ANALYZE
        call    ClrLCD
        movf    option_temp, W
        call    KPBintoNum
        call    WR_DATA
        movf    stats9, W
        Display Flicker_Fail
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

DISPLAY_ERROR
        movwf   stats_temp
        xorlw   d'0'
        btfsc   STATUS, Z
        Display Pass
 ;       movf    stats_temp, W
 ;       xorlw   d'1'
 ;       btfsc   STATUS, Z
 ;       Display Flicker_Fail
        return
;FISH
;        movf    stats_temp, W
;        xorlw   d'2'
;        btfsc   STATUS, Z
;        Display Flicker_Fail
;        Display NO_LIGHT
        return

SUBDISPLAY
        call    Switch_Lines
        Display Return_Message
        btfss   PORTB, 1
        goto    $-1
        return
OPTIOND
        movf    option_temp, W
        xorlw   b'00001111'
        btfss   STATUS, Z           ; if it's option D return to start
        goto    END_DISPLAY         ; otherwise stay in the end menu.
        goto    STANDBY_DISPLAY
;***************************************
; LCD control
;***************************************
Switch_Lines
		movlw	B'11000000'
		call	WR_INS
		return

Clear_Display
		movlw	B'00000001'
		call	WR_INS
		return

;***************************************
; Delay 0.5s
;***************************************
HalfS
	local	HalfS_0
      movlw 0x88
      movwf COUNTH
      movlw 0xBD
      movwf COUNTM
      movlw 0x03
      movwf COUNTL

HalfS_0
      decfsz COUNTH, f
      goto   $+2
      decfsz COUNTM, f
      goto   $+2
      decfsz COUNTL, f
      goto   HalfS_0

      goto $+1
      nop
      nop
		return


;******* LCD-related subroutines *******


    ;***********************************
InitLCD
	bcf STATUS,RP0
	bsf E     ;E default high

	;Wait for LCD POR to finish (~15ms)
	call lcdLongDelay
	call lcdLongDelay
	call lcdLongDelay

	;Ensure 8-bit mode first (no way to immediately guarantee 4-bit mode)
	; -> Send b'0011' 3 times
	movlw	b'00110011'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay
	movlw	b'00110010'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; 4 bits, 2 lines, 5x7 dots
	movlw	b'00101000'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; display on/off
	movlw	b'00001100'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; Entry mode
	movlw	b'00000110'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay

	; Clear ram
	movlw	b'00000001'
	call	WR_INS
	call lcdLongDelay
	call lcdLongDelay
	return
    ;************************************

    ;ClrLCD: Clear the LCD display
ClrLCD
	movlw	B'00000001'
	call	WR_INS
    return

    ;****************************************
    ; Write command to LCD - Input : W , output : -
    ;****************************************
WR_INS
	bcf		RS				;clear RS
	movwf	com				;W --> com
	andlw	0xF0			;mask 4 bits MSB w = X0
	movwf	PORTD			;Send 4 bits MSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	call	lcdLongDelay
	return

    ;****************************************
    ; Write data to LCD - Input : W , output : -
    ;****************************************
WR_DATA
	bsf		RS
	movwf	dat
	movf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0
	addlw	4
	movwf	PORTD
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	return

lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return

	END



