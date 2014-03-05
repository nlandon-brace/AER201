   ; with ADC and Servos, 2014
   ;saved as final code v2.0
   ; updated March 5 2014
      list p=16f877                 ; list directive to define processor
      #include <p16f877.inc>        ; processor specific variable definitions
      __CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


    cblock  0x20
        COUNTH
        COUNTM
        COUNTL
        Table_Counter
        lcd_d1
        lcd_d2
        com
        dat
        optime
        optime_final
        lights_total
        option_temp
        stats_temp
        stats1
        stats2
        stats3
        stats4
        stats5
        stats6
        stats7
        stats8
        stats9
        count
        ones
        tens
        huns
        binary_num
        w_temp
        status_temp
        count38
        adc_delay
        Tray_CheckCounter
        LastStableState
        data_points
        voltage_refh
        voltage_refl
        voltage_temph
        voltage_templ
        led_on_flag
        flicker_flag

    endc

    ;Declare constants for pin assignments (LCD on PORTD)
        #define RS      PORTD,2
        #define E       PORTD,3
        #define STEPA   PORTC, 0
        #define STEPB   PORTC, 1
        #define STEPC   PORTD, 4
        #define STEPD   PORTC, 3
        #define TRAYPORT PORTD, 0
        #define MUX0    PORTA, 1
        #define MUX1    PORTA, 2
        #define MUX2    PORTA, 3
        #define MUX3    PORTA, 4
        #define MUXE    PORTA, 5
        #define IRMUX   PORTD, 1
        #define LIGHT_IN PORTA, 5

         ORG    0x0000     ;RESET vector must always be at 0x00
         goto   init       ;Just jump to the main code section.

         ORG    0x0004
         goto   INTERRUPT_THINGS
         
;***************************************
; Look up table
;***************************************

Welcome_Msg1
        addwf   PCL,F
        dt      "Welcome!", 0
Welcome_Msg2
        addwf   PCL,F
        dt      "Press * to Start",0

OpMessage
        addwf   PCL,F
        dt      "Checking...",0

End_Message1
        addwf   PCL,F
        dt      "DONE! A: Op. Time, B: # of Lights",0

End_Message2
        addwf   PCL, F
        dt      "C: Light Info, D: Return to Standby",0

Op_time1
        addwf   PCL,F
        dt      " seconds",0

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
        addwf   PCL, F
        dt      " - Pass", 0

Flicker_Fail
        addwf   PCL, F
        dt      " - Flicker Fail",0

LED_Fail
        addwf   PCL, F
        dt      " - LED Fail", 0

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
Display macro   Message
        local   loop_
        local   end_
        clrf    Table_Counter
        clrw
loop_   movf    Table_Counter,W
        call    Message
        xorlw   B'00000000' ;check WORK reg to see if 0 is returned
        btfsc   STATUS,Z
            goto    end_
        call    WR_DATA
        incf    Table_Counter,F
        goto    loop_
end_
        endm


bank0   macro
        bcf STATUS, RP0
        bcf STATUS, RP1
        endm

bank1   macro
        bsf STATUS, RP0
        bcf STATUS, RP1
        endm

bank2   macro
        bcf STATUS, RP0
        bsf STATUS, RP1
        endm

bank3   macro
        bsf STATUS, RP0
        bsf STATUS, RP1
        endm

;***************************************
; Initialize LCD
;***************************************
init
        bsf       INTCON, GIE
        bsf       INTCON, 5
        bcf       INTCON, 2

        bank1
        movlw     b'00000001'
        movwf     TRISA
    ;    clrf      TRISA          ; All port A is output
        movlw     b'11110110'    ; Set required keypad inputs
        movwf     TRISB
        clrf      TRISC          ; All port C is output
        movlw     b'00000011'
        movwf     TRISD
;        clrf      TRISD          ; All port D is output

        bank0
        clrf      PORTA
        clrf      PORTB
        clrf      PORTC
        clrf      PORTD

        call      InitADC
        call      InitLCD        ;Initialize the LCD (code in lcd.asm; imported by lcd.inc)
        call      SERVO_NEUTRAL
;***************************************
; Main code
;***************************************
STANDBY_DISPLAY
        call        ClrLCD
        Display     Welcome_Msg1    ;Display line 1 of the welcome message
        call        Switch_Lines    ; Switch lines
        Display     Welcome_Msg2    ; Display line 2 of the welcome message

test
        btfss   PORTB,1             ; Check for input from the keypad
        goto    $-1     ; if no input, keep displaying the welcome message

        swapf   PORTB, W            ;When input is detected, read it in to W
        andlw   0x0F                ;Sample Code put this here...???
        xorlw   b'00001100'         ; Check to see if it is the 12th key (*)
        btfss   STATUS,Z            ; If status Z goes to 0, it is the 12th key, skip
        goto    test     ; If it's not *, keep displaying the main message

        btfsc   PORTB,1             ;Wait for key to be released
        goto    $-1                 ;
        goto    STARTER

STARTER
        call    ClrLCD              ;Clear the LCD for the new message
        Display OpMessage           ;Display the operation message
        ;all temporary - these variables will be loaded during BEGIN_OPERATION
        movlw   d'0'
        movwf   optime

        movlw   d'0'
        movwf   lights_total

        movlw   d'0'
        movwf   stats1

        movlw   d'0'
        movwf   stats2

        movlw   d'0'
        movwf   stats3

        banksel stats4
        movlw   d'0'
        movwf   stats4

         banksel stats5
         movlw   d'0'
         movwf   stats5

         banksel stats6
         movlw   d'0'
         movwf   stats6

         banksel stats7
         movlw   d'0'
         movwf   stats7

         banksel stats8
         movlw   d'0'
         movwf   stats8

         banksel stats9
         movlw  d'0'
         movwf  stats9

         bank0
       ;call    BEGIN_OPERATION     ;Call the operation

; Temporary countdown (while there is no operation)
COUNTDOWN
        call    HalfS
        call    HalfS
        call    BEGIN_OPERATION
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
        movf    optime_final, W   ; move optime into the working register
        movwf   binary_num  ; move working register into binary_num for conversion
        call    BIN2BCD     ; convert binary to ASCII for display
        movf    huns, W     ; display each digit individually
        call    WR_DATA
        movf    tens, W
        call    WR_DATA
        movf    ones, W
        call    WR_DATA
        Display Op_time1       ;Display "seconds"
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
        movwf   option_temp         ;Store the input option
        xorlw   b'00000011'         ; Is it A?
        btfsc  STATUS, Z            ; If not A, check some more
        call    INPUT_LOOP          ; If it is A, stay put.
        movf    option_temp, W      ; Restoring the input option
        xorlw   b'00000111'         ; Is it B?
        btfsc   STATUS, Z
        call    INPUT_LOOP          ; If it's B - wait.
        movf   option_temp, W
        xorlw   b'00001011'         ; If it's C - wait
        btfsc   STATUS, Z
        call    INPUT_LOOP
        movf   option_temp, W       ; Restore to check for D
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
        movf    option_temp,W   ;move the option into W
        xorlw   b'00000000'     ; check to see if 1 was pressed
        btfss   STATUS, Z
        goto    CHECK2          ; if not 1, check 2
        call    ClrLCD          ; if 1, clear the LCD and display
        movf    option_temp, W  ; restore the option
        call    KPBintoNum      ; convert it to a decimal and display on keypad
        call    WR_DATA         ; write to the LCD
        movf    stats1, W       ; move the value stored in stats to the working register
        call    DISPLAY_ERROR   ; check to see which message to display
        ;Display Pass
        call    SUBDISPLAY      ; display the second line of the message
        goto    LIGHTSTATS_ANALYZE  ; return to get upper menu to get new input

; same as CHECK1 except for light 2.
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
        call    DISPLAY_ERROR
       ;Display Flicker_Fail
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
        call    DISPLAY_ERROR
       ;Display LED_Fail
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
        banksel stats4
        movf    stats4, W
        bank0
        call    DISPLAY_ERROR
       ;Display NO_LIGHT
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
        banksel stats5
        movf    stats5, W
        bank0
        call    DISPLAY_ERROR
       ;Display Flicker_Fail
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
        banksel stats6
        movf    stats6, W
        bank0
        call    DISPLAY_ERROR
       ;Display LED_Fail
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
        banksel stats7
        movf    stats7, W
        bank0
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
        banksel stats8
        movf    stats8, W
        bank0
        call    DISPLAY_ERROR
       ;Display Pass
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
        banksel stats9
        movf    stats9, W
        bank0
        call    DISPLAY_ERROR
       ;Display NO_LIGHT
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE


;*****************
; DISPLAY ERROR
; This method determines what state the light is in (0 - Pass, 1 - Flicker Fail etc.)
; and displays the correct message according to the value stored in stats# for that light.
;*****************
DISPLAY_ERROR
        movwf   stats_temp
        xorlw   d'0'
        btfss   STATUS, Z
        goto    CHECK_FLICK
        Display Pass ; If the value stored in status is 0, display Pass
        return
; If the value stored in the status register is 1, display Flicker Fail
CHECK_FLICK
        movf    stats_temp, W
        xorlw   d'1'
        btfss   STATUS, Z
        goto    CHECK_LED
        Display Flicker_Fail
        return
; If the value stored in the status reg is 2, display LED Fail
CHECK_LED
        movf    stats_temp, W
        xorlw   d'2'
        btfss   STATUS, Z
        goto    CHECK_NONE
        Display LED_Fail
        return

; If the value stored in the status register is 3, display N/A
CHECK_NONE
        movf   stats_temp,W
        xorlw   d'3'
        btfss   STATUS, Z
        return
        movlw   " "
        call    WR_DATA
        movlw   "-"
        call    WR_DATA
        movlw   " "
        call    WR_DATA
        movlw   "N"
        call    WR_DATA
        movlw   "/"
        call    WR_DATA
        movlw   "A"
        call    WR_DATA
        return

; Displays D; Return to Main on the second line, waits for input
SUBDISPLAY
        call    Switch_Lines
        Display Return_Message
        btfss   PORTB, 1
        goto    $-1
        return


; Returns to Standby if D is pushed
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
        movlw   B'11000000'
        call    WR_INS
        return

Clear_Display
        movlw   B'00000001'
        call    WR_INS
        return

;***************************************
; Delay 0.5s
;***************************************
HalfS
    local   HalfS_0
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
    movlw   b'00110011'
    call    WR_INS
    call lcdLongDelay
    call lcdLongDelay
    movlw   b'00110010'
    call    WR_INS
    call lcdLongDelay
    call lcdLongDelay

    ; 4 bits, 2 lines, 5x7 dots
    movlw   b'00101000'
    call    WR_INS
    call lcdLongDelay
    call lcdLongDelay

    ; display on/off
    movlw   b'00001100'
    call    WR_INS
    call lcdLongDelay
    call lcdLongDelay

    ; Entry mode
    movlw   b'00000110'
    call    WR_INS
    call lcdLongDelay
    call lcdLongDelay

    ; Clear ram
    movlw   b'00000001'
    call    WR_INS
    call lcdLongDelay
    call lcdLongDelay
    return
    ;************************************

    ;ClrLCD: Clear the LCD display
ClrLCD
    movlw   B'00000001'
    call    WR_INS
    return

    ;****************************************
    ; Write command to LCD - Input : W , output : -
    ;****************************************
WR_INS
    bcf     RS              ;clear RS
    movwf   com             ;W --> com
    andlw   0xF0            ;mask 4 bits MSB w = X0
    movwf   PORTD           ;Send 4 bits MSB
    bsf     E               ;
    call    lcdLongDelay    ;__    __
    bcf     E               ;  |__|
    swapf   com,w
    andlw   0xF0            ;1111 0010
    movwf   PORTD           ;send 4 bits LSB
    bsf     E               ;
    call    lcdLongDelay    ;__    __
    bcf     E               ;  |__|
    call    lcdLongDelay
    return

    ;****************************************
    ; Write data to LCD - Input : W , output : -
    ;****************************************
WR_DATA
    bsf     RS
    movwf   dat
    movf    dat,w
    andlw   0xF0
    addlw   4
    movwf   PORTD
    bsf     E               ;
    call    lcdLongDelay    ;__    __
    bcf     E               ;  |__|
    swapf   dat,w
    andlw   0xF0
    addlw   4
    movwf   PORTD
    bsf     E               ;
    call    lcdLongDelay    ;__    __
    bcf     E               ;  |__|
    return

lcdLongDelay
    movlw d'20'
    movwf lcd_d2
LLD_LOOP
    LCD_DELAY
    decfsz lcd_d2,f
    goto LLD_LOOP
    return


;*********
; BIN2BCD
; Converts a binary number to ASCII
; characters for display on the LCD
; Written by: A. Borowski
; Sourced from: piclist.com --> 8 bit to ASCII Decimal 3 digits
;**********
BIN2BCD
    movlw 8
    movwf count
    clrf huns
    clrf tens
    clrf ones

BCDADD3

    movlw 5
    subwf huns, 0
    btfsc STATUS, C
    CALL ADD3HUNS

    movlw 5
    subwf tens, 0
    btfsc STATUS, C
    CALL ADD3TENS

    movlw 5
    subwf ones, 0
    btfsc STATUS, C
    CALL ADD3ONES

    decf count, 1
    bcf STATUS, C
    rlf binary_num, 1
    rlf ones, 1
    btfsc ones,4 ;
    CALL CARRYONES
    rlf tens, 1

    btfsc tens,4 ;
    CALL CARRYTENS
    rlf huns,1
    bcf STATUS, C

    movf count, 0
    btfss STATUS, Z
    goto BCDADD3

    movf huns, 0 ; add ASCII Offset
    addlw h'30'
    movwf huns

    movf tens, 0 ; add ASCII Offset
    addlw h'30'
    movwf tens

    movf ones, 0 ; add ASCII Offset
    addlw h'30'
    movwf ones
    return

ADD3HUNS
    movlw 3
    addwf huns,1
    return

ADD3TENS
    movlw 3
    addwf tens,1
    return

ADD3ONES
    movlw 3
    addwf ones,1
    return

CARRYONES
    bcf ones, 4
    bsf STATUS, C
    return

CARRYTENS
    bcf tens, 4
    bsf STATUS, C
    return


;**************
; OPERTION CODE
;**************
BEGIN_OPERATION
    movlw     b'11000111'
    banksel   OPTION_REG
    movwf     OPTION_REG
    bank0
    call    CHECK_TRAY
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    HalfS
    call    HalfS
    call    SERVO_ON

CHECK_LED1
    bcf     MUXE
    bcf     MUX0
    bcf     MUX1
    bcf     MUX2
    bcf     MUX3
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats1

CHECK_LED2
    bsf     MUX0
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats2

CHECK_LED3
    bcf     MUX0
    bsf     MUX1
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats3

CHECK_LED4
    bsf     MUX0
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats4

CHECK_LED5
    bcf     MUX0
    bcf     MUX1
    bsf     MUX2
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats5

CHECK_LED6
    bsf     MUX0
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats6

CHECK_LED7
    bsf     MUX1
    bcf     MUX0
    call    LIGHT_TEST
    movwf   stats7

CHECK_LED8
    bsf     MUX0
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats8

CHECK_LED9
    bcf     MUX0
    bcf     MUX1
    bcf     MUX2
    bsf     MUX3
    call    HalfS
    call    HalfS
    call    LIGHT_TEST
    movwf   stats9

    call    SERVO_NEUTRAL
    call    STEPPER_DRIVERREV
    bsf     INTCON, 5
    movf    optime, W
    movwf   optime_final

    return

;***********
; LIGHT TEST
;***********
    
LIGHT_TEST
    btfss   IRMUX
    goto    NOT_THERE
    incf    lights_total
    movlw   b'0'
    movwf   data_points
    movwf   voltage_templ
    movwf   voltage_refl
    movwf   voltage_temph
    movwf   voltage_refh
    movwf   led_on_flag
    movwf   flicker_flag
    goto    ON_TEST
    call    InitADC

NOT_THERE
    movlw   d'3'
    return

ON_TEST
; DIGITAL TEST, FUNCTIONAL
 ;   incf   data_points
 ;   movlw   d'254'
 ;   subwf   data_points, W
 ;   btfsc   STATUS, C
 ;   goto    LED_TEST
 ;   btfss   LIGHT_IN
 ;   goto    ON_TEST

;    incf    voltage_temp
;    goto    ON_TEST

; ADC TEST
    call    ADC_MainLoop
    banksel ADRESL
    movf    ADRESL, W
    bank0
    movwf   voltage_refl
    movf    ADRESH, W
    movwf   voltage_refh

LED_TEST
; DIGITAL TEST, FUNCTIONAL
;    movlw   d'0'
;    movwf   data_points
;    call    HalfS   ;temporary
;    call    HalfS   ;temporary
;    call    HalfS   ;temporary
;    call    HalfS   ;temporary
;    movlw   d'10'
;    subwf   voltage_temp, W
;    btfsc   STATUS, C
;    goto    FLICKER_TEST

;    movlw   d'2'
;    return

; ADC TEST
    call    ADC_MainLoop
    banksel ADRESL
    movf    ADRESL, W
    movwf   voltage_templ
    bank0
    movf    ADRESH, W
    movwf   voltage_temph
   

    call    COMPARE
    btfss   STATUS, C   ; if status C is set, voltage_temp > W (threshold)
    incf    led_on_flag
    
    incf    data_points
    movlw   d'254'
    subwf   data_points, W
    btfss   STATUS, C
    goto    LED_TEST

    movlw   d'0'
    movwf   data_points
    movlw   d'10'
    subwf   led_on_flag, W
    btfsc   STATUS, C
    goto    FLICKER_TEST
    movlw   d'2'
    return

FLICKER_TEST
; DIGITAL TEST
;    incf   data_points
;    movlw   d'254'
;    subwf   data_points, W
;    btfsc   STATUS, C
;    goto    PASS
;    btfss   LIGHT_IN
;    incf    voltage_ref
;    call   ADC_Delay
;    call    ADC_Delay
;    call    ADC_Delay
;    call    ADC_Delay
;    call    ADC_Delay
;    call    ADC_Delay
;    goto    FLICKER_TEST

;   ADC TEST
    call    ADC_MainLoop
    banksel ADRESL
    movf    ADRESL, W
    movwf   voltage_templ
    bank0
    movf    ADRESH, W
    movwf   voltage_temph
    call    COMPARE_REF_NEW
    btfsc   STATUS, C
    call    SWAP_VOLTAGES

;16 bit subtraction, voltage_temp = voltage_temp - voltage_reference
;http://www.piclist.com/techref/microchip/math/sub/16bb.htm
 
    movf    voltage_refl, W
    subwf   voltage_templ
    movf    voltage_refh, W
    btfss   STATUS, C
    incfsz  voltage_refh, W
    subwf   voltage_temph

    call    COMPARE
    btfss   STATUS, C
    incf    flicker_flag

    incf    data_points
    movlw   d'254'
    subwf   data_points, W
    btfss   STATUS, C
    goto    FLICKER_TEST
    
    movlw   d'10'
    subwf   flicker_flag, W
    btfsc   STATUS, C
    goto    PASS
    movlw   d'1'
    return

PASS
    movlw   d'0'
    return

SWAP_VOLTAGES
    movf    voltage_templ, W
    movwf   w_temp
    movf    voltage_refl, W
    movwf   voltage_templ
    movf    w_temp, W
    movwf   voltage_refl

    movf    voltage_temph, W
    movwf   w_temp
    movf    voltage_refh, W
    movwf   voltage_temph
    movf    w_temp, W
    movwf   voltage_refh
    return

COMPARE
    movlw   d'000'
    movwf   w_temp
    movf    voltage_temph, W
    subwf   w_temp, W
    btfss   STATUS, Z
    return
    movlw   d'100'
    movwf   w_temp
    movf    voltage_templ, W
    subwf   w_temp, W ; subtract the temp from 0.2 V.
; If voltage > 0.2 V, C = 0
; If voltage <= 0.2 V, C = 1
    return


COMPARE_REF_NEW
;    movf    voltage_temph, W ; temp is Y
;    xorlw   0x80
;    movwf   w_temp
;    movf    voltage_refh, W ; ref is X
;    xorlw   0x80
;    subwf   w_temp, W
;    goto    EQUAL2
    movf    voltage_temph, W
    subwf   voltage_refh, W
EQUAL2
    btfss   STATUS, Z
    return
    movf    voltage_templ, W
    subwf   voltage_refl, W

; if temp > ref, status C = 1
; if ref > temp, status C = 0
    return

; DIGITAL TEST, FUNCTIONAL
;    movlw   d'10'
;   subwf   voltage_ref, W
;    btfsc   STATUS, C
;    goto    SET_PASS
;    movlw   d'1'
;    return

;SET_PASS
;    movlw   d'0'
;    return


;*********
; STEPPER MOTOR DRIVING THINGS
; *********
STEPPER_DRIVERFOR
;    bsf     STEPA
;    bcf     STEPB
;    bcf     STEPC
;    bcf     STEPD
;    call    HalfS
    bsf     STEPA
    call    HalfS
    bcf     STEPA
    bsf     STEPC
    call    HalfS
    bcf     STEPC
    bsf     STEPB
    call    HalfS
    bcf     STEPB
    bsf     STEPD
    call    HalfS
    bcf     STEPD
    return

STEPPER_DRIVERREV
    bsf     STEPD
    call    HalfS
    bcf     STEPD
    call    HalfS
    bsf     STEPB
    call    HalfS
    bcf     STEPB
    call    HalfS
    bsf     STEPC
    call    HalfS
    bcf     STEPC
    call    HalfS
    bsf     STEPA
    call    HalfS
    bcf     STEPA
    call    HalfS
    return

;***************
; SERVO MOTORS
;***************
SERVO_NEUTRAL
    bank1
    movlw   b'10000000'
    movwf   PR2
    bank0
    movlw   b'00001100'
    movwf   CCP1CON
    movlw   b'00000110'
    movwf   T2CON
    movlw   b'01001000'
    movwf   CCPR1L
    clrf    TMR2
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    return

SERVO_ON
    movlw   b'01010010'
    movwf   CCPR1L
    clrf    TMR2
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    return

;****************
; INTERRUPT THINGS
; ****************
INTERRUPT_THINGS
    movwf  w_temp
    movf   STATUS, W
    movwf  status_temp
    btfsc  INTCON, 2
    call   TIMER_ISR
    movf   status_temp, W
    movwf  STATUS
    swapf  w_temp, f
    swapf  w_temp, w
    retfie

TIMER_ISR
    bcf     INTCON, 2
    decfsz  count38, f
    return
    incf    optime
    movlw   d'38'
    movwf   count38
    return


;**************
; ADC THINGS
;**************

InitADC
    bank1
    movlw   b'10001110'
    movwf   ADCON1
    bank0
    movlw   b'11000101'
    movwf   ADCON0
    return

ADC_MainLoop
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    bsf     ADCON0, GO
WAIT
    btfsc   ADCON0, GO
    goto    WAIT
    bank0
;    call    ADC_Delay
;    call    ADC_Delay
;    call    ADC_Delay
;    call    ADC_Delay
    return
 
ADC_Delay
    movlw   0xFF
    movwf   adc_delay
    decfsz  adc_delay, f
    goto    $-1
    nop
    return

;**************
; CHECK TRAY
;
; Debounce the signal coming from the contact switches to ensure that
; the tray is placed correctly so that operation can begin
;**************

CHECK_TRAY
    movlw   D'1'
    movwf   LastStableState ;assume that the switch is up
    clrf    Tray_CheckCounter

CHECK_TRAY_LOOP
    clrw
    btfsc   LastStableState, 0
    goto    CHECK_TRAY_DOWN

CHECK_TRAY_UP
    btfsc   TRAYPORT
    incf    Tray_CheckCounter, W
    goto    END_CHECK_TRAY

CHECK_TRAY_DOWN
    btfss   TRAYPORT
    incf    Tray_CheckCounter, W

END_CHECK_TRAY
    movwf   Tray_CheckCounter
    xorlw   d'5'
    btfss   STATUS, Z
    goto    Delay1ms

TRAY_CHECKED
    comf    LastStableState, f
    clrf    Tray_CheckCounter
    btfsc   LastStableState, 0
    goto    Delay1ms
    return

Delay1ms
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    goto    CHECK_TRAY_LOOP

    END



