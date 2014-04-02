; Things work!
; April 2, 2014
; Changed loop counter on motors for testing purposes

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
        optime  ; operation time for display (gets modified)
        optime_final ; "permanent" operation time (if operation time needs to be viewed multiple times
        lights_total ; total number of lights tested
        option_temp ; used to determine which key was pressed (modifiable)
        stats_temp  ; used to check which state the light is in (modifiable)
        stats1  ; this is the variable which holds the state of light 1 after it is tested (permanent)
        stats2  ; as above, for light 2
        stats3  ; as above, for light 3
        stats4  ; as above, for light 4
        stats5  ; as above, for light 5
        stats6  ; as above, for light 6
        stats7  ; as above, for light 7
        stats8  ; as above, for light 8
        stats9  ; as above, for light 9 
        count   ; used to convert optime to decimal for display
        ones    ; ones digit of the converted binary number
        tens    ; tens digit of the converted binary number
        huns    ; hundreds digit of the converted binary number (hopefully not used)
        binary_num ; move optime to this variable to allow binary --> decimal for display
        w_temp  ; saves the value in the working register
        status_temp ; saves the current state of the status register (for ISR)
        count38 ; used to count to 38 ---> for operation timer
        adc_delay   ; used for acquisition time for ADC
        Tray_CheckCounter   ; Switch Debouncing
        LastStableState ; Switch Debouncing
        data_points ; number of data points collected for the current voltage of the light
        voltage_refh    ; MSBs of the voltage for comparison (first data point collected - to determine flickering)
        voltage_refl    ; LSBs of the voltage for comparison (first data point collected - to determine flickering)
        voltage_temph   ; MSBs of the ADC conversion (current voltage) -- ADRESH
        voltage_templ   ; LSBs of the ADC conversion (current voltage) -- ADRESL
        led_on_flag     ; counts number of times LED is detected to be on (increments each time voltage_temp > On Threshold)
        flicker_flag    ; counts number of times LED is detected to have flickered (voltage_ref - voltage_temp > Flicker Threshold)
        loop_counter    ; counts the number of times the loop has been run - used for testing flickering and shade array movement
    endc

    ;Declare constants for pin assignments (LCD on PORTD)
    ; servos plug in to RC2

        #define RS      PORTD,2
        #define E       PORTD,3
        #define STEPA   PORTC, 0    ;output to stepper motor port A
        #define STEPB   PORTC, 1    ;output to stepper motor port B
        #define STEPC   PORTD, 4    ;output to stepper motor port C
        #define STEPD   PORTC, 3    ;output to stepper motor port D
        #define TRAYPORT PORTD, 0   ;port for contact switch input
        #define MUX0    PORTA, 1    ;A select, mux
        #define MUX1    PORTA, 2    ;B select, mux
        #define MUX2    PORTA, 3    ;C select, mux
        #define MUX3    PORTA, 4    ;D select, mux
        #define MUXE    PORTA, 5    ;Enable, mux
        #define IRMUX   PORTD, 1    ; Input from IR mux (digital)
        #define IR_POWER    PORTC, 6    ; signal to bias transistor to power IR board. 
;        #define LIGHT_IN PORTA, 5

         ORG    0x0000     ;RESET vector must always be at 0x00
         goto   init       ;Just jump to the main code section.

         ORG    0x0004  ; Goes here when an interrupt is triggered
         goto   INTERRUPT_THINGS

;***************************************
; Look up tables
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
        dt      "A:Time B:Summary",0

End_Message2
        addwf   PCL, F
        dt      "C:Info D:Standby",0

Op_time1
        addwf   PCL,F
        dt      " seconds",0

Return_Message
        addwf   PCL,F
        dt      "D: Back to Main",0

Lights_Tested
        addwf   PCL, F
        dt      "PFLN",0

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
; Delay: ~160us macro -- From Sample
;***************************************
LCD_DELAY macro
    movlw   0xFF
    movwf   lcd_d1
    decfsz  lcd_d1,f
    goto    $-1
    endm


;***************************************
; Display macro -- From Sample
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
; Initialize the Operation
;***************************************
init
        bsf       INTCON, GIE   ; enable global interrupts
        bsf       INTCON, 5     ; enable timer 0 interrupts
        bcf       INTCON, 4     ; clear timer0 interrupt flag
        bcf       INTCON, 2     ; disable internal interrupts (from Port B)
        bcf       INTCON, 1     ; clear internal interrupt flag.

        bank1
        movlw     b'00000001'
        movwf     TRISA         ; intialize Port A to have RA0/AN1 be input (for ADC)
    ;    clrf      TRISA          
        movlw     b'11110111'    ; Set required keypad inputs & interrupts
        movwf     TRISB
    ;    movlw     b'00110000'
    ;    movwf     TRISC
        clrf      TRISC          ; All port C is output
        movlw     b'00000011'   ; Enabling input from IR board and contact switches
        movwf     TRISD
;        clrf      TRISD          

; clear all ports before program runs
        bank0
        clrf      PORTA
        clrf      PORTB
        clrf      PORTC
        clrf      PORTD

        call      InitADC   ; initialize the ADC
        call      InitLCD        ;Initialize the LCD 
        call      SERVO_NEUTRAL  ; ensure the servo is in the neutral position before the tray is placed
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
        ;initializing variables which will be loaded during BEGIN_OPERATION
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

COUNTDOWN
        call    ADC_Delay
        call    BEGIN_OPERATION ; Begin checking lights
        call    ClrLCD  ; Clear LCD and display termination message
        movlw   "D"
        call    WR_DATA
        movlw   "O"
        call    WR_DATA
        movlw   "N"
        call    WR_DATA
        movlw   "E"
        call    WR_DATA
        movlw   "!"
        call    WR_DATA
        call    HalfS   ;Delay to make termination message readable 
        call    HalfS
        call    HalfS
        call    HalfS

ENDING
        call    ClrLCD  ;Clear the LCD to make space for the new message
        Display End_Message1    ;Display the first line of the end message
        call    Switch_Lines    ;Switch Lines
        Display End_Message2    ;Display the second line of the end message

END_DISPLAY                     ; Shifts the ending messages to the left
       ; movlw   b'00011000'
        ;call    WR_INS
        ;call    HalfS

        btfss   PORTB,1             ; Check for input from the keypad
        goto    $-1         ; if no input, keep displaying the end message

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
        movlw   " "
        call    WR_DATA
        movlw   "-"
        call    WR_DATA
        movlw   " "
        call    WR_DATA
        movf    stats1, W
        call    Lights_Tested   ; determine which letter to display for each light in summary (N/L/F/P)
        call    WR_DATA
        movf    stats2, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats3, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats4, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats5, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats6, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats7, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats8, W
        call    Lights_Tested
        call    WR_DATA
        movf    stats9, W
        call    Lights_Tested
        call    WR_DATA
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check 1, but for light 3
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check1, but for light 4
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check1, but for light 5 
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check1, but for light 6
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check1, but for light 7
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check1 but for light 8 
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
        call    SUBDISPLAY
        goto    LIGHTSTATS_ANALYZE

; as in check1 but for light 9
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
; LCD control - From Sample
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
; Delay 0.5s - From Sample
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
    movlw     b'11000111'   ;intializing the timer
    banksel   OPTION_REG
    movwf     OPTION_REG    ; starting the timer
    bank0
 ;  call    CHECK_TRAY  ; checking whether the tray is seated correctly
    movlw   d'1'        ; initializing the loop counter for lowering the shade array
    movwf   loop_counter
ARRAY_LOWER ;lowering the shade array
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    call    STEPPER_DRIVERFOR
    decfsz  loop_counter    ; if the loop counter is not 0, keep lowering!
    goto    ARRAY_LOWER
    bsf     STEPD   ; set the motor to stay at the last step
    call    HalfS   ; delay
    call    SERVO_ON    ; turn on the lights :)
; TEST REMOVING HALFS DELAYS
CHECK_LED1
    bcf     MUXE    ; ground the enable to activate the mux
    bcf     MUX0    ; ground all select pins to select light 1
    bcf     MUX1
    bcf     MUX2
    bcf     MUX3
    call    HalfS   ; delay to allow switching to occur
    call    LIGHT_TEST  ; test the light
    movwf   stats1  ; store the result in the stat register for the light
    call    HalfS   ; delay to ensure values have been stored correctly
    
CHECK_LED2
    bsf     MUX0    ; select 1 to get light 2
    call    HalfS
    call    LIGHT_TEST
    movwf   stats2
    call    HalfS

CHECK_LED3
    bcf     MUX0    ; select 2 to get light 3
    bsf     MUX1
    call    HalfS
    call    LIGHT_TEST
    movwf   stats3
    call    HalfS

CHECK_LED4
    bsf     MUX0    ; select 3 to get light 4
    call    HalfS
    call    LIGHT_TEST
    movwf   stats4
    call    HalfS

CHECK_LED5
    bcf     MUX0    ; select 4 to get light 5
    bcf     MUX1
    bsf     MUX2
    call    HalfS
    call    LIGHT_TEST
    movwf   stats5
    call    HalfS

CHECK_LED6
    bsf     MUX0    ; select 5 to get light 6
    call    HalfS
    call    LIGHT_TEST
    movwf   stats6
    call    HalfS

CHECK_LED7
    bsf     MUX1    ; select 6 to get light 7
    bcf     MUX0
    call    HalfS
    call    LIGHT_TEST
    movwf   stats7
    call    HalfS

CHECK_LED8
    bsf     MUX0    ; select 7 to get light 8 
    call    HalfS
    call    LIGHT_TEST
    movwf   stats8
    call    HalfS

CHECK_LED9
    bcf     MUX0    ; select 8 to get light 9 #eternalmysteries
    bcf     MUX1
    bcf     MUX2
    bsf     MUX3
    call    HalfS
    call    LIGHT_TEST
    movwf   stats9
    call    HalfS
    

END_OPERATION
    call    SERVO_NEUTRAL   ; turn off the lights
    bcf     STEPD   ; clear the stepper motor from its current position
    movlw   d'1';30 ; set the loop counter so sensor array goes up as much as it went down
    movwf   loop_counter

ARRAY_LIFT
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    call    STEPPER_DRIVERREV
    decfsz  loop_counter
    goto    ARRAY_LIFT  ; continue lifting the array as long as the counter is not 0

    bsf     INTCON, 5   ; clear the timer interrupt
    movf    optime, W   ; save the operation time
    movwf   optime_final

    return

;***********
; LIGHT TEST
;***********

LIGHT_TEST
    bcf     IR_POWER    ; unbias transistor to turn on IR board
    call    ADC_Delay   ; call delay to allow board to turn on
    btfss   IRMUX       ; see if the light is there!
    goto    NOT_THERE   ; if IR_MUX is low, no reflection = no light
    incf    lights_total    ;if the light is there, increment the total number of lights
    movlw   b'0'    ; clear data points, voltage_temp, voltage_ref, led_on and flicker counters
    movwf   data_points
    movwf   voltage_templ
    movwf   voltage_refl
    movwf   voltage_temph
    movwf   voltage_refh
    movwf   led_on_flag
    movwf   flicker_flag
    movlw   d'30'   ; SMALLER?? Set loop counter, to make sure enough data points are collected
    movwf   loop_counter
    bsf     IR_POWER    ; bias transistor to turn of IR board
    call    ADC_Delay   ; allow board to power down
    goto    ON_TEST     ; begin testing lights
    call    InitADC

NOT_THERE
    movlw   d'3'
    return
; If not there, store 'N/A'
ON_TEST

; ADC TEST
    call    ADC_MainLoop    ;call for conversion of incoming voltage
    banksel ADRESL
    movf    ADRESL, W
    bank0
    movwf   voltage_refl
    movf    ADRESH, W
    movwf   voltage_refh
; store ADRESL in voltage_refL and ADRESH in voltage_refH (first data point only) for reference for flickering
LED_TEST
; ADC TEST
    call    ADC_MainLoop    ; convert incoming voltage
    banksel ADRESL
    movf    ADRESL, W
    movwf   voltage_templ
    bank0
    movf    ADRESH, W
    movwf   voltage_temph
; store new voltage in voltage_temp

    call    COMPARE_ON  ; see if voltage_temp > threshold
    btfss   STATUS, C   ; if status C is set, voltage_temp > W (threshold)
    incf    led_on_flag ; if voltage_temp > threshold, increase the number of times the light is detected "on"

    incf    data_points ; increment number of data points used
    movlw   d'254'  ; max out at 254 data points
    subwf   data_points, W  ; see if data points > 254
    btfss   STATUS, C   ; if it is, end. if not, check again
    goto    LED_TEST

    movlw   d'0'    ; clear data points (for flicker testing)
    movwf   data_points
    movlw   d'10'   ; test if led_flag > 10
    subwf   led_on_flag, W
    btfsc   STATUS, C   ; if greater than 10, test for flickering. if not store LED fail and end checking
    goto    FLICKER_TEST
    movlw   d'2'    ; LED fail
    return

FLICKER_TEST

;   ADC TEST
    call    ADC_MainLoop    ; convert incoming voltage
    banksel ADRESL
    movf    ADRESL, W
    movwf   voltage_templ
    bank0
    movf    ADRESH, W
    movwf   voltage_temph    ; store new voltage in voltage_temp
    call    COMPARE_REF_NEW ; see if voltage_temp > voltage_reference or not
    btfsc   STATUS, C   ; if voltage_temp < reference, swap them
    call    SWAP_VOLTAGES

; Flicker voltage change - take the difference between the reference and the new voltage
;16 bit subtraction, voltage_temp = voltage_temp - voltage_reference
;http://www.piclist.com/techref/microchip/math/sub/16bb.htm

    movf    voltage_refl, W
    subwf   voltage_templ
    movf    voltage_refh, W
    btfss   STATUS, C
    incfsz  voltage_refh, W
    subwf   voltage_temph

; Compare the result of subtraction (now stored in voltage_temp) to the threshold
    call    COMPARE
    btfsc   STATUS, C   ; if voltage_temp > threshold (0.25 V), STATUS C = 1
    incf    flicker_flag    ; if Status C = 1, increment the number of times it's flickered

    incf    data_points ; increment number of data points taken
    movlw   d'254'  ; if data points < 254, check again
    subwf   data_points, W
    btfss   STATUS, C
    goto    FLICKER_TEST    ; if data points > 254, check number of flickers

    movlw   d'10'
    subwf   flicker_flag, W ; if there have been more than 10 flickers, in 254 data points, end checking
    btfsc   STATUS, C
    goto    LIGHT_STATUS    ; skip to storing pass as the result

    decfsz  loop_counter    ; decrease number of repeats for the loop to make. if not 0, check again
    goto    FLICKER_TEST

LIGHT_STATUS
    movlw   d'10'   ; check if flicker was detected more than 10 times
    subwf   flicker_flag, W
    btfsc   STATUS, C
    goto    PASS    ; if flicker > 10, then store pass
    movlw   d'1'    ; otherwise, store flicker fail
    return

PASS
    movlw   d'0'
    return

SWAP_VOLTAGES
    movf    voltage_templ, W    ; store voltage_temp in W, save W
    movwf   w_temp
    movf    voltage_refl, W     ; move voltage_ref into W, move W into voltage_temp
    movwf   voltage_templ
    movf    w_temp, W           ; move W_temp (saved W) into voltage_ref
    movwf   voltage_refl

    movf    voltage_temph, W
    movwf   w_temp
    movf    voltage_refh, W
    movwf   voltage_temph
    movf    w_temp, W
    movwf   voltage_refh
    return

; 16 bit compare. Voltage_temp against flicker difference threshold
; Compare function sourced from piclist.com
COMPARE
    movlw   d'000'
    movwf   w_temp
    movf    voltage_temph, W
    subwf   w_temp, W
    btfss   STATUS, Z
    return
    movlw   d'50'
    movwf   w_temp
    movf    voltage_templ, W
    subwf   w_temp, W ; subtract the temp from 0.25 V.
; If voltage > 0.25 V, C = 0
; If voltage <= 0.25 V, C = 1
    return

; 16 bit compare sourced from piclist.com
; Comparing voltage_temp to 0.8 V (ON threshold)
COMPARE_ON
    movlw   d'000'
    movwf   w_temp
    movf    voltage_temph, W
    subwf   w_temp, W
    btfss   STATUS, Z
    return
    movlw   d'165'
    movwf   w_temp
    movf    voltage_templ, W
    subwf   w_temp, W ; subtract the temp from 0.8 V.
; If voltage > 0.8 V, C = 0
; If voltage <= 0.8 V, C = 1
    return

; 16 bit compare from piclist.com
; Compare the new voltage measure to the reference
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

;*********
; STEPPER MOTOR DRIVING THINGS
; *********
; Driving the stepper motor "forward" - ACBD
STEPPER_DRIVERFOR
    bsf     STEPA
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPA
    bsf     STEPC
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPC
    bsf     STEPB
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPB
    bsf     STEPD
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPD

    bsf     STEPA
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPA
    bsf     STEPC
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPC
    bsf     STEPB
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPB
    bsf     STEPD
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPD

    bsf     STEPA
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPA
    bsf     STEPC
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPC
    bsf     STEPB
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPB
    bsf     STEPD
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPD

    return

; Reversing the stepper motor. DBCA
STEPPER_DRIVERREV
    bsf     STEPD
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPD
    bsf     STEPB
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPB
    bsf     STEPC
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPC
    bsf     STEPA
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPA

    bsf     STEPD
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPD
    bsf     STEPB
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPB
    bsf     STEPC
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPC
    bsf     STEPA
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPA

    bsf     STEPD
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPD
    bsf     STEPB
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPB
    bsf     STEPC
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPC
    bsf     STEPA
    call    lcdLongDelay
    call    lcdLongDelay
    bcf     STEPA

    return

;***************
; SERVO MOTORS
;***************
; Servo neutral initializes servo position, turns lights off 
SERVO_NEUTRAL
    bank1
    movlw   b'10000000' ; set timer period (128)
    movwf   PR2
    bank0
    movlw   b'00001100' ; setting to PWM mode
    movwf   CCP1CON
    movlw   b'00000110' ; set the timer pre-scaler (1:16) and turn on timer
    movwf   T2CON
    movlw   b'00000110' ; setting duty cycle to 6%
    movwf   CCPR1L
    clrf    TMR2    ; clear timer before starting
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    return
; turn lights on :) 
SERVO_ON
    movlw   b'01100011' ; change the duty cycle to 99%
    movwf   CCPR1L
    clrf    TMR2    ; clear the timer
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    call    ADC_Delay
    return

;****************
; INTERRUPT THINGS
; ****************
INTERRUPT_THINGS
    movwf  w_temp   ; move whatever is in W to a temp register
    movf   STATUS, W    ;move status to the W register
    movwf  status_temp  ; save the status register in a temp register
    btfsc  INTCON, 2    ; test to see if the timer is causing the interrupt
    call   TIMER_ISR    ; if it's the timer, go to the timer ISR
  ;  btfsc  INTCON, 1   ; one way of doing the emergency stop...test to see if internal interrupt
  ;  call   EMERGSTOP_ISR
    movf   status_temp, W   ; restore the status register
    movwf  STATUS
    swapf  w_temp, f    ; restore W
    swapf  w_temp, w
    retfie  ; return from interrupt

TIMER_ISR
    bcf     INTCON, 2   ; clear the timer flag
    decfsz  count38, f  ; decrease the counter from 38 (timer interrupt happens ~38 times/second)
    return  ; if the interrupt hasn't happened 38 times, return to main program
    incf    optime  ; if it has, increment optime
    movlw   d'38'   ; restore counter to 38
    movwf   count38
    return  ; return to main

; Emergency Stop ISR would display Emergency Stop Message and poll for the interrupt to clear
;EMERGSTOP_ISR
 ;   btfss   PORTB, 0
 ;   return

 ;   bcf     INTCON, 1
 ;   call    ClrLCD
 ;   movlw   "E"
 ;   call    WR_DATA
 ;   movlw   "M"
 ;   call    WR_DATA
 ;   movlw   "E"
 ;   call    WR_DATA
 ;   movlw   "R"
 ;   call    WR_DATA
 ;   movlw   "G"
 ;   call    WR_DATA
 ;   movlw   "E"
 ;   call    WR_DATA
 ;   movlw   "N"
 ;   call    WR_DATA
 ;   movlw   "C"
 ;   call    WR_DATA
 ;   movlw   "Y"
 ;   call    WR_DATA
 ;   movlw   " "
 ;   call    WR_DATA
 ;   movlw   "S"
 ;   call    WR_DATA
 ;   movlw   "T"
 ;   call    WR_DATA
 ;   movlw   "O"
 ;   call    WR_DATA
  ;;  movlw   "P"
  ;  call    WR_DATA
  ;  btfsc   PORTB, 0
  ;  goto    $-1
  ;  call    ClrLCD
  ;  Display OpMessage
  ;  return

;**************
; ADC THINGS
;**************
; Initialize the ADC
InitADC
    bank1
    movlw   b'10001110' ; right justified, only RA0 is Analog, Vref+ = Vdd, Vref- = Vss
    movwf   ADCON1
    bank0
    movlw   b'11000101' ; setting for internal clock to be used, RA0 to be tested, turn on ADC
    movwf   ADCON0
    return

ADC_MainLoop
    call    ADC_Delay   ; wait acquisition time
    call    ADC_Delay
    bsf     ADCON0, GO  ; start conversion
WAIT
    btfsc   ADCON0, GO
    goto    WAIT    ; wait until conversion completes
    bank0
    return  ; return to main code

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



