	Delay1	EQU	0x22
	Delay2	EQU	0x23

	bank1
	movlw	b'00001110' ; All digital input except RA0
	movwf	ADCON1

	bank0
	movlw	b'11000001' ; Clock selected, ADC on
	movwf	ADCON0

	call	AD_CONVERTOR



AD_CONVERTOR
	bsf		ADCON0, 2 ;start conversion and wait for it to complete
	btfsc   ADCON0, 2
	goto	$-1

	movwf	ADRESH, W
	return

