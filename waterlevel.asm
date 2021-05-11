; project1.asm: Determines and speaks out the percentage fill of a cup
;	authors: Mohkam, Gagan, Kaustabh
;
; Connections:
; 
; EFM8 board  SPI_FLASH
; P0.0        Pin 6 (SPI_CLK)
; P0.1        Pin 2 (MISO)
; P0.2        Pin 5 (MOSI)
; P0.3        Pin 1 (CS/)
; GND         Pin 4
; 3.3V        Pins 3, 7, 8  (The MCP1700 3.3V voltage regulator or similar is required)
;
; P3.0 is the DAC output which should be connected to the input of power amplifier (LM386 or similar)
;

$NOLIST
$MODEFM8LB1
$LIST

SYSCLK         EQU 72000000  ; Microcontroller system clock frequency in Hz
TIMER2_RATE    EQU 22050     ; 22050Hz is the sampling rate of the wav file we are playing
TIMER2_RELOAD  EQU 0x10000-(SYSCLK/TIMER2_RATE)
F_SCK_MAX      EQU 20000000
BAUDRATE       EQU 115200

FLASH_CE EQU P0.3
SPEAKER  EQU P2.0

; Commands supported by the SPI flash memory according to the datasheet
WRITE_ENABLE     EQU 0x06  ; Address:0 Dummy:0 Num:0
WRITE_DISABLE    EQU 0x04  ; Address:0 Dummy:0 Num:0
READ_STATUS      EQU 0x05  ; Address:0 Dummy:0 Num:1 to infinite
READ_BYTES       EQU 0x03  ; Address:3 Dummy:0 Num:1 to infinite
READ_SILICON_ID  EQU 0xab  ; Address:0 Dummy:3 Num:1 to infinite
FAST_READ        EQU 0x0b  ; Address:3 Dummy:1 Num:1 to infinite
WRITE_STATUS     EQU 0x01  ; Address:0 Dummy:0 Num:1
WRITE_BYTES      EQU 0x02  ; Address:3 Dummy:0 Num:1 to 256
ERASE_ALL        EQU 0xc7  ; Address:0 Dummy:0 Num:0
ERASE_BLOCK      EQU 0xd8  ; Address:3 Dummy:0 Num:0
READ_DEVICE_ID   EQU 0x9f  ; Address:0 Dummy:2 Num:1 to infinite

; Variables used in the program:
dseg at 30H
	x:		ds 4
	y:		ds 4
	bcd:	ds 5
	w:   ds 3 ; 24-bit play counter.  Decremented in Timer 2 ISR.

bseg
	mf:		dbit 1 
; Interrupt vectors:
cseg
Left_blank mac
    mov a, %0
    anl a, #0xf0
    swap a
    jz Leftblank%M_a
    ljmp %1
Leftblank%M_a:
    Display_char(#' ')
    mov a, %0
    anl a, #0x0f
    jz Leftblank%M_b
    ljmp %1
Leftblank%M_b:
    Display_char(#' ')
endmac
org 0x0000 ; Reset vector
    ljmp MainProgram

org 0x0003 ; External interrupt 0 vector (not used in this code)
	reti

org 0x000B ; Timer/Counter 0 overflow interrupt vector (not used in this code)
	reti

org 0x0013 ; External interrupt 1 vector (not used in this code)
	reti

org 0x001B ; Timer/Counter 1 overflow interrupt vector (not used in this code
	reti

org 0x0023 ; Serial port receive/transmit interrupt vector (not used in this code)
	reti

org 0x005b ; Timer 2 interrupt vector.  Used in this code to replay the wave file.
	ljmp Timer2_ISR

Msg1:  db 'PERCENT', 0
Msg2:	db 'FILLED', 0

LCD_RS equ P2.0
LCD_RW equ P1.7
LCD_E  equ P1.6
LCD_D4 equ P1.1
LCD_D5 equ P1.0
LCD_D6 equ P0.7
LCD_D7 equ P0.6
$NOLIST
$include(LCD_4bit_72MHz.inc)
$include(math32.inc)
$LIST

Wait_one_second:        ; changed to 72 MHz clock
    ;For a 72MHz clock one machine cycle takes 1/72MHz=13.88889ns
    mov R3, #3 ; Calibrate using this number to account for overhead delays
X4: mov R2, #180
X3: mov R1, #245
X2: mov R0, #167
X1: djnz R0, X1 ; 3 machine cycles -> 340.81633ns167=6.95833us (see table 10.2 in reference manual)
    djnz R1, X2 ; 6.95883us245=1.70479ms
    djnz R2, X3 ; 1.70479ms180=0.30686s
    djnz R3, X4 ; 0.30686*3=0.9205875s + overhead 
    ret

;Converts the hex number in TH0-TL0 to packed BCD in R2-R1-R0
hex2bcd_period:
	clr a
    mov R0, #0  ; Set packed BCD result to 00000 
    mov R1, #0
    mov R2, #0
    mov R3, #16 ; Loop counter.
    
hex2bcd_L0_period:
    mov a, TL0 ; Shift TH0-TL0 left through carry
    rlc a
    mov TL0, a
    
    mov a, TH0
    rlc a
    mov TH0, a
    
	; Perform bcd + bcd + carry
	; using BCD numbers
	mov a, R0
	addc a, R0
	da a
	mov R0, a
	
	mov a, R1
	addc a, R1
	da a
	mov R1, a
	
	mov a, R2
	addc a, R2
	da a
	mov R2, a
	
	djnz R3, hex2bcd_L0_period
	ret

; Dumps the 5-digit packed BCD number in R2-R1-R0 into the LCD
DisplayBCD:
	; 5th digit:
    mov a, R2
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 4th digit:
    mov a, R1
    swap a
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 3rd digit:
    mov a, R1
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 2nd digit:
    mov a, R0
    swap a
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 1st digit:
    mov a, R0
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
    
    ret
;-------------------------------------;
; ISR for Timer 2.  Used to playback  ;
; the WAV file stored in the SPI      ;
; flash memory.                       ;
;-------------------------------------;
Timer2_ISR:
	mov	SFRPAGE, #0x00
	clr	TF2H ; Clear Timer2 interrupt flag

	; The registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Check if the play counter is zero.  If so, stop playing sound.
	mov a, w+0
	orl a, w+1
	orl a, w+2
	jz stop_playing
	
	; Decrement play counter 'w'.  In this implementation 'w' is a 24-bit counter.
	mov a, #0xff
	dec w+0
	cjne a, w+0, keep_playing
	dec w+1
	cjne a, w+1, keep_playing
	dec w+2
	
keep_playing:

	setb SPEAKER
	lcall Send_SPI ; Read the next byte from the SPI Flash...
	
	; It gets a bit complicated here because we read 8 bits from the flash but we need to write 12 bits to DAC:
	mov SFRPAGE, #0x30 ; DAC registers are in page 0x30
	push acc ; Save the value we got from flash
	swap a
	anl a, #0xf0
	mov DAC0L, a
	pop acc
	swap a
	anl a, #0x0f
	mov DAC0H, a
	mov SFRPAGE, #0x00
	
	sjmp Timer2_ISR_Done

stop_playing:
	clr TR2 ; Stop timer 2
	setb FLASH_CE  ; Disable SPI Flash
	clr SPEAKER ; Turn off speaker.  Removes hissing noise when not playing sound.

Timer2_ISR_Done:	
	pop psw
	pop acc
	reti

;---------------------------------;
; Sends a byte via serial port    ;
;---------------------------------;
putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret

;---------------------------------;
; Receive a byte from serial port ;
;---------------------------------;
getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret

;---------------------------------;
; Sends AND receives a byte via   ;
; SPI.                            ;
;---------------------------------;
Send_SPI:
	mov	SPI0DAT, a
Send_SPI_L1:
	jnb	SPIF, Send_SPI_L1 ; Wait for SPI transfer complete
	clr SPIF ; Clear SPI complete flag 
	mov	a, SPI0DAT
	ret

;---------------------------------;
; SPI flash 'write enable'        ;
; instruction.                    ;
;---------------------------------;
Enable_Write:
	clr FLASH_CE
	mov a, #WRITE_ENABLE
	lcall Send_SPI
	setb FLASH_CE
	ret

;---------------------------------;
; This function checks the 'write ;
; in progress' bit of the SPI     ;
; flash memory.                   ;
;---------------------------------;
Check_WIP:
	clr FLASH_CE
	mov a, #READ_STATUS
	lcall Send_SPI
	mov a, #0x55
	lcall Send_SPI
	setb FLASH_CE
	jb acc.0, Check_WIP ;  Check the Write in Progress bit
	ret
	
Init_all:
	; Disable WDT:
	mov	WDTCN, #0xDE
	mov	WDTCN, #0xAD
	
	mov	VDM0CN, #0x80
	mov	RSTSRC, #0x06
	
	; Switch SYSCLK to 72 MHz.  First switch to 24MHz:
	mov	SFRPAGE, #0x10
	mov	PFE0CN, #0x20
	mov	SFRPAGE, #0x00
	mov	CLKSEL, #0x00
	mov	CLKSEL, #0x00 ; Second write to CLKSEL is required according to datasheet
	
	; Wait for clock to settle at 24 MHz by checking the most significant bit of CLKSEL:
Init_L1:
	mov	a, CLKSEL
	jnb	acc.7, Init_L1
	
	; Now switch to 72MHz:
	mov	CLKSEL, #0x03
	mov	CLKSEL, #0x03  ; Second write to CLKSEL is required according to datasheet
	
	; Wait for clock to settle at 72 MHz by checking the most significant bit of CLKSEL:
Init_L2:
	mov	a, CLKSEL
	jnb	acc.7, Init_L2

	mov	SFRPAGE, #0x00
	
	; Configure P3.0 as analog output.  P3.0 pin is the output of DAC0.
	anl	P3MDIN, #0xFE
	orl	P3, #0x01
	
	; Configure the pins used for SPI (P0.0 to P0.3)
	mov	P0MDOUT, #0x1D ; SCK, MOSI, P0.3, TX0 are push-pull, all others open-drain

	ORL P0SKIP, #0b11001000
	ORL P1SKIP, #0b00000011

	mov	XBR0, #0x03 ; Enable SPI and UART0: SPI0E=1, URT0E=1
	mov	XBR1, #0x00
	mov	XBR2, #0x40 ; Enable crossbar and weak pull-ups

	; Enable serial communication and set up baud rate using timer 1
	mov	SCON0, #0x10	
	mov	TH1, #(0x100-((SYSCLK/BAUDRATE)/(12*2)))
	mov	TL1, TH1
	anl	TMOD, #0x0F ; Clear the bits of timer 1 in TMOD
	orl	TMOD, #0x20 ; Set timer 1 in 8-bit auto-reload mode.  Don't change the bits of timer 0
	setb TR1 ; START Timer 1
	setb TI ; Indicate TX0 ready
	
	; Configure DAC 0
	mov	SFRPAGE, #0x30 ; To access DAC 0 we use register page 0x30
	mov	DACGCF0, #0b_1000_1000 ; 1:D23REFSL(VCC) 1:D3AMEN(NORMAL) 2:D3SRC(DAC3H:DAC3L) 1:D01REFSL(VCC) 1:D1AMEN(NORMAL) 1:D1SRC(DAC1H:DAC1L)
	mov	DACGCF1, #0b_0000_0000
	mov	DACGCF2, #0b_0010_0010 ; Reference buffer gain 1/3 for all channels
	mov	DAC0CF0, #0b_1000_0000 ; Enable DAC 0
	mov	DAC0CF1, #0b_0000_0010 ; DAC gain is 3.  Therefore the overall gain is 1.
	; Initial value of DAC 0 is mid scale:
	mov	DAC0L, #0x00
	mov	DAC0H, #0x08
	mov	SFRPAGE, #0x00
	
	; Configure SPI
	mov	SPI0CKR, #((SYSCLK/(2*F_SCK_MAX))-1)
	mov	SPI0CFG, #0b_0100_0000 ; SPI in master mode
	mov	SPI0CN0, #0b_0000_0001 ; SPI enabled and in three wire mode
	setb FLASH_CE ; CS=1 for SPI flash memory
	clr SPEAKER ; Turn off speaker.
	
	; Configure Timer 2 and its interrupt
	mov	TMR2CN0,#0x00 ; Stop Timer2; Clear TF2
	orl	CKCON0,#0b_0001_0000 ; Timer 2 uses the system clock
	; Initialize reload value:
	mov	TMR2RLL, #low(TIMER2_RELOAD)
	mov	TMR2RLH, #high(TIMER2_RELOAD)
	; Set timer to reload immediately
	mov	TMR2H,#0xFF
	mov	TMR2L,#0xFF
	setb ET2 ; Enable Timer 2 interrupts
	; setb TR2 ; Timer 2 is only enabled to play stored sound

	;Initializes timer/counter 0 as a 16-bit counter
	clr TR0 ; Stop timer 0
    mov a, TMOD
    anl a, #0b_1111_0000 ; Clear the bits of timer/counter 0
    orl a, #0b_0000_0001 ; Sets the bits of timer/counter 0 for a 16-bit timer with input clock 24.5MHz/12
    mov TMOD, a

	; Configure LCD and display initial message
	lcall LCD_4BIT
	Set_Cursor(1, 1)
    Send_Constant_String(#Msg1)
    Set_Cursor(2,1)
    Send_Constant_String(#Msg2)
	setb EA ; Enable interrupts
	
	ret

; Sends 10-digit BCD number in bcd to the LCD
Display_10_digit_BCD:
    Set_Cursor(2, 1)
    Display_BCD(bcd+4)
    Display_BCD(bcd+3)
    Display_BCD(bcd+2)
    Display_BCD(bcd+1)
    Display_BCD(bcd+0)
    ; Replace all the zeros to the left with blanks
    Set_Cursor(2, 1)
    Left_blank(bcd+4, skip_blank)
    Left_blank(bcd+3, skip_blank)
    Left_blank(bcd+2, skip_blank)
    Left_blank(bcd+1, skip_blank)
    mov a, bcd+0
    anl a, #0f0h
    swap a
    jnz skip_blank
    Display_char(#' ')
skip_blank:
    ret
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
MainProgram:
    mov SP, #0x7f ; Setup stack pointer to the start of indirectly accessable data memory minus one
    lcall Init_all ; Initialize the hardware
	


Forever:
    ; Measure the period applied to pin P0.0
    clr TR0 ; Stop counter 0
    mov TL0, #0
    mov TH0, #0
    jb P1.2, $
    jnb P1.2, $
    mov R0, #100 ; Counting 100 periods to increase resolution
    setb TR0 ; Start counter 0
    
period_loop:
    jb P1.2, $
    jnb P1.2, $
    djnz R0, period_loop
    clr TR0 ; Stop counter 0, TH0-TL0 has the period
	Wait_Milli_Seconds(#250)
	
	; store period in y
	mov y+0, TL0
	mov y+1, TH0
	mov y+2, #0 ; pad high bits with zero
	mov y+3, #0 ; pad high bits with zero
	
	Load_x(6340)		; max value at 100%
	lcall sub32			; change in max value minus period 
	
	; check percentage of change 
	Load_y(5995)		; x times slope 
	lcall mul32 
	
	Load_y(10000)
	lcall div32 
	
	;store result in y
	mov y+0, x+0
	mov y+1, x+1
	mov y+2, x+2
	mov y+3, x+3
	
	load_x(100)
	lcall mul32		; y*100
	
	; store result in y 
	mov y+0, x+0
	mov y+1, x+1
	mov y+2, x+2
	mov y+3, x+3
	
	Load_x(10821)			; 108.21 - y
	lcall sub32 
	
	Load_y(100)				; overflows past 108
	lcall div32
	
	; overflow = 0%
	; greater than 100 = 100%
	
	; add interrupts for sounds?
	; compare values, jump to functions for sounds 
	; so if x is between 	0-5 = 0%			; if x is less than 10 say 0%, if not check next statement 
	;						6-15 = 10%			; if x is greater than 10 but less than 20 
	;						16-25 = 20%
	;						26-35 = 30%
	;						36-45 = 40%
	;						46-55 = 50%
	;						56-65 = 60%
	;						66-75 = 70%
	;						76-85 = 80%
	;						86-95 = 90%
	;						96-105 = 100%

	
	Wait_milli_seconds(#250)
	Wait_milli_seconds(#250)
	Wait_milli_seconds(#250)
	Wait_milli_seconds(#250)
	Wait_milli_seconds(#250)
	Wait_milli_seconds(#250)
	Wait_milli_seconds(#250)
	
	; Convert the result to BCD and display on LCD in units of the clock period applied to timer 0 (12/(100*24.5Mhz)=4.898ns)
	;Set_Cursor(2, 1)
    lcall hex2bcd
    ;lcall Display_10_digit_BCD
    
	; -------------------------------------------0-5%
	; check at 0 percent
	mov a,x+0
	cjne a,#0, check_1 
	lcall play_zero
	
check_1:
	cjne a,#1, check_2
	lcall play_zero

check_2:
	cjne a,#2, check_3
	lcall play_zero
	
check_3:
	cjne a,#3, check_4
	lcall play_zero
	
check_4:
	cjne a,#4, check_5
	lcall play_zero
	
check_5:
	cjne a,#5, check_6
	lcall play_zero

;------------------------------------------6-15%
check_6:
	cjne a, #6, check_7
	lcall play_ten

check_7:
	cjne a, #7, check_8
	lcall play_ten
	
check_8:
	cjne a, #8, check_9
	lcall play_ten
	
check_9:
	cjne a, #9, check_10
	lcall play_ten
	
check_10:
	cjne a, #10, check_11
	lcall play_ten
	
check_11:
	cjne a, #11, check_12
	lcall play_ten
	
check_12:
	cjne a, #12, check_13
	lcall play_ten
	
check_13:
	cjne a, #13, check_14
	lcall play_ten
	
check_14:
	cjne a, #14, check_15
	lcall play_ten
	
check_15:
	cjne a, #15, check_16
	lcall play_ten
	
;----------------------------------------16-25%
check_16:
	cjne a, #16, check_17
	lcall play_twenty
	
check_17:
	cjne a, #17, check_18
	lcall play_twenty
	
check_18:
	cjne a, #18, check_19
	lcall play_twenty
	
check_19:
	cjne a, #19, check_20
	lcall play_twenty
	
check_20:
	cjne a, #20, check_21
	lcall play_twenty
	
check_21:
	cjne a, #21, check_22
	lcall play_twenty
	
check_22:
	cjne a, #22, check_23
	lcall play_twenty
	
check_23:
	cjne a, #23, check_24
	lcall play_twenty
	
check_24:
	cjne a, #24, check_25
	lcall play_twenty
	
check_25:
	cjne a, #25, check_26
	lcall play_twenty
	
;------------------------------------26-35%
check_26:
	cjne a, #26, check_27
	lcall play_thirty

check_27:
	cjne a, #27, check_28
	lcall play_thirty
	
check_28:
	cjne a, #28, check_29
	lcall play_thirty
	
check_29:
	cjne a, #29, check_30
	lcall play_thirty
	
check_30:
	cjne a, #30, check_31
	lcall play_thirty
	
check_31:
	cjne a, #31, check_32
	lcall play_thirty
	
check_32:
	cjne a, #32, check_33
	lcall play_thirty
	
check_33:
	cjne a, #33, check_34
	lcall play_thirty
	
check_34:
	cjne a, #34, check_35
	lcall play_thirty
	
check_35:
	cjne a, #35, check_36
	lcall play_thirty

;-----------------------------------36-45%	
check_36:
	cjne a, #36, check_37
	lcall play_forty

check_37:
	cjne a, #37, check_38
	lcall play_forty
	
check_38:
	cjne a, #38, check_39
	lcall play_forty
	
check_39:
	cjne a, #39, check_40
	lcall play_forty
	
check_40:
	cjne a, #40, check_41
	lcall play_forty
	
check_41:
	cjne a, #41, check_42
	lcall play_forty
	
check_42:
	cjne a, #42, check_43
	lcall play_forty
	
check_43:
	cjne a, #43, check_44
	lcall play_forty
	
check_44:
	cjne a, #44, check_45
	lcall play_forty
	
check_45:
	cjne a, #45, check_46
	lcall play_forty
	
;-----------------------------46-55%
check_46:
	cjne a, #46, check_47
	lcall play_fifty
	
check_47:
	cjne a, #47, check_48
	lcall play_fifty
	
check_48:
	cjne a, #48, check_49
	lcall play_fifty
	
check_49:
	cjne a, #49, check_50
	lcall play_fifty
	
check_50:
	cjne a, #50, check_51
	lcall play_fifty
	
check_51:
	cjne a, #51, check_52
	lcall play_fifty
	
check_52:
	cjne a, #52, check_53
	lcall play_fifty
	
check_53:
	cjne a, #53, check_54
	lcall play_fifty
	
check_54:
	cjne a, #54, check_55
	lcall play_fifty
	
check_55:
	cjne a, #55, check_56
	lcall play_fifty
	
;---------------------------------56-65%
check_56:
	cjne a, #56, check_57
	lcall play_sixty
	
check_57:
	cjne a, #57, check_58
	lcall play_sixty
	
check_58:
	cjne a, #58, check_59
	lcall play_sixty
	
check_59:
	cjne a, #59, check_60
	lcall play_sixty
	
check_60:
	cjne a, #60, check_61
	lcall play_sixty
	
check_61:
	cjne a, #61, check_62
	lcall play_sixty
	
check_62:
	cjne a, #62, check_63
	lcall play_sixty
	
check_63:
	cjne a, #63, check_64
	lcall play_sixty
	
check_64:
	cjne a, #64, check_65
	lcall play_sixty
	
check_65:
	cjne a, #65, check_66
	lcall play_sixty
	
;----------------------------------66-75%
check_66:
	cjne a, #66, check_67
	lcall play_seventy 
	
check_67:
	cjne a, #67, check_68
	lcall play_seventy 
	
check_68:
	cjne a, #68, check_69
	lcall play_seventy 
	
check_69:
	cjne a, #69, check_70
	lcall play_seventy 
	
check_70:
	cjne a, #70, check_71
	lcall play_seventy 
	
check_71:
	cjne a, #71, check_72
	lcall play_seventy 
	
check_72:
	cjne a, #72, check_73
	lcall play_seventy 
	
check_73:
	cjne a, #73, check_74
	lcall play_seventy 
	
check_74:
	cjne a, #74, check_75
	lcall play_seventy 
	
check_75:
	cjne a, #75, check_76
	lcall play_seventy 
	
;----------------------------------76-85%
check_76:
	cjne a, #76, check_77
	lcall play_eighty 
	
check_77:
	cjne a, #77, check_78
	lcall play_eighty 
	
check_78:
	cjne a, #78, check_79
	lcall play_eighty 
	
check_79:
	cjne a, #79, check_80
	lcall play_eighty 
	
check_80:
	cjne a, #80, check_81
	lcall play_eighty 
	
check_81:
	cjne a, #81, check_82
	lcall play_eighty 
	
check_82:
	cjne a, #82, check_83
	lcall play_eighty 
	
check_83:
	cjne a, #83, check_84
	lcall play_eighty 
	
check_84:
	cjne a, #84, check_85
	lcall play_eighty 
	
check_85:
	cjne a, #85, check_86
	lcall play_eighty 
	
;--------------------------------------86-95%
check_86:
	cjne a, #86, check_87
	lcall play_ninety 
	
check_87:
	cjne a, #87, check_88
	lcall play_ninety 
	
check_88:
	cjne a, #88, check_89
	lcall play_ninety 
	
check_89:
	cjne a, #89, check_90
	lcall play_ninety 
	
check_90:
	cjne a, #90, check_91
	lcall play_ninety 
	
check_91:
	cjne a, #91, check_92
	lcall play_ninety 
	
check_92:
	cjne a, #92, check_93
	lcall play_ninety 
	
check_93:
	cjne a, #93, check_94
	lcall play_ninety 
	
check_94:
	cjne a, #94, check_95
	lcall play_ninety 
	
check_95:
	cjne a, #95, check_96
	lcall play_ninety 

;----------------------------------------96-105%
check_96:
	cjne a, #96, check_97
	lcall play_one_hundred
	
check_97:
	cjne a, #97, check_98
	lcall play_one_hundred
	
check_98:
	cjne a, #98, check_99
	lcall play_one_hundred
	
check_99:
	cjne a, #99, check_100
	lcall play_one_hundred
	
check_100:
	cjne a, #100, check_101
	lcall play_one_hundred
	
check_101:
	cjne a, #101, check_102
	lcall play_one_hundred
	
check_102:
	cjne a, #102, check_103
	lcall play_one_hundred
	
check_103:
	cjne a, #103, check_104
	lcall play_one_hundred
	
check_104:
	cjne a, #104, check_105
	lcall play_one_hundred
	
check_105:
	cjne a, #105, repeat
	lcall play_one_hundred

repeat:
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;=======================================================================	
	ljmp Forever ; Repeat!

;compare:
;	cjne A, #27, not_equal

;equal:        ;A=27
;    SJMP END_CMP

;not_equal:        
;	JC A_LESS
		
;A_GREATER:    ;A > 27
;    SJMP END_CMP

;A_LESS:        ;A < 27

;END_CMP:    ;Finishe
	

	
	
forever_loop:
	;jb RI, serial_get
	;jb P3.7, forever_loop ; Check if push-button pressed
	;jnb P3.7, $ ; Wait for push-button release
	; Play the whole memory
	;clr TR2 ; Stop Timer 2 ISR from playing previous request
	;setb FLASH_CE
	;clr SPEAKER ; Turn off speaker.
	
	;clr FLASH_CE ; Enable SPI Flash
	;mov a, #READ_BYTES
	;lcall Send_SPI
	; Set the initial position in memory where to start playing
	;mov a, #0x00
	;lcall Send_SPI
	;mov a, #0x00
	;lcall Send_SPI
	;mov a, #0x00
	;lcall Send_SPI
	;mov a, #0x00 ; Request first byte to send to DAC
	;lcall Send_SPI
	
	; How many bytes to play? All of them!  Asume 4Mbytes memory: 0x3fffff
	;mov w+2, #0x3f
	;mov w+1, #0xff
	;mov w+0, #0xff
	
	;setb SPEAKER ; Turn on speaker.
	;setb TR2 ; Start playback by enabling Timer 2
	
	;lcall zero_percent_1
	;lcall zero_percent_2
	
serial_get:
	lcall getchar ; Wait for data to arrive
	cjne a, #'#', forever_loop ; Message format is #n[data] where 'n' is '0' to '9'
	clr TR2 ; Stop Timer 2 from playing previous request
	setb FLASH_CE ; Disable SPI Flash	
	clr SPEAKER ; Turn off speaker.
	lcall getchar

;---------------------------------------------------------	
	cjne a, #'0' , Command_0_skip
Command_0_start: ; Identify command
	clr FLASH_CE ; Enable SPI Flash	
	mov a, #READ_DEVICE_ID
	lcall Send_SPI	
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	setb FLASH_CE ; Disable SPI Flash
	ljmp forever_loop	
Command_0_skip:

;---------------------------------------------------------	
	cjne a, #'1' , Command_1_skip 
Command_1_start: ; Erase whole flash (takes a long time)
	lcall Enable_Write
	clr FLASH_CE
	mov a, #ERASE_ALL
	lcall Send_SPI
	setb FLASH_CE
	lcall Check_WIP
	mov a, #0x01 ; Send 'I am done' reply
	lcall putchar		
	ljmp forever_loop	
Command_1_skip:

;---------------------------------------------------------	
	cjne a, #'2' , Command_2_skip 
Command_2_start: ; Load flash page (256 bytes or less)
	lcall Enable_Write
	clr FLASH_CE
	mov a, #WRITE_BYTES
	lcall Send_SPI
	lcall getchar ; Address bits 16 to 23
	lcall Send_SPI
	lcall getchar ; Address bits 8 to 15
	lcall Send_SPI
	lcall getchar ; Address bits 0 to 7
	lcall Send_SPI
	lcall getchar ; Number of bytes to write (0 means 256 bytes)
	mov r0, a
Command_2_loop:
	lcall getchar
	lcall Send_SPI
	djnz r0, Command_2_loop
	setb FLASH_CE
	lcall Check_WIP
	mov a, #0x01 ; Send 'I am done' reply
	lcall putchar		
	ljmp forever_loop	
Command_2_skip:

;---------------------------------------------------------	
	cjne a, #'3' , Command_3_skip 
Command_3_start: ; Read flash bytes (256 bytes or less)
	clr FLASH_CE
	mov a, #READ_BYTES
	lcall Send_SPI
	lcall getchar ; Address bits 16 to 23
	lcall Send_SPI
	lcall getchar ; Address bits 8 to 15
	lcall Send_SPI
	lcall getchar ; Address bits 0 to 7
	lcall Send_SPI
	lcall getchar ; Number of bytes to read and send back (0 means 256 bytes)
	mov r0, a

Command_3_loop:
	mov a, #0x55
	lcall Send_SPI
	lcall putchar
	djnz r0, Command_3_loop
	setb FLASH_CE	
	ljmp forever_loop	
Command_3_skip:

;---------------------------------------------------------	
	cjne a, #'4' , Command_5_skip
;Command_4_start: ; Playback a portion of the stored wav file
;	clr TR2 ; Stop Timer 2 ISR from playing previous request
;	setb FLASH_CE
	
;	clr FLASH_CE ; Enable SPI Flash
;	mov a, #READ_BYTES
;	lcall Send_SPI
	; Get the initial position in memory where to start playing
;	lcall getchar
;	lcall Send_SPI
;	lcall getchar
;	lcall Send_SPI
;	lcall getchar
;	lcall Send_SPI
	; Get how many bytes to play
;	lcall getchar
;	mov w+2, a
;	lcall getchar
;	mov w+1, a
;	lcall getchar
;	mov w+0, a
	
;	mov a, #0x00 ; Request first byte to send to DAC
;	lcall Send_SPI
	
;	setb TR2 ; Start playback by enabling timer 2
;	ljmp forever_loop

Command_5_start: ; Calculate and send CRC-16 of ISP flash memory from zero to the 24-bit passed value.
	; Get how many bytes to use to calculate the CRC.  Store in [r5,r4,r3]
	lcall getchar
	mov r5, a
	lcall getchar
	mov r4, a
	lcall getchar
	mov r3, a
	
	; Since we are using the 'djnz' instruction to check, we need to add one to each byte of the counter.
	; A side effect is that the down counter becomes efectively a 23-bit counter, but that is ok
	; because the max size of the 25Q32 SPI flash memory is 400000H.
	inc r3
	inc r4
	inc r5
	
	; Initial CRC must be zero.
	mov	SFRPAGE, #0x20 ; UART0, CRC, and SPI can work on this page
	mov	CRC0CN0, #0b_0000_1000 ; // Initialize hardware CRC result to zero;

	clr FLASH_CE
	mov a, #READ_BYTES
	lcall Send_SPI
	clr a ; Address bits 16 to 23
	lcall Send_SPI
	clr a ; Address bits 8 to 15
	lcall Send_SPI
	clr a ; Address bits 0 to 7
	lcall Send_SPI
	mov	SPI0DAT, a ; Request first byte from SPI flash
	sjmp Command_5_loop_start

Command_5_loop:
	jnb SPIF, Command_5_loop 	; Check SPI Transfer Completion Flag
	clr SPIF				    ; Clear SPI Transfer Completion Flag	
	mov a, SPI0DAT				; Save received SPI byte to accumulator
	mov SPI0DAT, a				; Request next byte from SPI flash; while it arrives we calculate the CRC:
	mov	CRC0IN, a               ; Feed new byte to hardware CRC calculator

Command_5_loop_start:
	; Drecrement counter:
	djnz r3, Command_5_loop
	djnz r4, Command_5_loop
	djnz r5, Command_5_loop
Command_5_loop2:	
	jnb SPIF, Command_5_loop2 	; Check SPI Transfer Completion Flag
	clr SPIF			    	; Clear SPI Transfer Completion Flag
	mov a, SPI0DAT	            ; This dummy read is needed otherwise next transfer fails (why?)
	setb FLASH_CE 				; Done reading from SPI flash
	
	; Computation of CRC is complete.  Send 16-bit result using the serial port
	mov	CRC0CN0, #0x01 ; Set bit to read hardware CRC high byte
	mov	a, CRC0DAT
	lcall putchar

	mov	CRC0CN0, #0x00 ; Clear bit to read hardware CRC low byte
	mov	a, CRC0DAT
	lcall putchar
	
	mov	SFRPAGE, #0x00

	ljmp forever_loop	
Command_5_skip:

;---------------------------------------------------------	
	cjne a, #'5' , Command_6_skip 
Command_6_start: ; Fill flash page (256 bytes)
	lcall Enable_Write
	clr FLASH_CE
	mov a, #WRITE_BYTES
	lcall Send_SPI
	lcall getchar ; Address bits 16 to 23
	lcall Send_SPI
	lcall getchar ; Address bits 8 to 15
	lcall Send_SPI
	lcall getchar ; Address bits 0 to 7
	lcall Send_SPI
	lcall getchar ; Byte to write
	mov r1, a
	mov r0, #0 ; 256 bytes
Command_6_loop:
	mov a, r1
	lcall Send_SPI
	djnz r0, Command_6_loop
	setb FLASH_CE
	lcall Check_WIP
	mov a, #0x01 ; Send 'I am done' reply
	lcall putchar		
	ljmp forever_loop	
Command_6_skip:

	ljmp forever_loop
	
play_zero: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x00
 	lcall Send_SPI
 	mov a, #0x00
 	lcall Send_SPI
 	mov a, #0x2d
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0x8a
 	mov w+0, #0x4f
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_ten: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x00
 	lcall Send_SPI
 	mov a, #0x8a
 	lcall Send_SPI
 	mov a, #0x7c
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0x91
 	mov w+0, #0x4c
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_twenty: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x01
 	lcall Send_SPI
 	mov a, #0x1b
 	lcall Send_SPI
 	mov a, #0xc8
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xa1
 	mov w+0, #0xfb
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_thirty: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x01
 	lcall Send_SPI
 	mov a, #0xbd
 	lcall Send_SPI
 	mov a, #0xc3
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0x9c
 	mov w+0, #0xa3
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_forty: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x02
 	lcall Send_SPI
 	mov a, #0x5a
 	lcall Send_SPI
 	mov a, #0x66
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xa4
 	mov w+0, #0x06
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_fifty: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x02
 	lcall Send_SPI
 	mov a, #0xfe
 	lcall Send_SPI
 	mov a, #0x6c
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xd0
 	mov w+0, #0xd4
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'

play_sixty: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x03
 	lcall Send_SPI
 	mov a, #0xcf
 	lcall Send_SPI
 	mov a, #0x40
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xa6
 	mov w+0, #0x9e
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_seventy: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x04
 	lcall Send_SPI
 	mov a, #0x75
 	lcall Send_SPI
 	mov a, #0xde
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xad
 	mov w+0, #0xfa
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_eighty: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x05
 	lcall Send_SPI
 	mov a, #0x23
 	lcall Send_SPI
 	mov a, #0xd8
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0x97
 	mov w+0, #0xc1
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_ninety: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x05
 	lcall Send_SPI
 	mov a, #0xbb
 	lcall Send_SPI
 	mov a, #0x99
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xa1
 	mov w+0, #0x1a
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
 	
play_one_hundred: ; Playback a portion of the stored wav file
 	clr TR2 ; Stop Timer 2 ISR from playing previous request
	setb FLASH_CE
 
 	; Set the begining of the sound to play
 	clr FLASH_CE ; Enable SPI Flash
 	mov a, #READ_BYTES
 	lcall Send_SPI
 	mov a, #0x06
 	lcall Send_SPI
 	mov a, #0x5c
 	lcall Send_SPI
 	mov a, #0xb3
 	lcall Send_SPI
 
	; Set how many bytes to play
 	mov w+2, #0x00
 	mov w+1, #0xe7
 	mov w+0, #0xe5
 
 	mov a, #0x00 ; Request first byte to send to DAC
 	lcall Send_SPI
 
 	setb TR2 ; Start playback by enabling timer 2
 	ret ; If this is a subroutine, it must end with 'ret'
END
