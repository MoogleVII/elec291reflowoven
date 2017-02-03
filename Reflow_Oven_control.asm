$MODLP52
org 0000H
   ljmp MainProgram
   
org 0x002B
	ljmp Timer2_ISR
	
DSEG at 30H

x:   ds 4
y:   ds 4
bcd: ds 5
Count1ms:     ds 2

BSEG
mf: dbit 1
seconds_flag: dbit 1


$NOLIST
$include(math32.inc)
$LIST

CLK  EQU 22118400
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))

TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

; These ’EQU’ must match the wiring between the microcontroller and ADC
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3
;Set some pin for breaker box control -  plot with py to test

CSEG

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret


;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know half second had passed

	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
Timer2_ISR_done:
	pop psw
	pop acc
	reti	
	
INIT_SPI:
	setb MY_MISO ; Make MISO an input pin
	clr MY_SCLK  ; For mode (0,0) SCLK is zero
	ret

; Configure the serial port and baud rate using timer 1
InitSerialPort:
    ;Since the reset button bounces, we need to wait a bit before
    ;sending messages, or risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ;Now we can safely proceed with the configuration
	clr	TR1
	anl	TMOD, #0x0f
	orl	TMOD, #0x20
	orl	PCON,#0x80
	mov	TH1,#T1LOAD
	mov	TL1,#T1LOAD
	setb TR1
	mov	SCON,#0x52
    ret

	

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
    
newline:
	DB  '\r', '\n', 0

MainProgram:
    mov SP, #7FH 		; Set the stack pointer to the begining of idata
    mov PMOD, #0 		; Configure all ports in bidirectional mode
    lcall Timer2_Init   ;init timer 2
    setb EA   			; Enable Global interrupts
    lcall InitSerialPort
   	setb seconds_flag   ; initialize flag to 1 for instant first loop
   	
loop:    
    jnb seconds_flag, loop ;only send to serial every 1 second
    
Read_Write:
	;TODO: Define some pin = OVEN
	;connect pin to breaker box
	; formula simplification: only set y points to be boundary positions
	; ie one for first increase, one for stable point, one for second increase
	; one for second stable point.
	;change y to next step at each time signature (60sec,180sec,etc)
		
	clr seconds_flag
	mov R1, #0x00   
    lcall DO_SPI_G 
    
    mov R1, #0
    Read_ADC_Channel(0) ;Read serial port from ADC channel 0

    mov x+0, R1			;Load X with low 8 bits of data (only 8 bits exist)
    mov x+1, #0
    mov x+2, #0
    mov x+3, #0

    ;mov y+0, (formula go here)   ; Set y to expected value of temperature
	;mov y+1, #0
	;mov y+2, #0
	;mov y+3, #0
	    
    ;Compare the actual temp to expected temp
	lcall x_lt_y    
    jb mf, OVEN_ON 	;If actual < expected turn on oven
	;cpl OVEN (??)
    lcall x_gt_y
    jb mf, OVEN_OFF	;If actual > expected turn off oven
    jmp OVEN_STAY

OVEN_ON:
	;setb OVEN
	jmp OVEN_STAY
OVEN_OFF: 
	;clr OVEN   
OVEN_STAY:        
	
	;Print value in x to serial (may need to convert to 
			;temperature if we are using voltage in main calculation
    lcall hex2bcd		;convert x to BCD for display - result stored in BCD
    send_bcd(bcd)		;write value of x to serial port
    mov DPTR, #newline  ;newline for readability by python
    lcall SendString		
    ljmp loop			

;----------------------------------------------;    
;Read serial port, 1 bit at a time, store in R1
;----------------------------------------------;
DO_SPI_G:
	push acc
	mov R1, #0 ; Received byte stored in R1
	mov R2, #8 ; Loop counter (8-bits)
	DO_SPI_G_LOOP:
	mov a, R0 ; Byte to write is in R0
	rlc a ; Carry flag has bit to write
	mov R0, a
	mov MY_MOSI, c
	setb MY_SCLK ; Transmit
	mov c, MY_MISO ; Read received bit
	mov a, R1 ; Save received bit in R1
	rlc a
	mov R1, a
	clr MY_SCLK
	djnz R2, DO_SPI_G_LOOP
	pop acc
	ret
	

END


