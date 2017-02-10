cseg
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
	; Init One millisecond interrupt counter.
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
    clr a
	mov Count1ms_2+0, a
	mov Count1ms_2+1, a
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; PSW logic
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; In mode 1 we need to reload the timer.
	push acc
	push psw
	
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	
	inc Count1ms_2+0    ; Increment the low 8-bits first
	mov a, Count1ms_2+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done_0
	inc Count1ms_2+1

Inc_Done_0:
	
	; Check if half second has passed
	mov a, Count1ms_2+0
	;TODO change value in brackets to be a variable
	
	cjne a, #low(500), Timer0_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms_2+1
	cjne a, #high(500), Timer0_ISR_done
	
	; X milliseconds have passed - turn on or off oven 
	inc temp
	;x milliseconds have not passed, turn off oven
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms_2+0, a
	mov Count1ms_2+1, a
	
Timer0_ISR_done:
	
	pop psw
	pop acc
	reti


;increment counter
;Some logic to enable bit for 100%, 20%, 0% of 1 sec based on variable vs counter

	
INIT_SPI:
	setb MY_MISO ; Make MISO an input pin
	clr MY_SCLK ; For mode (0,0) SCLK is zero
	ret

; Configure the serial port and baud rate using timer 1
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, or risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can safely proceed with the configuration
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
	
    
newline:
	DB  '\r', '\n', 0
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
	cpl P0.3
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

	
	
Inc_Done:
	clr c
	mov a, count1ms+0
	subb a, pwm+0
	mov a, count1ms+1
	subb a, pwm+1
	mov P0.2, c

	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(500), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(500), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	; Reset to zero the milli-seconds counter, it is a 16-bit variable

	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counte	
	;inc secs
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

Save_Configuration:
	; Erase FDATA page 1
	clr EA ; No interrupts please!
	mov MEMCON, #01011000B ; AERS=1, MWEN=1, DMEN=1
	mov DPTR, #0x0000
	mov a, #0xff
	movx @DPTR, A
	; Load page
	mov MEMCON, #00111000B ; LDPG=1, MWEN=1, DMEN=1
	; Save variables
	mov a, my_variable
	movx @DPTR, A
	inc DPTR	
	mov a, #0x55 ; First key value
	movx @DPTR, A
	inc DPTR	
	mov a, #0xAA ; Second key value
	movx @DPTR, A
	mov MEMCON, #00011000B ; Copy page to flash
	mov a, #0xff
	movx @DPTR, A
	mov MEMCON, #00000000B ; Disable further access to data flash
	setb EA ; Re-enable interrupts
	ret

Load_Configuration:
	mov MEMCON, #00001000B ; Enable read access to data flash
	mov dptr, #0x0001 ;First key value location.  Must be 0x55
	movx a, @dptr
	cjne a, #0x55, Load_Defaults
	inc dptr ; Second key value location.  Must be 0xaa
	movx a, @dptr
	cjne a, #0xaa, Load_Defaults
	; Keys are good.  Load saved values.
	mov dptr, #0x0000
	movx a, @dptr
	mov my_variable, a
	mov MEMCON, #00000000B ; Disable further access to data flash
	ret

Load_Defaults: ; Load defaults if keys are incorrect
   mov my_variable, #123
   mov MEMCON, #00000000B ; Disable access to data flash
   ret
   
   Change_8bit_Variable MAC
    jb %0, %2
    Wait_Milli_Seconds(#50)
    jb %0, %2
    jnb %0, $
    jb SHIFT_BUTTON, skip%Mb
    dec %1
    sjmp skip%Ma
skip%Mb:
    inc %1
skip%Ma:
ENDMAC

; Eight bit number to display passed in ‘a’.
Display_Accumulator:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall ?WriteData ; display ASCII
	mov a, b    ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall ?WriteData ; display ASCII
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall ?WriteData ; display ASCII
	ret
	




