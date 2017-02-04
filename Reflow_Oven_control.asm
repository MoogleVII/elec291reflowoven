; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP52

; asodfjadjgflsjdflgjs;llksg

;asdfadfafd 

$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))
TIMER0_RATE   EQU 1000 ;1000hz for timer tick of 1ms 
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0
SHIFT_BUTTON  equ P0.7
MY_VARIABLE_BUTTON  equ P0.4
restart_button equ P0.1


; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
x:   ds 4
y:   ds 4
bcd: ds 5
Count1ms:     ds 2 ; Used for timer 2
Count1ms_2:   ds 2 ; used for timer 0
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
my_variable:  ds 1 ;
state:        ds 1
pwm:		  ds 1
sec:		  ds 1
power_time:   ds 2 ; to set how much time for oven power to be on
temp:		  ds 2
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
mf: dbit 1

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

$NOLIST
$include(math32.inc) ; A library of LCD related functions and utility macros
$LIST
;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'BCD_counter: xx ', 0
Line_2:           db 'my_variable: xxx', 0

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
	cjne a, #low(1000), Timer0_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms_2+1
	cjne a, #high(1000), Timer0_ISR_done
	
	; 1000 milliseconds have passed.  
	
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

	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
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
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
	;inc sec
	
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

; Eight bit number to display passed in a.
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

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
	Set_Cursor(2, 1)
    Send_Constant_String(#Line_2)
    setb half_seconds_flag
	mov BCD_counter, #0x00
	
	mov state, #0
	
	lcall Load_Configuration
	Set_Cursor(2, 14)
	mov a, my_variable
	lcall Display_Accumulator
	
	; After initialization the program stays in this 'forever' loop
loop:
	Change_8bit_Variable(MY_VARIABLE_BUTTON, my_variable, loop_c)
	Set_Cursor(2, 14)
	mov a, my_variable
	lcall Display_Accumulator
	lcall Save_Configuration
	mov a, state ;state keeps track of the state# we are in
state0:
	cjne a, #0, state1
	mov pwm, #0
	jb P0.1, state0_done
	Wait_milli_seconds(#50)
	jb P0.1, state0_done
	jnb P0.1, $
	mov state, #1
state0_done:
	ljmp loop
state1: ;cmp temp
	cjne a, #1, state2
	mov pwm, #100 ;100%duty cycle
	mov sec, #0
	mov a, #150 ;change to memory temp at state1
	clr c
	subb a, temp ;temp is real time reading from port. if temp greater than a, doens't set carry c, c=0
	jnc state1_done ;come out of this state if a<temp, stay in state 1
	mov state, #2
state1_done:
	ljmp loop
state2: ;cmp time
	cjne a, #2, state3
	mov pwm, #20
	mov a, #60
	clr c
	subb a,sec
	jnc state2_done
	mov state, #3
state2_done:
	ljmp loop
state3: ;cmp temp
	cjne a, #3, state4
	mov pwm,#100
	mov sec, #0
	mov a, #220 ;
	clr c
	subb a, temp ; 
	jnc state3_done
	mov state, #4
state3_done:
	ljmp loop
state4: ;cmp time
	cjne a, #4, state5
	mov pwm, #20
	mov sec, #45
	clr c
	subb a, sec
	jnc state4_done
	mov state, #5
state4_done:
	ljmp loop
state5: ;cmp temp
	cjne a, #5, state0
	mov pwm, #0
	mov a, #60
	clr c
	subb a, temp ;if 
	jc state5_done ;
	mov state, #0
state5_done:
	ljmp loop


loop_c:	
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov BCD_counter, a
	setb TR2                ; Start timer 2
	ljmp loop_b             ; Display the new value
	
loop_a:
	jnb half_seconds_flag, loop
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
    ljmp loop


		
END
