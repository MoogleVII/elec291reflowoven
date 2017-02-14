; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP52

$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
BAUD          equ 115200
T1LOAD        equ (0x100-(CLK/(16*BAUD)))
TIMER0_RATE   EQU 4096 ;1000hz for timer tick of 1ms 
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON  		 equ P4.5
SOUND_OUT    		 equ P3.7
	
;CONTINUE    	     equ P0.0
restart_button       equ P0.1
increment_button     equ P0.5
SETBUTTON 		     equ P2.4
;SHIFT_BUTTON         equ P2.6

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
x:   		  ds 4
y:   		  ds 4
bcd: 		  ds 5
Count1ms:     ds 2 ; Used for timer 2
Count1ms_2:   ds 2 ; used for timer 0
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
my_variable:  ds 1 
state:        ds 1
pwm:		  ds 2
sec:		  ds 1
power_time:   ds 2 ; to set how much 
 ;for oven power to be on
tempreal :    ds 1 ; converted temperateure from ADC
temp:		  ds 1
oven: 		  ds 1
temp1_ideal:  ds 2
result:		  ds 4
time:		  ds 2

state1tempVal:ds 1
state2secVal:ds 1
state3tempVal:ds 1
state4secVal:ds 1
state5tempVal:ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
mf: 			   dbit 1
load_time_flag:    dbit 1

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
$include(justin.inc)
$include(math32.inc)
$include(davin.inc) ; A library of LCD related functions and utility macros
;$include(setSTUFF.inc) ; setting the time and temps relating to the reflow oven
$LIST
;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'State           ', 0
Line_2:           db 'oven            ', 0

Set_temp_prompt:  db 'Set Temp(T) or load mem(L)     ', 0

;                     
T:  db 'T', 0
;                     
M:  db 'M', 0
CLEAR: db '                         ', 0

SETTEMP1: db 'Set Temp 1:',0
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;


;Rading average from ADC

Average_ADC_Channel MAC 
	mov b, #%0 
	lcall ?Average_ADC_Channel 
ENDMAC
	?Average_ADC_Channel: 
	Load_x(0) 
	mov R5, #100
	Sum_loop0: 
	;Read_ADC_Channel(#0) 
	mov y+3, #0
	mov y+2, #0
	mov y+1, R7 
	mov y+0, R6 
	lcall add32 
	djnz R5, Sum_loop0 
	load_y(100) 
	lcall div32 
	ret
;-------



main:
	
	; Initialization
    mov SP, #0x7F
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall LCD_4BIT
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA 	;Enable Global interrups
    lcall InitSerialPort
    setb half_seconds_flag
	mov state, #0
	mov pwm+0, #low(0) 		;initialize pwm to 0% 
	mov pwm+1, #high(0)	
	mov oven, #0 			;dummy variable for testing
	mov temp, #125			; 		" 		"
	mov sec, #0				;timer for states
	mov time, #0			;timer for whole program
	
	

lcall Load_Configuration
	
	
	;///////RYAN + DAVIN ////////////
	;///////IMPLEMENTATION///////////
	;///////OF SETTING	 ////////////  
	;///////VARIABLES////////////////
	Set_Cursor(2,1)
	Send_Constant_String(#state1temp)
	ComebackState1:
	Set_Cursor(2,14)
	increment(increment_button, state1tempVal)
	lcall Save_Configuration
	jb SETBUTTON, ComebackState1 
	;now state1temp is set
	wait_milli_seconds(#255)
	
	Set_Cursor(2,1)
	Send_Constant_String(#state2sec)
	Wait_Milli_Seconds(#100)
	Wait_Milli_Seconds(#100)
	ComebackState2:
	Set_Cursor(2,14)
	increment(increment_button, state2secVal)
	jb SETBUTTON, ComebackState2
	lcall Save_Configuration
	;now state2sec is set
	wait_milli_seconds(#255)
	
	Set_Cursor(2,1)
	Send_Constant_String(#state3temp)
	Wait_Milli_Seconds(#100)
	Wait_Milli_Seconds(#100)
	ComebackState3:
	Set_Cursor(2,14)
	increment(increment_button, state3tempVal)
	jb SETBUTTON, ComebackState3
	lcall Save_Configuration
	;now state3 temperature is set
	wait_milli_seconds(#255)
	
	Set_Cursor(2,1)
	Send_Constant_String(#state4sec)
	Wait_Milli_Seconds(#100)
	Wait_Milli_Seconds(#100)
	ComebackState4:
	Set_Cursor(2,14)
	increment(increment_button, state4secVal)
	jb SETBUTTON, ComebackState4
	lcall Save_Configuration
	;now state4 seconds is set
	wait_milli_seconds(#255)
	
	Set_Cursor(2,1)
	Send_Constant_String(#state5temp)
	Wait_Milli_Seconds(#100)
	Wait_Milli_Seconds(#100)
	ComebackState5:
	Set_Cursor(2,14)
	increment(increment_button, state5tempVal)
	lcall Save_Configuration
	jb SETBUTTON, ComebackState5	
	wait_milli_seconds(#255)	
	
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
	Set_Cursor(2, 1)
    Send_Constant_String(#Line_2)





	; After initialization the program stays in this 'forever' loop
loop:

	
;;;;;;;;get thermocouple reading;;;;;;;;;;;;;;;;;;;;;;;;;
	clr CE_ADC
	mov R0, #00000001B ; Start bit:1
	lcall DO_SPI_G
	mov R0, #10000000B ; Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1 ; R1 contains bits 8 and 9
	anl a, #00000011B ; We need only the two least significant bits
	mov Result+1, a ; Save result high.
	mov x+1, a
	mov R0, #55H ; It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1 ; R1 contains bits 0 to 7. Save result low.
	mov x, R1
	setb CE_ADC
	mov x+2, #0
	mov x+3, #0
	Wait_milli_seconds(#255)
	
	
	;Average_ADC_Channel(#100)
	
;;;;;math
	load_Y(50000)
	lcall mul32
	load_Y(1023)
	lcall div32 ;Causes weird delay for PWM
	load_Y(128)
	lcall div32
	load_Y(23)
	lcall add32
	

;//////////////math ends//////////////////////////////////////////////////////////////////////
	lcall hex2bcd
	mov tempreal+0, bcd+0
	mov tempreal+1, bcd+1


;;;;///////////////////////////////////////////DISPLAYING TO PUTTY;///////////////////////////////
	send_bcd(tempreal+1)
	send_bcd(tempreal+0)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
;/////////////////////////////////////////////////////////////////////////////////////////////////
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	

	Set_Cursor(2, 7)
	mov a, tempreal+0
	;lcall Display_Accumulator
	Display_BCD(tempreal+0)
	Set_Cursor(2, 5)
	mov a, tempreal+1
	Display_BCD(tempreal+1)
	
	;lcall Display_Accumulator
	
	Set_Cursor(1 , 7)
	mov a, state ;state keeps track of the state# we are in
	lcall Display_Accumulator
	
	Set_Cursor(2, 14)
	clr a
	mov a, time 
	lcall Display_Accumulator
	
	
	Set_Cursor(1, 14)
	clr a
	mov a, sec
	lcall Display_Accumulator
	
	mov a, state
state0:
	
	cjne a, #0, state1
	mov sec, #0
	mov time, #0
	mov pwm+0, #0
	mov pwm+1, #0	
	jb P0.1, state0_done
	Wait_milli_seconds(#50)
	jb P0.1, state0_done
	jnb P0.1, $
	mov state, #1	
	cpl TR0
state0_done:
	ljmp loop
state1: ;cmp temp
	clr TR0
	cjne a, #1, state2
	mov pwm+0, #low(1001) ;100%duty cycle (500/500ms = 100%)
	mov pwm+1, #high(1001) 
	jb P0.1, keepgo1
	Wait_milli_seconds(#50)
	jb P0.1, keepgo1
	jnb P0.1, $
	mov state, #0
keepgo1:
	clr c 
	clr a 
	mov a, sec
	subb a,#60 ;compare whether temp is 50 when it's 60sec
	jc keepgoing
isfifty:
	clr c 
	clr a 
	mov a, #50
	subb a, x 
	jnc state0 ;if at 60s, the temp is not 50, then go to state0 
keepgoing:		
	clr c
	clr a
	mov a, state1tempval
	subb a, x
				
	jnc state1_done
	mov state, #2
	mov sec, #0    ;set timer to 0 for comparison in next state
	cpl TR0
state1_done:
	ljmp loop
state2: ;cmp time
	clr TR0
	cjne a, #2, state3
	mov pwm+0, #low(200) ; 20% duty cycle (100/500ms = 20%)
	mov pwm+1, #high(200)
	jb P0.1, keepgo2
	Wait_milli_seconds(#50)
	jb P0.1, keepgo2
	jnb P0.1, $
	mov state, #0
keepgo2:
	mov a, state2secVal				
					
	clr c
	subb a, sec

	jnc state2_done
	mov state, #3
	mov sec, #0
	cpl TR0
state2_done:
	ljmp loop
state3: ;cmp temp
	clr TR0
	cjne a, #3, state4
	mov pwm+0, #low(1001) ;100%duty cycle
	mov pwm+1, #high(1001)
	
	jb P0.1, keepgo3
	Wait_milli_seconds(#50)
	jb P0.1, keepgo3
	jnb P0.1, $
	mov state, #0
keepgo3:	
	clr c
	mov a, state3tempVal 
	subb a, x					
	jnc state3_done
	mov state, #4
	mov sec, #0
	cpl TR0
state3_done:
	ljmp loop
state4: ;cmp time
	clr TR0
	cjne a, #4, state5
	mov pwm+0, #low(200)
	mov pwm+1, #high(200)
	jb P0.1, keepgo4
	Wait_milli_seconds(#50)
	jb P0.1, keepgo4
	jnb P0.1, $
	mov state, #0
keepgo4:
	mov a, state4secVal
	clr c
	subb a, sec
	
	jnc state4_done
	mov state, #5
	cpl TR0
	mov sec, #0
	
state4_done:
	ljmp loop
state5: ;cmp temp
	mov pwm+0, #0 ;Disable oven to let cooling happen
	mov pwm+1, #0
	jb P0.1, keepgo5
	Wait_milli_seconds(#50)
	jb P0.1, keepgo5
	jnb P0.1, $
	mov state, #0
keepgo5:
	clr c
	mov a, state5tempVal
	subb a, x 
	jc state5_done
	
	mov state, #0
	clr TR0
state5_done:
	ljmp loop
	
		
	
END