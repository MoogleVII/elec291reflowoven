cseg




; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30




; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!


;                     1234567890123456    <- This helps determine the location of the counter
state1temp:		  db 'State1temp:', 0
state2sec:		  db 'State2sec :', 0
state3temp:		  db 'State3temp:', 0
state4sec:		  db 'State4sec :', 0
state5temp:		  db 'State5temp:', 0


; // - - - - - - - - - - - Macros - - - - - - - - - - - - - - //
; Parameters: (Button, variable)
;	Set_Cursor(2, 14)
; 	Used the %mb to append variable name to label to prevent error of redefining label name



increment MAC
	jb %0, skip_inc%Mb
	Wait_Milli_Seconds(#200)	
	mov a, %1
	inc a
	mov %1, a
	skip_inc%Mb:
	mov a, %1
	lcall Display_Accumulator
endmac

save mac
	inc DPTR
	mov a, %0
	movx @DPTR, A
endmac

load mac
	inc dptr
	movx a, @dptr
	mov %0, a
endmac

Save_Configuration:
	; Erase FDATA page 1
	clr EA ; No interrupts please!
	mov MEMCON, #01011000B ; AERS=1, MWEN=1, DMEN=1
	mov DPTR, #0x0000
	mov a, #0xff
	movx @DPTR, A
	; Load page
	mov MEMCON, #00111000B ; LDPG=1, MWEN=1, DMEN=1
	
	; Save Keys at beginning of memory
	mov DPTR, #0x0000
	mov a, #0x55 ; First key value
	movx @DPTR, A
	inc DPTR	
	mov a, #0xAA ; Second key value
	movx @DPTR, A
	
	
	; Saving Variables
	save(state1tempVal)
	save(state2secVal)
	save(state3tempVal)
	save(state4secVal)
	save(state5tempVal)
	
	
	mov MEMCON, #00011000B ; Copy page to flash
	mov a, #0xff
	movx @DPTR, A
	mov MEMCON, #00000000B ; Disable further access to data flash
	setb EA ; Re-enable interrupts
	ret

Load_Configuration:
	; Enable read access to data flash
	mov MEMCON, #00001000B 
	
	; Check Keys at Location 0x0000
	mov dptr, #0x0000 
	movx a, @dptr
	cjne a, #0x55, Load_Defaults
	inc dptr 
	movx a, @dptr
	cjne a, #0xaa, Load_Defaults
	
	; Keys are good.  Load saved values
	load(state1tempVal)
	load(state2secVal)
	load(state3tempVal)
	load(state4secVal)
	load(state5tempVal)
		
	mov MEMCON, #00000000B ; Disable further access to data flash
	ret

Load_Defaults: ; Load defaults if keys are incorrect
   	mov state1tempVal, #150
	mov state2secVal, #60
	mov state3tempVal,#220
	mov state4secVal, #45
	mov state5tempVal, #60
   mov MEMCON, #00000000B ; Disable access to data flash
   ret

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
	







;forever:
;ljmp forever
	
$list