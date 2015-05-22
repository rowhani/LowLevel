Title Artman SINH Calculator - Normal_Precision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This is a Very High Precision Calculator Program (VHPCP) developed by Artman Systems Incorporated, headed by Payman Rowhani and Artin Rezaie.
;It will calculate the Sinh(x) where x is a positive integer less than 255.
;The following formula is taken into action:  temp = (anSequence * seqtn) / floatoff  
;where  temp is a temporary variable nedded for calculation and anSequence keeps the
;new and  old values for an sequence and seqtn is X^2 (x is input integer) and float off is 2n(2n+1).
;After each time the temp is caculated, it is moved to anSequence (the new an) and in advance 'total'
;is added with the current temp, so the main formula is an += (an-1 * x^2) / 2n(2n+1)
;You can calcute similar functions like e^x, coshx, sinx, cosx and ... by modifying only the 'KICKOFF' procedure !!!
;By increasing number of 'terms' the precision along with the calculation time will certainly increase
;You can increase the number of fractions by increasing 'farc'
;After you modify 'frac' you should increase the 'output' size too.
;Normal Precision = 2,398 digits precision	Takes seconds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


stackseg segment stack 'stack'
	mstack dw 1000 dup(?)
stackseg ends
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
dataseg segment 'data'
   inputlen db 4
   actlen db ?
   userInput db 4 dup(?)			;needed for input the number
   
   terms equ 1000				;number of terms (n) ? max=65535
   input db ?					;the converted input string
   digits equ 50				;number of digits needed to satisfy the required precision  2^(50*8) = 10^120
   digitstem dw 0				;used for dec to ascii(see below)
   frac equ 300					;number of fractions, no max imaginable!!!, just increase the terms and output
   ten db 10					;a constant for division and multiplication and converting
   counter dw 0  				;counter for counting numner of current terms (n)
   
   welcomemsg db "Sinh(x) calculation developed by Artman!$"
   errormsg db "Error 655: Input must not be greater than 255!$"
   inputmsg db "Please enter a non-negative integer less than 255 inclusive: $"
   
   digits2 equ digits-2  			;needed for mul, temp_size = anSequence_size+seqtn_size
   anSequence db digits2+frac dup(0) 		;contains seqtn*tatal (an=q(an-1))   
   seqtn	db  2	dup(0)			;input^2 or (x^2)   ;the recursive multiplier for each term
   
   remainder db 0				;order is important!
   total db digits+frac dup(0)			;the so-far caculated answer
   
   fremainder db 4+1 dup(0)			;order is important!
   temp db digits+frac dup(0) 			;a temporary var for calculating
   floatoff db 4 dup(0)				;2n(2n+1)  			
   
   temp10 db digits+frac+1 dup(0) 		; temporary var used for converting hex fraction to ascii
   output db 3000 dup(0)   			;used for printing the result    
dataseg ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

codeseg segment 'code'

main proc near
	assume cs:codeseg , ds:dataseg , ss:stackseg 
	mov ax,dataseg
	mov ds,ax
	
		mov ah,9
		mov dx, offset welcomemsg
		int 21h
		mov ah,2	
		mov dl,0dh	;carriage
		int 21h
		mov dl,0ah	;new line
		int 21h	
		mov ah,9
		mov dx, offset inputmsg
		int 21h
		
		mov ah,0ah
		mov dx,offset inputlen			;get the input
		int 21h
		
		call ASCII2DEC				;convert to Decimal
		state: 	
		call kickoff				;this will calculate the recurive multiplier seqtn
		call addToResult			;this will add anSequence to total 
		inc counter
		cmp counter,terms
		jne state			
				
		mov ah,2	
		mov dl,0ah	;new line
		int 21h
		mov dl,0ah	;new line
		int 21h
		mov dl,0dh	;free carriage
		int 21h	
		mov digitstem,digits ;argument
		call DEC2ASCII			;converting the result to ascii	must be called before  FDEC2ASCII
		mov ah,9
		mov dx,offset output
		int 21h
		mov ah,2
		mov dl,'.'
		int 21h	
		add digitstem,frac		;argument
		call FDEC2ASCII			;converting the result to ascii	
		mov ah,9			;print the result
		mov dx,offset output
		int 21h	
		mov ax,4c00h
		int 21h			
main endp

kickoff proc near
		cmp counter,0
		je firstTime				;the first time so that temp=x

		mov al, input
		mul al		;x^2

		xchg al,ah				;right order in the memory
		mov word ptr seqtn,ax
		
		mov bx, counter
		shl bx,1	;2n
		mov ax,bx	;ax=2n

		inc bx		;2n+1
		mul bx		;2n(2n+1)
		xchg al,ah
		xchg dl,dh
		mov word ptr floatoff, dx
		mov word ptr floatoff+2, ax
		
		call multiplication	
		jmp done
		
		firstTime:	
		mov bl,input
		mov temp+digits-1,bl		
		mov floatoff+3, 1		;needed for further division  temp=(anSequence*seqtn)/floatoff  ;  last temp = anSequence*seqtn
		done:  
		ret
kickoff endp

addToResult proc near	
		call floatdiv			;making temp=temp/floatoff	
		call addit				;add the new an to the result
		call cleanup			;moving temp to anSequence and cleaning temp(now anSequence will keep the new 'an' for next operation)
		ret	
addToResult endp

multiplication proc near
		mov si,digits2+frac-1		;anSequence
		For1: 
		mov bx,1 	;seqtn
		For2: 
		mov al,anSequence[si]
		mul seqtn[bx]
		Add temp[bx+si+1],al
		Adc temp[bx+si], ah
	
		pushf	
		mov di,bx   ;mov di,bx+si-1
		Add di,si
		dec di		
		popf
	
		While1: 
		Adc temp[di],0
		dec di
		js check ;check for end condition
		jc While1; check for carry
		
		check:
		dec bx
		jns For2
		dec si
		jns For1
		ret
multiplication endp

addit proc near
		mov bx,frac+digits-1		
		clc	
		agadd:
		mov al,temp[bx]
		adc total[bx],al
		dec bx
		jns agadd
		ret
addit endp

cleanup proc near
		mov bx,frac+digits2-1
		agcl:
		mov al,temp[bx+2]	;+2 change digits2 to digits1 for temp ,the 2 first bytes of temp are ignored
		mov anSequence[bx],al
		dec bx
		jns agcl
		
		mov bx,frac+digits-1
		agclt:	
		mov temp[bx],0
		dec bx
		jns agclt
		
		mov word ptr seqtn,0		
		
		ret
cleanup endp

divideBy10 proc near
		mov ax,digitstem
		mov cl,3
		shl ax,cl
		
		mov cx,ax
		divag:	
		call shiftBit
		call compare
		loop divag	
		ret

		shiftBit:
		mov bx,digitstem 		;digits+1  -1    (remainder+total)
		clc	
		bitShift: 
		rcl remainder[bx],1
		dec bx
		jns bitShift	
		ret
	
		subRem:
		mov bx,digitstem
		dec bx	;(digitstem)-1		
		OR total[bx], 01h
		sub remainder,10	
		ret
	
		compare:
		cmp remainder,10	
		jb endit
		call subRem
		endit:
		ret
divideBy10 endp

Dec2ASCII proc near
		xor si,si
		d2a:
		xor bx,bx
		checkzero:			;check every time to see whether total is zero or not
		cmp total[bx],0
		jne getStartPoint
		inc bx
		cmp bx,digitstem	
		jne checkzero
		getStartPoint:
		cmp bx,digitstem		;if bx equals digits, then'total' is zero
		je reverseRes
		mov remainder,0
		call divideBy10			;divide by ten and put the remainder in the output
		or remainder,030h
		mov al,remainder
		mov output[si],al		
		inc si				;si will count the number of digits
		jmp d2a

		reverseRes:			;reverse the 'output' to let the most significant be the first digit	
		cmp si,0
		jz onest
		xor bx,bx
		push si	;needed for '$'
		dec si
		contrev:
		mov al,output[bx]
		mov ah,output[si]
		mov output[bx],ah
		mov output[si],al
		inc bx
		dec si				;if output is one digit decriment will result in a negative number whcih will not work with 'ja'
		cmp si,bx
		jg contrev
		pop si
		mov output[si],'$'
		jmp goret
		onest:
		mov output,30h
		mov output+1,'$'
		goret:
		ret
Dec2ASCII endp

multemp proc near
		mov di,digitstem
		clte:
		mov temp10[di],0
		dec di		
		jns clte
		
		mov bx,digitstem
		dec bx
		Fort1:
		mov al,total[bx]
		mul ten		
		
		Add temp10[bx+1],al
		Adc temp10[bx], ah
	
		pushf	
		mov di,bx	
		dec di		
		popf
	
		TWhile1: 
		Adc temp10[di],0
		dec di
		js Tcheck ;check for end condition
		jc TWhile1; check for carry
		
		Tcheck:
		dec bx
		jns Fort1	

		mov bx,digitstem
		dec bx
		xchag:
		mov al,temp10[bx+1]
		mov total[bx],al
		dec bx
		jns xchag		
		ret	
multemp endp

FDec2ASCII proc near			;converts fraction from hex to decimal by subsequent multiplications by 10
		xor si,si	
		fd2a:
		mov bx,digits
		fcheckzero:		;check every time to see whether total is zero or not
		cmp total[bx],0
		jne fgetStartPoint
		inc bx
		cmp bx,digitstem	
		jne fcheckzero
		fgetStartPoint:
		cmp bx,digitstem	;if bx equals digits, then'total' is zero
		je gobackf		
		call multemp
		mov al,total+digits-1
		or al,30h
		mov output[si],al
		mov total+digits-1,0
		inc si
		jmp fd2a
		gobackf:
		cmp si,0
		jne putend
		mov output,30h
		inc si
		putend:
		mov output[si],'$'
		ret
FDec2ASCII endp

ASCII2DEC proc near		
		CMP actlen,3
		je convertit
		CMP actlen,2
		je shift2
		CMP actlen,1
		je shift1
		
		MOV userInput,0	;if no input
		MOV userInput+1,0
		MOV userInput+2,0
		jmp convertit
		
		shift2:
		mov al,userInput+1
		mov userInput+2,al
		mov al,userInput
		mov userInput+1,al
		mov userInput,0
		jmp convertit

		shift1:
		mov al,userInput
		mov userInput+2,al	
		mov userInput,0
		mov userInput+1,0
		
		convertit:	
		
		cmp userInput,32h		;this will chek that input is not greater than 255
		ja  errorinput
		jne okdo
				
		cmp userInput+1,35h
		ja errorinput
		jne okdo
		
		cmp userInput+2,35h
		ja errorinput			;validity check is done
		
		okdo:		
		xor bx,bx				
		a2d:
		and userInput[bx], 00Fh
		mov dl,userInput[bx]
		add input, dl
		inc bx
		cmp bx,3			;maximum input is a 3-digit number(1 byte)
		je fins
		mov al,input
		mul ten
		mov input, al
		jmp a2d
			
		fins:
		ret
ASCII2DEC endp

floatdiv proc near
		mov fremainder,0
		mov fremainder+1,0
		mov fremainder+2,0
		mov fremainder+3,0
		mov fremainder+4,0
		
		mov cx,8*(frac+digits)		;temp size
		fstartAgain:
		Call fshiftBit
		call fcompare
		loop fstartAgain	
		ret

		fshiftBit: 
		Mov bx,frac+digits+4		;(frac+digits)+remainder size  -1
		clc	
		fbitShift: 
		rcl fremainder[bx],1
		dec bx
		jns fbitShift	
		ret
		
		fsubRem:
		mov bx,frac+digits-1		;frac+2 -1
		OR temp[bx], 01h	
		mov bx,3			;floatoff size - 1
		clc	
		fsubit:
		mov al,floatoff[bx]
		sbb fremainder[bx+1],al
		dec bx
		jns fsubit
		sbb fremainder,0		;fremainder has one more byte
		ret
		
		fcompare: 
		cmp fremainder,0
		jne fsublbl
		
		xor si,si
		fcompst: 
		mov dl,fremainder[si+1]
		cmp dl,floatoff[si]
		jb fendit
		ja fsubLbl
		inc si     ;if equal
		cmp si,4
		jne fcompst	
		fsubLbl:
		call fsubRem
		fendit:
		ret
floatdiv endp

errorinput proc near
		mov ah,2		;print the result
		mov dl,0ah		;new line
		int 21h
		mov dl,0dh		;free carriage
		int 21h	
		mov ah,9
		mov dx,offset errormsg
		int 21h
		mov ax,4c00h
		int 21h
errorinput endp

codeseg ends

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end main

;;Copyright (c)  2004-2007 Artman(R) Systems Incorporated and its licensors. All rights reserved.