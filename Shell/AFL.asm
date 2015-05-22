TITLE ARTMAN MS-DOS OLD-STYLE EXECUATBLE-FILE LOADER 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This program is a private shell for loading and executing exe files developed by Artman Systems Inc.
;;It starts loading executable files by primarily loading the header and then allocating the needed space
;;and copying the program to the allocted space and changing the replaceable items' relative addresses to the real addresses.
;;MAIN procedure reads the PSP to get the argument passed by the user in the command line and then copy it to the ASCIIZ
;; as the file name and path. It also reads another argument separated by a space to copy it to the new program's PSP.
;;Then it calls the LOAD procedure which is responsiblefor loading the header and checking the validity of the exe file and
;;calculating the file size minus the header size. Then ALLOCATE procedure is called by  MAIN. This time the needed space
;;according to the real file size is allocated using ms-dos interrupt. Note that due to the insuffisient space supplied by dos,
;;we need to set space in some arbitrary (but not dangerous!) places in the memory. This procedure copies the rest of file in
;;this buffer. Afterwards Execute procedure is called by MAIN. It is responsible for calculating CS, SS, SP ... and altering
;;the replaceable items' address ,determined by the guide table in the header, to the real addresses. It also sets the PSP
;;Now MAIN can set all the segment and data registers to the achieved values and run the program by a far jump.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


STACKSEG SEGMENT STACK 'STACK'
	MSTACK DW 100 DUP(?)
STACKSEG ENDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATASEG SEGMENT 'DATA'
	ASCIIZ DB 128 DUP(0)		;keeps the path and file name of the exe file.
	ARG DB 80H DUP(0)			;keeps the argument of the exe file.
	HANDLE DW ?					;a handle for the exe file
	FILE_BUFFER DW ?			;the buffer where the file is to be loaded into
	DST DW ?					;data segment
	EST DW ?					;extra segment
	
	HEADER_BUFFER LABEL WORD	;The buffer for the header. The data below are placed in order based on the exe file header.
	EXE_MARKER DW ?				;5A 4D the exe files remark
	LAST_BLK DW ?				;number of bytes in the last block of an exe file
	FILE_SIZE DW ?				;total file size in 512-byte blocks 
	REALLOCATABLE_EL_NUM DW ?	;number of replaceable items (like dataseg and codeseg)
	HEADER_SIZE DW ?			;the size of the header in 16-byte paragraphs
	MIN_PARA DW ?				;minimum number of paragraphs needed to be set above the end of the program
	UP_DOWN_STC DW ?			;FFFF or 0000 a switch for up or down loading
	SST DW ?					;stack segment
	SPT DW ?					;stack pointer
	VALIDITY_BYTES DW ?			;checksum
	IPT DW ?					;IP
	CST DW ?					;code segment
	TABLE_STR_ADDRESS DW ?		;start of the replaceable items' guide table relative to the start of the header
	OVERLAP_NUM DW ?			;overlap number
	REP_TABLE DW ?				;number of replaceable items (just before the table)
	REST_HD DW 241 DUP(?)		;rest of the header bytes containing the table or zero
		
	PROMPT DB " ARTMAN SYSTEMS INC.",10,13," MS-DOS OLD-STYLE EXECUTABLE-FILE LOADER VER. 1.1",10,13,10,13,
		"   USAGE:     AFL [drive:][path][filename]",10,13,'$'
	FERROR DB "FATAL ERROR 414: FILE NOT FOUND OR INSUFFICIENT MEMORY TO RUN THE PROGRAM.$"
	NXERROR DB "FATAL ERROR 219: INVALID EXECUTABLE FILE WAS DETECTED.$"
DATASEG ENDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CODESEG SEGMENT 'CODE'


ASSUME CS:CODESEG, DS:DATASEG, SS:STACKSEG, ES:DATASEG

MAIN PROC FAR
	MOV AX, DATASEG
	MOV DS,AX
	
	XOR CH,CH
	MOV CL, ES:[80H]
	CMP CL, 1				;if no argument is entered, go back
	JBE SHPRT
							
	MOV BX,82H				;copying the loader argument to the ASCIIZ
	LEA DI,ASCIIZ
	DEC CX	
	COPAZ:
	MOV AL, ES:[BX]
	CMP AL, ' '				;using space as a seperator between the loader and the exe file arguments
	JE ARGUMENT
	MOV [DI], AL
	INC BX
	INC DI
	LOOP COPAZ
	JMP GETTOPRG
	
	ARGUMENT:
	MOV ARG, CL				;copying the exe file argument to the ARG
	LEA DI, ARG+1
	COPAG:
	MOV AL,ES:[BX]
	MOV [DI],AL
	INC BX
	INC DI
	LOOP COPAG	
	
	MOV BYTE PTR[DI], 0DH
	
	GETTOPRG:
	PUSH DS					;setting es to ds
	POP ES
	
	CALL LOAD
	CALL ALLOCATE
	CALL EXECUTE
	
	PRG_ADDRESS DD ?					;a variable, but not placed in dataseg (since how can  i set ds while i need prg_address!)
										
	MOV CX, FILE_SIZE					;reading and setting the registers
	MOV AX, SST
	MOV SS, AX
	MOV AX, SPT
	MOV SP, AX
	MOV AX, CST
	MOV WORD PTR PRG_ADDRESS[2], AX		;setting cs:ip to PRG_ADDRESS
	MOV AX, IPT
	MOV WORD PTR PRG_ADDRESS, AX
	MOV AX, DST
	MOV DS, AX
	MOV ES, AX
	XOR AX,AX							;setting data registers to zero
	XOR BX, BX
	XOR DX, DX
	XOR SI,SI
	XOR DI, DI
	JMP PRG_ADDRESS						;jumping to cs:ip to run the program
	
	SHPRT:								;if no argument is set the program will prompt a message and exit.
	MOV AH,9
	LEA DX, PROMPT
	INT 21H
	MOV AX,4C00H
	INT 21H
MAIN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ALLOCATE PROC FAR	
	;MOV AH, 48H							;allocating space using function 48h of int 21h
	;MOV BX, FILE_SIZE
	;MOV CL,4
	;SHR BX, CL	;SIZE IN PARAGRAPHS
	;INT 21H
	;JC ERRORPRN
	;MOV FILE_BUFFER, AX
	MOV FILE_BUFFER, 1F00H				;beacause of insufficient memory allocated by ms-dos, we need to allocate some where
										;in the memory without ms-dos authorization
	
	MOV AH,3FH							;reading the rest of file (bytes followed by the header)
	MOV BX,HANDLE
	MOV CX, FILE_SIZE
	ADD FILE_BUFFER, 10H				;look at EXECUTE procedure for more info
	PUSH DS
	PUSH AX
	MOV AX, FILE_BUFFER
	MOV DS, AX							;the file should be opended in the allocated buffer so we need to change  DS to FILE_BUFFER
	MOV DX, 0							;from the start of the FILE_BUFFER
	POP AX
	INT 21H
	POP DS
	JC ERRORPRN
	CMP AX, 0
	JE ERRORPRN							;zero byte
	
	RET
ALLOCATE ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOAD PROC FAR
	MOV AH, 3DH							;opening the file
	MOV AL, 0
	LEA DX, ASCIIZ
	INT 21H
	JC ERRORPRN
	MOV HANDLE, AX
	
	MOV AH,3FH							;reading 512 byte as the approximate header size
	MOV BX,HANDLE
	MOV CX, 512
	LEA DX, HEADER_BUFFER
	INT 21H
	JC ERRORPRN
	CMP AX, 0
	JE ERRORPRN							;zero byte
	
	CMP EXE_MARKER, 5A4DH				;checks to see whether the file is a valid executable file or not, 5A4D = ZM
	JNE INXE
	
	MOV AX, HEADER_SIZE
	MOV BX, 16
	MUL BX		
	MOV HEADER_SIZE, AX					;header_size in bytes
	
	MOV AX, FILE_SIZE					;calculating the file_size
	DEC AX
	MOV BX, 512
	MUL BX
	ADD AX, LAST_BLK
	SUB AX, HEADER_SIZE
	MOV FILE_SIZE, AX					;the filesize without headrer size in bytes
	
	RET
	
	INXE:								;if the file is not valid, prints an error message and exits.
	MOV AH,9
	LEA DX, NXERROR
	INT 21H
	MOV AX,4C00H
	INT 21H
LOAD ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EXECUTE PROC FAR
	MOV AX, FILE_BUFFER
	ADD SST, AX
	ADD CST, AX
	SUB AX, 10H							;in fact file buffer is a word:0 (code segment is 20 buts)  and we should add 100h(psp) with file_size:0 and 
	MOV DST, AX							;then shift it to left four times but we can add file_size with 10h instead
	MOV EST, AX
	
	
	PUSH ES
	MOV ES,DST
	XOR BX,BX
	FILLZ:								;setting PSP to all zero
	MOV BYTE PTR ES:[BX], 0
	INC BX
	CMP BX,100H
	JNE FILLZ
	
	LEA SI, ARG							;copying from ARG to the exe file PSP offset 80H
	MOV BX,80H
	FILLARG:
	MOV AL, [SI]
	CMP AL, 0
	JE ARGSGOTTEN
	MOV ES:[BX], AL
	INC BX
	INC SI
	JMP FILLARG
	
	ARGSGOTTEN:
	POP ES
	
	MOV DX, REALLOCATABLE_EL_NUM		;calculating number of replaceable items in bytes (multiplying by 4 as each entry is 4 bytes)
	SHL DX,1							;(the first 2 are offsets needed to be added to the DST(start of the PSP) and the other 2 are stack segment values)
	SHL DX,1							
	XOR SI, SI	
	
	MOV CX, FILE_BUFFER
	NEXTENT:
	MOV BX, TABLE_STR_ADDRESS			;go to the start of the table
	MOV AX, HEADER_BUFFER[BX+SI]		;reading 4 bytes each time
	MOV BX, AX
	PUSH DS
	MOV AX, CST							;changing DS to CST as the code should be changed
	MOV DS, AX	
	MOV AX, [BX]						;getting the item in the CST offset by the value read in the table
	ADD AX, CX							;adding DS offset to the value
	MOV [BX], AX
	POP DS
	CMP UP_DOWN_STC, 0FFFFH				;if it is a down stack
	JNE UPSTACK
	ADD SI, 4
	COMMONI:
	CMP SI, DX
	JNE NEXTENT
	JMP EXFI
	
	UPSTACK:
	ADD SI,2							;add stack value in the table to ss if it is an up stack
	MOV AX, TABLE_STR_ADDRESS[SI]
	ADD SST, AX
	ADD SI,2
	JMP COMMONI
	
	EXFI:
	RET
EXECUTE ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ERRORPRN PROC FAR								
	MOV AH,9							;PRINT ERROR message if any problem occured while opening or reading the file or space allocating
	LEA DX, FERROR
	INT 21H
	MOV AX, 4C01H
	INT 21H
ERRORPRN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CODESEG ENDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

END MAIN

;;Copyright (c)  2004-2007 Artman(R) Systems Incorporated and its licensors. All rights reserved.