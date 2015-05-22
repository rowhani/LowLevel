TITLE ARTMAN HARD DISK READER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This program is a fat-16 hard-disk reader developed by Artman Systems Inc.
;;MAIN procedure starts by reading the command line argument as the file path and name.and checking a switch to alternate between
;;ASCII and Binary printing actions. Then it calls DRIVE_MANAGER to determine the drive. This procedure convert the letter to a number
;;i.e. c to 0 and d to 1 and so on. PARTITION_READER is called by the DRIVE_MANAGER to load the partition data (MBR). Number of primary
;; partitions are read to compare with the converted letter; if the drive is less than this number, it multiplies the drive number by 16 to get the
;; partition information from the partition table. If it is greater: the number of primary partitions is  subtracted from driver number to get the 
;;number of times to go to the nested extended partitions. Note that the start of logical partions is relative to the start of the extended partitions,
;;so the start of every  extended partition is added to the variable EXTST. If the drive is found, the MAIN calls BOOT_RECORD_READER
;; to load the boot sector data for the drive. Then SET_INPUT is called for the first time. This procedure is responsible for tokenizing the PATH
;;elements and setting DIR_FLAG to specify whether the token is a file or directory. There is a ROOT_FLAG which tells the CHECK_ENTRY to decide
;;to search the token in the root or in a subdirectory. ROOT_FLAG is set to 1 for the first time and then to zero for next times.
;;CHECK_ENTRY calls DIR_INT or FILE_READER depending on the ROOT_FLAG. DIR_INT will search in the directory entries to find the token.
;;FILE_READER reads a file content. If the start cluster is zero the file is empty, otherwise it loads the cluster using CLUSTER_FINDER.
;;This function converts the current cluster number to a logical sector number and pass it to the LOGICAL_TO_PHYSICAL_SECONT to convert the 
;;logical sector into a physical sector and cylinder and head and then reads n sector (n = sectors per cluster). It uses DIVISION procedure for its divisions.
;;After that FILE_READER reads 1 cluster, it checks that if DIR_FLAG is 1 (subdirectory) to call CHANGE_DIRECTORY or else it calls the PRINT_BUFFER
;;to print the cluster data. CHANGE_DIRECTORY reads the Buffer to find the next tokenized element (finding some item in a subdirectory).
;;When one cluster is read, FILE_READER calls FAT_CLUS_ENT to find the next cluster in the FAT table. FAT_CLUS_ENT finds the FAT sectors
;;and loads them using LOGICAL_TO_PHYSICAL_SECONT and sets the next cluster. Each time cluster value is compared with FFFF to see whether the file
;;is fisished or not. MAIN keeps calling SET_INPUT and CHECK_ENTRY until the end of file is reached. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


STACKSEG SEGMENT STACK 'STACK'
	MSTACK DW 100 DUP(?)
STACKSEG ENDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DATASEG SEGMENT 'DATA'
		;MBR DATA
       	MBR DB 1BEH DUP(?)				;partition table is at the 1BEH offset of the MBR
		PARTITION DB 64 DUP(?)
		AA55 DB 2 DUP(?)				;end of MBR marker
		DRIVE_COUNTER DB 0				;used for DRIVE_MANAGER
		
		PRT_STATE DB ?					;partition state (active or inactive)
		ST_HEAD DB 0					;Start Head number of the partition
		ST_SECTOR DB 1					;Start Sector number of the partition
		ST_CYL DB 0						;Start Cylinder number of the partition
		PRT_TYPE DB ?					;partition type
		EN_HEAD DB ?					;End Head number of the partition
		EN_SECTOR DB ?					;End Sector number of the partition
		EN_CYL DB ?						;End Cylinder number of the partition
		DISTANCE DD ?					;distance from the begining of the disk
		SIZE_SEC DD ?					;partition size in sectors
		
		;BOOT RECORD DATA
		BOOT_RECORD LABEL BYTE
		JMPDATA DB 3 DUP(?)				;EB jmp opcode followed by a number
		MN_VER DB 8 DUP(?)				;Manufacturer version
		BYTE_PER_SEC DW ?				;Bytes per sector
		SEC_PER_CLUS DB ?				;sectors per cluster
		RES_SEC DW ?					;reserved sectors at the begining of the partition
		FATS DB ?						;number of FAT copies
		DIR_ENTRIES DW ?				;number of directory entries (each 32 bytes)
		SIZE_SEC_SMALL DW ?				;size in sectors for less than 32M
		MEDIA_TYPE DB ?					;Media Type
		FAT_SECTS16 DW ?				;number of sectors per fat
		SEC_PER_CYL DW ?				;number of sectors per track(cylinder)
		HEADSNO DW ?					;number of heads (sides)
		HIDDEN_SEC DD ?					;distance from the begining of the disk
		SIZE_SEC_BIG DD ?				;size in sectors for greater than 32M
		FAT_SECTS32 DD ?				;size in sectors for fat 32
		BOOT_REST DB 472 DUP(?)			;rest of boot record
		
		;CONVERT DATA
		LOGIC_SECTOR DD ?				;used as argument for LOGICAL_TO_PHYSICAL_SECONT (the sector to be converted)
		SECTOR DB ?						;physical sector
		CYLINDER DB ?					;physical cylinder
		HEAD DB ?						;physical head
		NSEC DB ?						;number of sectors to be read
		SEC_BUFFER DB 64*512 DUP(0)		;a buffer for reading a cluster of data

		;CLUSTER DATA
		RESERVED_BYTES DW ?				;not used
		CLUSTER DW ?					;current cluster
		
		;FAT DATA
		BUF_FLAG DB ?					;a flag passed to LOGICAL_TO_PHYSICAL_SECONT to specify the destination buffer
		FAT_BUFFER DW 256 DUP(?) 		;a buffer for fat data
		
		;DIRECTORY AND SUBDIRECTORY DATA
		NAMEARG DB 12 DUP(' ')			;tokenized item
		DIR_BUFFER DD 128 DUP(?)		;a buffer for directory entries
		FILE_FLAG DB ?					;a flag to tell the FILE_READER that file is not found
		ROOT_FLAG DB ?					;flag to specify the location of the NAMEARG
		DIR_FLAG DB ?					;flag to specify a file or subdirectory
		EXTST DD 0						;keeps the start of the extended partitions
		FILEERROR DB "FILE NOT FOUND.$"
		DIRERROR DB "PATH NOT SPECIFIED.$"
		DRIVEERROR DB "INVALID DRIVE SPECIFICATION.$"
		FATALERROR DB "FATAL ERROR: UNABLE TO READ FROM THE DEVICE.$"
		
		;MAIN DATA
		PROMPT DB " ARTMAN SYSTEMS INC.",10,13," LOW LEVEL FILE READER VERSION 1.0",10,13,10,13,
		"     HDR [drive:][path][filename] [/0]",10,13,10,13,10,13,"   /0        USES BINARY FILE READING.$"
		PATHLEN DB ?
		PATH DB 81 DUP(0)
		TKNPT DB 3						;token pointer used by SET_INPUT, skip drive and ':\'
		PRT_FLAG DB 0DH					;flag to specify the last token for printing error
		AS_BIN_FLAG DB 1				;flag to print binary pr ASCII
		DRIVE_ERROR DB 1				;drive not found
		REMAINDER DB 3 DUP(0)			;data for DIVISION (order is important!)
		DIVIDEND DB 4 DUP(0)
		DIVISOR DB 2 DUP(0)
DATASEG ENDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CODESEG SEGMENT 'CODE'


ASSUME CS:CODESEG, DS:DATASEG, SS:STACKSEG, ES:DATASEG

PARTITION_READER PROC NEAR
	
	PUSH BX
	MOV AH,2
	MOV AL,1
	MOV CH,ST_CYL
	MOV CL,ST_SECTOR
	MOV DH,ST_HEAD
	MOV DL,81H						;hard disk 2
	LEA BX,MBR
	INT 13H
	JC READERROR
	POP BX
	
	MOV AL, PARTITION[BX]			;setting the partition data into the defined variables
	MOV PRT_STATE, AL
	
	MOV AL,PARTITION[BX]+1
	MOV ST_HEAD, AL
	
	MOV AL,PARTITION[BX]+2
	MOV ST_SECTOR, AL
	
	MOV AL,PARTITION[BX]+3
	MOV ST_CYL, AL
	
	MOV AL, PARTITION[BX]+4
	MOV PRT_TYPE, AL
	
	MOV AL,PARTITION[BX]+5
	MOV EN_HEAD, AL
	
	MOV AL,PARTITION[BX]+6
	MOV EN_SECTOR, AL
	
	MOV AL,PARTITION[BX]+7
	MOV EN_CYL, AL
	
	MOV AX,WORD PTR PARTITION[BX]+8
	MOV WORD PTR DISTANCE, AX
	MOV AX,WORD PTR PARTITION[BX]+10
	MOV WORD PTR DISTANCE[2], AX
	
	MOV AX,WORD PTR PARTITION[BX]+12
	MOV WORD PTR SIZE_SEC, AX
	MOV AX,WORD PTR PARTITION[BX]+14
	MOV WORD PTR SIZE_SEC[2], AX	
	
	RET
PARTITION_READER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DRIVE_MANAGER PROC NEAR
	
	MOV AH,2
	MOV AL,1
	MOV CH,0
	MOV CL,1
	MOV DH,0
	MOV DL,81H
	LEA BX,MBR
	INT 13H
	JC READERROR

	XOR BX, BX							;calculate the number of primary partitions
	NEXTPRI:
	CMP PARTITION[BX+4], 6				;primary
	JE INBXD
	CMP PARTITION[BX+4], 4				;primary
	JE INBXD
	JMP NAROU
	INBXD:
	INC DRIVE_COUNTER
	NAROU:
	ADD BX, 16							;each entry is 16 bytes
	CMP BX,64							;end of partion table
	JNE NEXTPRI
	
	XOR AH,AH							;convert the letter to number
	MOV AL, PATH
	AND AL, 0BFH						;remove 40H from the drive letter
	SUB AL,3
	MOV BP, AX
	
	CMP AL, DRIVE_COUNTER				;if DRIVE_COUNTER is less than primary partition number then find it
	JAE EXTENDED
	MOV CL,4
	SHL AL, CL
	XOR BH,BH
	MOV BL,AL 
	CALL PARTITION_READER
	MOV DRIVE_ERROR, 0
	RET
	
	EXTENDED:							;DRIVE_COUNTER = (drive number  - DRIVE_COUNTER+1); number of nested extended partitions to read
	MOV AX, BP
	SUB AL, DRIVE_COUNTER
	INC AX
	
	XOR BH,BH
	MOV BL, DRIVE_COUNTER
	MOV CL,4
	SHL BX,CL	
	
	MOV DRIVE_COUNTER, AL
	
	XOR CX,CX
	GETNEXTEXT:							
	PUSH CX
	CALL PARTITION_READER
	CMP PRT_TYPE, 5						;extended attribute
	JE GSDH
	CMP PRT_TYPE, 0FH					;extended attribute
	JNE ERRORPRN
	GSDH:
	MOV BX, WORD PTR DISTANCE
	ADD WORD PTR EXTST, BX
	MOV BX, WORD PTR DISTANCE[2]
	ADC WORD PTR EXTST[2], BX
	
	POP CX
	INC CX
	CMP CL,DRIVE_COUNTER
	JE READDRV
	MOV BX,1
	JMP GETNEXTEXT
	
	READDRV:
	XOR BX,BX
    CALL PARTITION_READER
	MOV DRIVE_ERROR, 0
	RET	
	
DRIVE_MANAGER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BOOT_RECORD_READER PROC NEAR
	
	MOV AH,2
	MOV AL,1
	MOV CH,ST_CYL
	MOV CL,ST_SECTOR
	MOV DH,ST_HEAD
	MOV DL,81H
	LEA BX, BOOT_RECORD
	INT 13H
	JC READERROR
	
	RET
BOOT_RECORD_READER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
DIVISION PROC NEAR
	MOV REMAINDER, 0						;increments LOGIC_SECTOR and divides it  by SEC_PER_CYL
	MOV REMAINDER+1, 0
	MOV REMAINDER+2, 0

	MOV AL, BYTE PTR LOGIC_SECTOR[3]
	MOV DIVIDEND, AL
	MOV AL, BYTE PTR LOGIC_SECTOR[2]
	MOV DIVIDEND+1, AL
	MOV AL, BYTE PTR LOGIC_SECTOR[1]
	MOV DIVIDEND+2, AL
	MOV AL, BYTE PTR LOGIC_SECTOR
	MOV DIVIDEND+3, AL
	
	INC DIVIDEND+3
	ADC DIVIDEND+2,0
	ADC DIVIDEND+1,0
	ADC DIVIDEND,0
	
	MOV AL, BYTE PTR SEC_PER_CYL+1
	MOV DIVISOR, AL
	MOV AL, BYTE PTR SEC_PER_CYL
	MOV DIVISOR+1, AL
	
	MOV CX, 32
	STARTAGAIN:	
	CALL SHIFTBIT
	CALL COMPARE
	LOOP STARTAGAIN
	JMP FIND

	SHIFTBIT:
	MOV BX,6
	CLC	
	BITSHIFT: 
	RCL REMAINDER[BX],1
	DEC BX
	JNS BITSHIFT	
	RET
	
	SUBREM:
	MOV BX,3
	OR DIVIDEND[BX], 01H	
	SUB BX,2
	CLC	
	SUBIT: 
	MOV AL, BYTE PTR DIVISOR[BX]
	SBB REMAINDER[BX+1],AL
	DEC BX
	JNS SUBIT
	SBB REMAINDER,0
	RET
	
	COMPARE:
	CMP REMAINDER,0
	JNE SUBLBL

	XOR SI,SI
	COMPST: 
	MOV AL,REMAINDER[SI+1]
	CMP AL,BYTE PTR DIVISOR[SI]
	JB ENDIT
	JA SUBLBL
	INC SI    ;if equal
	CMP SI,2
	JNE COMPST	
	SUBLBL:
	CALL SUBREM
	ENDIT:
	RET
	
	FIND:
	RET
DIVISION ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOGICAL_TO_PHYSICAL_SECONT PROC NEAR
	;Sector = (logic_sector + 1) % sectors per cylinder and  X= (logic_sector + 1) / sectors per cylinder
	;Cylinder = X / sides
	;Side = X % sides
	CALL DIVISION					
	
	MOV AL, REMAINDER+2
	MOV SECTOR, AL
	
	MOV DH, DIVIDEND
	MOV DL, DIVIDEND+1
	MOV AH, DIVIDEND+2
	MOV AL, DIVIDEND+3
	MOV BX, HEADSNO
	DIV BX
	
	MOV HEAD,DL				;****
	
	MOV CYLINDER,AL			;****
	MOV BL,40H				;multiplying the MSB of cylinder by 40H, causes it to become the MSB of AL and this can be added
							;with the SECTOR (6-bit) to make the 10-bit cylinder and put its 2-byte msb in msb of sector
	MOV AL,AH
	MUL BL
	
	ADD SECTOR, AL			;****
	
	MOV AH,2
	MOV AL, NSEC
	MOV CH, CYLINDER
	MOV CL, SECTOR
	MOV DH, HEAD
	MOV DL, 81H
	CMP BUF_FLAG,0
	JE PUTSEC
	CMP BUF_FLAG,1
	JE PUTFAT
	LEA BX, DIR_BUFFER
	JMP DONEF	
	PUTSEC:
	LEA BX, SEC_BUFFER
	JMP DONEF
	PUTFAT:
	LEA BX, FAT_BUFFER	
	DONEF:
	INT 13H
	JC READERROR
	
	RET
LOGICAL_TO_PHYSICAL_SECONT ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CLUSTER_FINDER PROC NEAR
	;logic_sector = number of fats * sectors per fat + reserved sectors + number of sectors for directory entries 
	;+ (cluster - 2) sectors per cluster + distance from the begining + extended partitions offset
	XOR BH,BH
	MOV BL, FATS
	MOV AX, FAT_SECTS16
	MUL BX
	
	ADD AX, RES_SEC
	ADC DX,0
	
	MOV BX, DIR_ENTRIES
	MOV CL, 4
	SHR BX, CL
	
	ADD AX, BX
	ADC DX,0
	
	MOV BX, WORD PTR DISTANCE	
	MOV CX, WORD PTR DISTANCE[2]
		
	ADD BX,AX
	ADC CX,DX
	
	MOV AX, CLUSTER
	SUB AX,2
	PUSH AX
	XOR AH,AH
	MOV AL,SEC_PER_CLUS
	MOV SI, AX
	POP AX
	MUL SI
	
	ADD BX,AX
	ADC CX,DX
	
	ADD BX, WORD PTR EXTST
	ADC CX, WORD PTR EXTST[2]
	
	MOV WORD PTR LOGIC_SECTOR, BX
	MOV WORD PTR LOGIC_SECTOR[2], CX
	
	MOV AL, SEC_PER_CLUS
	MOV NSEC, AL
	
	MOV BUF_FLAG, 0
	CALL LOGICAL_TO_PHYSICAL_SECONT
	
	RET
CLUSTER_FINDER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FAT_CLUS_ENT PROC NEAR
	;logic sector = cluster*2/512 + distance + extst + res_sec
	SHL CLUSTER, 1
	
	XOR DX,DX					;finding out that the cluster is in which sector of the fat
	MOV AX, CLUSTER
	MOV BX,512
	DIV BX
	MOV BX,DX
	PUSH BX
	
	XOR CX,CX
	
	ADD AX, RES_SEC
	ADC CX,0
	
	ADD AX, WORD PTR DISTANCE
	ADC CX, WORD PTR DISTANCE[2]
	
	ADD AX, WORD PTR EXTST
	ADC CX, WORD PTR EXTST[2]
	
	MOV WORD PTR LOGIC_SECTOR, AX
	MOV WORD PTR LOGIC_SECTOR[2], CX
	MOV NSEC,1					;only reads one sector
	MOV BUF_FLAG, 1
	CALL LOGICAL_TO_PHYSICAL_SECONT
	
	POP BX
	MOV AX, FAT_BUFFER[BX]		;BX=cluster*2 % 512
	MOV CLUSTER, AX
	
	RET
FAT_CLUS_ENT ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DIR_ENT PROC NEAR
	;logic_sector = number of fats * sectors per fat + reserved sectors + distance from the begining + extended partitions offset	
	MOV BX, WORD PTR DISTANCE
	MOV CX, WORD PTR DISTANCE[2]
	
	ADD BX, RES_SEC
	ADC CX,0
	
	PUSH CX
	XOR CH,CH
	MOV AX, FAT_SECTS16
	MOV CL, FATS
	MUL CX	
	POP CX
	
	ADD BX, AX
	ADC CX, DX

	ADD BX, WORD PTR EXTST
	ADC CX, WORD PTR EXTST[2]
	
	MOV WORD PTR LOGIC_SECTOR, BX
	MOV WORD PTR LOGIC_SECTOR[2], CX	
	
	MOV AX, DIR_ENTRIES
	MOV CL, 4
	SHR AX, CL
	
	XOR DX,DX
	MOV BUF_FLAG, 2
	MOV NSEC,1
	GETNEXTSEC:	
	PUSH DX
	PUSH AX
	CALL LOGICAL_TO_PHYSICAL_SECONT
	ADD WORD PTR LOGIC_SECTOR, 1
	ADC WORD PTR LOGIC_SECTOR[2], 0
	POP AX
	POP DX
	INC DX
	CMP DX, AX
	JE NOTFOUND
	
	XOR BX,BX
	NEXTENT:
	CLD
	LEA SI, NAMEARG
	LEA DI, DIR_BUFFER[BX]
	MOV BP,DI
	MOV CX,11
	REPE CMPSB
	JE FOUND
	ADD BX,32						;each entry is 32 bytes
	CMP BX, 512
	JE GETNEXTSEC
	JMP NEXTENT
	
	NOTFOUND:
	MOV FILE_FLAG,0
	JMP ERRORPRN
	
	FOUND:
	MOV FILE_FLAG,1
	ADD BP,26						;getting the first cluster
	MOV BX, BP
	MOV AX,[BX]
	MOV CLUSTER, AX
	RET		
DIR_ENT ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHECK_ENTRY PROC NEAR
	CMP ROOT_FLAG, 1
	JE FLIPENT
	CALL FILE_READER
	RET
	FLIPENT:
	CALL DIR_ENT
	RET
CHECK_ENTRY ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CHANGE_DIRECTORY PROC NEAR	
	XOR AH,AH					;finding the maximum number of entries in the loaded cluster for the subdirectory
	MOV AL, SEC_PER_CLUS
	;MOVZX AX, SEC_PER_CLUS
	MOV CL,9
	SHL AX, CL
	
	XOR BX, BX
	
	CHKNEXT:
	CLD
	LEA SI, NAMEARG
	LEA DI, SEC_BUFFER[BX]
	MOV BP, DI
	MOV CX,11
	REPE CMPSB
	JE FOUNDSUB
	ADD BX, 32
	CMP BX, AX
	JNE CHKNEXT
	
	MOV FILE_FLAG,0
	RET
	
	FOUNDSUB:
	MOV FILE_FLAG, 1
	ADD BP,26
	MOV BX, BP
	MOV AX, [BX]
	MOV CLUSTER, AX
	RET	
CHANGE_DIRECTORY ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FILE_READER PROC NEAR
	CMP CLUSTER, 0	;EMPTY, SIZE=0
	JE GETTODOS
	
	GONEXTCL:
	CALL CLUSTER_FINDER
	CMP DIR_FLAG, 1
	JE GOTOSUBDIR 
	CALL PRINT_BUFFER
	JMP COMCK
	GOTOSUBDIR:
	CALL CHANGE_DIRECTORY
	CMP FILE_FLAG,1
	JE RETVAL
	COMCK:
	CALL FAT_CLUS_ENT
	CMP CLUSTER, 0FFFFH
	JNE GONEXTCL
	CMP DIR_FLAG, 1
	JNE RETVAL
	CMP FILE_FLAG, 0
	JNE RETVAL
	JMP ERRORPRN
	
	RETVAL:
	RET
	
	GETTODOS:
	MOV AX,4C00H
	INT 21H
	
FILE_READER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRINT_BUFFER PROC NEAR
	MOV AH,3	;SET DH,DL
	MOV BH,0
	INT 10H
	
	MOV AH,13H
	MOV AL,1	
	LEA BP,SEC_BUFFER
	XOR BH,BH
	MOV BL, SEC_PER_CLUS
	MOV CL,9
	SHL BX,CL
	MOV CX,BX	;TOTAL CX	if printing = binary
	
	CMP AS_BIN_FLAG, 1
	JNE PRINTBINARY
	
	MOV DI,BP	;if  printing=ascii, print until reaching a 0H
	CLD
	MOV AL,0
	REPNE SCASB
	SUB DI,BP
	DEC DI
	MOV CX,DI
	
	PRINTBINARY:
	MOV BH,0
	MOV BL,07
	INT 10H	

	RET
PRINT_BUFFER ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SET_INPUT PROC NEAR
	MOV AL, PATHLEN
	INC AL
	CMP AL, TKNPT
	JNE EVALUATE
	MOV DIR_FLAG, 0				;no other token
	RET
	
	EVALUATE:
	CLD
	LEA DI,NAMEARG
	MOV AL,' '
	MOV CX,12
	REP STOSB
	
	XOR BX,BX
	XOR CH,CH
	MOV CL,TKNPT
	MOV SI, CX
	GETNTC:
	MOV AL, PATH[BX+SI]
	MOV PRT_FLAG , AL
	CMP AL, '\'
	JE OKB
	CMP AL, 0DH
	JE OKB
	MOV NAMEARG[BX], AL
	INC BX
	CMP BX,12
	JNE GETNTC
	
	OKB:
	INC BX
	ADD TKNPT, BL
	
	MOV DIR_FLAG, 1
	
	LEA DI, NAMEARG				;setting the NAMEARG when a token contains dot(removes dot and copy the characters placed after dot
	MOV CX,9					;to the extension place
	MOV AL,'.'
	REPNE SCASB
	JNE GOFIN
	
	MOV BYTE PTR[DI-1], ' '
	MOV AL,[DI+2]
	MOV BL,[DI+1]
	MOV CL,[DI]		
	MOV SI, DI
	ADD SI,2
	CALL PUTSPACE	
	MOV NAMEARG+10, AL		
	MOV SI, DI
	INC SI
	CALL PUTSPACE
	MOV NAMEARG+9, BL	
	MOV SI, DI
	CALL PUTSPACE
	MOV NAMEARG+8, CL	
	MOV BYTE PTR NAMEARG+11,' '
	JMP GOFIN	
	
	PUTSPACE:
	PUSH SI
	SUB SI, OFFSET NAMEARG
	CMP SI, 7
	JA GBSD
	POP SI
	MOV BYTE PTR[SI], ' '
	RET
	GBSD:
	POP SI
	RET
	
	GOFIN:
	RET
SET_INPUT ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ERRORPRN PROC NEAR

	MOV AH,9					;prints different error messages with respect to invalid drive, path or file
	CMP DRIVE_ERROR, 1
	JE DRV
	
	CMP PRT_FLAG , 0DH
	JE FILERR
	LEA DX, DIRERROR
	JMP PRNT
	FILERR:
	LEA DX, FILEERROR
	JMP PRNT
	DRV:
	LEA DX,DRIVEERROR
	PRNT:
	INT 21H
	
	MOV AX,4C01H				;sends error to ms-dos kernel
	INT 21H
ERRORPRN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

READERROR PROC NEAR

	MOV AH, 9					;fatal error due to hard disk inaccessibility or file read error
	LEA DX, FATALERROR
	INT 21H
	
	MOV AX,4C01H
	INT 21H
READERROR ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MAIN PROC NEAR   
	MOV AX, DATASEG
	MOV DS,AX
	PUSH DS
	
	MOV CL, ES:[80H]				;copying the argument
	MOV PATHLEN, CL
	CMP CL, 1
	JBE ABFIN
	
	XOR BX, BX
	CHKSW:
	CMP BYTE PTR ES:[82H+BX], ' '	;checking fo /0 swith
	JNE INCBDS
	CMP BYTE PTR ES:[82H+BX+1], '/'
	JNE INCBDS
	CMP BYTE PTR ES:[82H+BX+2], '0'
	JE FOUNDSW
	INCBDS:
	INC BX
	CMP BL, PATHLEN
	JNE CHKSW
	JMP GETFILE
	
	FOUNDSW:
	MOV AS_BIN_FLAG, 0
	MOV BYTE PTR ES:[82H+BX], 0DH
	INC BX
	MOV PATHLEN, BL
	
	GETFILE:
	XOR BX, BX
	FILLPATH:
	MOV CL, BYTE PTR ES:[82H+BX]
	MOV PATH[BX], CL
	INC BX
	CMP BL, PATHLEN
	JNE FILLPATH
	DEC PATHLEN
	
	POP ES
	
	XOR BX,BX
	CAPPATH:						;uppercasing the path
	CMP PATH[BX], 61H
	JB ADBX
	CMP PATH[BX], 7AH
	JA ADBX
	AND PATH[BX],0DFH
	ADBX:
	INC BX
	CMP BL, PATHLEN
	JNE CAPPATH
	
	CALL DRIVE_MANAGER
	CALL BOOT_RECORD_READER
	
	CALL SET_INPUT
	MOV ROOT_FLAG,1
	CALL CHECK_ENTRY
	
	MOV ROOT_FLAG,0
	NEXTTOKEN:
	CALL SET_INPUT
	CALL CHECK_ENTRY
	CMP DIR_FLAG, 0
	JNE NEXTTOKEN
	JMP FINISH
	
	ABFIN:							;if no argument is set
	MOV AH,9
	LEA DX,PROMPT
	INT 21H
	
	FINISH:
	MOV AH,2
	MOV DL,10
	INT 21H
	MOV DL,13
	INT 21H
	
	MOV AX,4C00H
	INT 21H
MAIN ENDP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CODESEG ENDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

END MAIN

;;Copyright (c)  2004-2007 Artman(R) Systems Incorporated and its licensors. All rights reserved.