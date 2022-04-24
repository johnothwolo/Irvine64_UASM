Title Irvine32 Link Library Source Code         (Irvine32.asm)

Comment @
To view this file with proper indentation, set your 
	editor's tab stops to columns 5, 11, 35, and 40.

Recent Updates:
05/02/09: Str_trim
12/11/2011: StrLength

This library was created exlusively for use with the book,
"Assembly Language for Intel-Based Computers", 4th, 5th, and 6th Editions,
by Kip R. Irvine, 2002-2010.

Copyright 2002-2012, Prentice-Hall Publishing. No part of this file may be
reproduced, in any form or by any other means, without permission in writing
from the author or publisher.

Acknowledgements:
------------------------------
Most of the code in this library was written by Kip Irvine.
Special thanks to Gerald Cahill for his many insights, suggestions, and bug fixes.
Thanks to Richard Stam for his development of Readkey-related procedures.
Thanks to James Brink for helping to test the library.

Alphabetical Listing of Public Procedures
----------------------------------
(Unless otherwise marked, all procedures are documented in Chapter 5.)

CloseFile
Clrscr
CreateOutputFile
Crlf
Delay
DumpMem
DumpRegs
GetCommandTail
GetDateTime	Chapter 11
GetMaxXY
GetMseconds
GetTextColor
Gotoxy
IsDigit
MsgBox
MsgBoxAsk
OpenInputFile
ParseDecimal32
ParseInteger64
Random32
Randomize
RandomRange
ReadChar
ReadDec
ReadFromFile
ReadHex
ReadInt
ReadKey
ReadKeyFlush
ReadString
SetTextColor
Str_compare	Chapter 9
Str_copy		Chapter 9
Str_length	Chapter 9
Str_trim		Chapter 9
Str_ucase		Chapter 9
WaitMsg
WriteBin
WriteBinB
WriteChar
WriteDec
WriteHex
WriteHexB
WriteInt
WriteStackFrame	Chapter 8  (James Brink)
WriteStackFrameName	Chapter 8  (James Brink)
WriteString
WriteToFile
WriteWindowsMsg

	          Implementation Notes:
	          --------------------
1. The Windows Sleep function modifies the contents of RCX.
2. Remember to save and restore all 32-bit general purpose
   registers (except RAX) before calling MS-Windows API functions.
---------------------------------------------------------------------@
;OPTION CASEMAP:NONE	; optional: force case-sensitivity

INCLUDE Irvine64.inc	; function prototypes for this library
INCLUDE Macros.inc	; macro definitions


;*************************************************************
;*                          MACROS                           *
;*************************************************************

;---------------------------------------------------------------------
pushaq MACRO
;
; emulates the pushad instruction from 32bit masm
; order is  EAX, ECX, EDX, EBX, original ESP, EBP, ESI, and EDI
;----------------------------------------------------
	mov r14, rsp ; Temporary = ESP;
	push rax
	push rcx
	push rdx
	push rbx
	push r14 ; Push Temporary (original esp value)
	push rbp
	push rsi
	push rdi
ENDM

;---------------------------------------------------------------------
popaq MACRO
;
; emulates the popad instruction from 32bit masm
; order is  EDI, ESI, EBP, EBX, EDX, ECX, and EAX
;----------------------------------------------------	
	pop rdi
	pop rsi
	pop rbp
	add rsp, 8 ; //skip next 8 bytes of stack
	pop rbx
	pop rdx
	pop rcx
	pop rax
ENDM

;---------------------------------------------------------------------
ShowFlag MACRO flagName, shiftCount
	     LOCAL flagStr, flagVal, L1
;
; Helper macro.
; Display a single CPU flag value
; Directly accesses the rflags variable in Irvine16.asm/Irvine32.asm
; (This macro cannot be placed in Macros.inc)
;---------------------------------------------------------------------

.data
flagStr DB "  &flagName="
flagVal DB ?,0

.code
	push rax
	push rdx

	mov  rax, rflags	; retrieve the flags
	mov  flagVal, '1'
	shr  rax, shiftCount	; shift into carry flag
	jc   L1
	mov  flagVal, '0'
L1:
	lea  rdx, flagStr	; display flag name and value
	call WriteString

	push rdx
	push rax
ENDM

;-------------------------------------------------------------
CheckInit MACRO
;
; Helper macro
; Check to see if the console handles have been initialized
; If not, initialize them now.
;-------------------------------------------------------------
LOCAL exit
	cmp InitFlag, 0
	jne exit
	call Initialize
exit:
ENDM

;*************************************************************
;*                      SHARED DATA                          *
;*************************************************************

MAX_DIGITS = 80

.data		               ; initialized data
InitFlag 	DB 0	               ; initialization flag
zeroVar 	DWORD 0
xtable 		BYTE "0123456789ABCDEF"
evRepeat    WORD  0			; Controls key repeat counting. Used by ReadKey and ReadKeyFlush

.data?		               ; uninitialized data
consoleInHandle  QWORD ?     	; handle to console input device
consoleOutHandle QWORD ?     	; handle to standard output device
consoleErrHandle QWORD ?     	; handle to standard err device
bytesWritten     DWORD ?     	; number of bytes written
rflags  QWORD ?
digitBuffer BYTE MAX_DIGITS DUP(?),?

buffer DB 512 DUP(?)
bufferMax = ($ - buffer)
bytesRead DQ ?
sysTime SYSTEMTIME <>	     ; system time structure


;*************************************************************
;*                    PUBLIC PROCEDURES                      *
;*************************************************************

.code

;--------------------------------------------------------
CloseFile PROC
;
; Closes a file using its handle as an identifier. 
; Receives: EAX = file handle 
; Returns: EAX = nonzero if the file is successfully 
;   closed.
; Last update: 6/8/2005
;--------------------------------------------------------

	mov	rdi, rax
	mov rax, 0x2000006
	syscall
	ret
CloseFile ENDP


;-------------------------------------------------------------
Clrscr PROC
;
; Clear the screen by writing blanks to all positions
; Receives: nothing
; Returns: nothing
; Last update: 10/15/02
;
; The original version of this procedure incorrectly assumed  the
; console window dimensions were 80 X 25 (the default MS-DOS screen).
; This new version writes both blanks and attribute values to each 
; buffer position. Restriction: Only the first 512 columns of each 
; line are cleared. The name capitalization was changed to "Clrscr".
;-------------------------------------------------------------

.data
clscr_str 	db	"\033[2J\033[1;1H",0

.code
	pushaq
	CheckInit
	lea rbx, clscr_str
	invoke printf, rbx
	popaq
	ret
Clrscr ENDP


;------------------------------------------------------
CreateOutputFile PROC
;
; Creates a new file and opens it in output mode.
; Receives: RDX points to the filename.
; Returns: If the file was created successfully, RAX 
;   contains a valid file handle. Otherwise, RAX  
;   equals INVALID_HANDLE_VALUE.
;------------------------------------------------------
	INVOKE CreateFile,
	  rdx, GENERIC_WRITE, DO_NOT_SHARE, NULL,
	  CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
	ret
CreateOutputFile ENDP


;-----------------------------------------------------
Crlf PROC
;
; Writes a carriage return / linefeed
; sequence (0Dh,0Ah) to standard output.
;-----------------------------------------------------
	CheckInit
	mWrite <0dh,0ah>	; invoke a macro
	ret
Crlf ENDP


;------------------------------------------------------
Delay PROC
;
; THIS FUNCTION IS NOT IN THE IRVINE16 LIBRARY
; Delay (pause) the current process for a given number
; of milliseconds.
; Receives: RAX = number of milliseconds
; Returns: nothing
; Last update: 7/11/01
;------------------------------------------------------

	pushaq
	INVOKE Sleep, eax
	popaq
	ret

Delay ENDP


;---------------------------------------------------
DumpMem PROC
	     LOCAL unitsize:dword, byteCount:word, memAddr:qword
;
; Writes a range of memory to standard output
; in hexadecimal.
; Receives: RSI = starting offset, RCX = number of units,
;           RBX = unit size (1=byte, 2=word, 4=doubleword, or 8=quadword)
; Returns:  nothing
; Last update: 7/11/01
;---------------------------------------------------
.data
oneSpace   DB ' ',0

dumpPrompt DB 13,10,"Dump of offset ",0
dashLine   DB "-------------------------------",13,10,0

.code
	pushaq
	mov  memAddr, rsi
	lea  rdx, dumpPrompt
	call WriteString
	mov  rax, rsi	; get memory offset to dump
	call WriteHex
	call Crlf
	lea  rdx, dashLine
	call WriteString

	mov  byteCount,0
	mov  unitsize, ebx
	cmp  ebx, 8	; select output size
	je   L0
	cmp  ebx, 4
	je   L1
	cmp  ebx, 2
	je   L2
	jmp  L3

	; 64-bit quadword output
L0:
	mov  rax, [rsi]
	call WriteHex
	mWriteSpace 2
	add  rsi,rbx
	Loop L1
	jmp  L4

	; 32-bit doubleword output
L1:
	mov  rax, [rsi]
	call WriteHex
	mWriteSpace 2
	add  rsi,rbx
	Loop L2
	jmp  L4

	; 16-bit word output
L2:
	mov  ax, [rsi]	; get a word from memory
	ror  ax, 8	; display high byte
	call HexByte
	ror  ax, 8	; display low byte
	call HexByte
	mWriteSpace 1	; display 1 space
	add  esi, unitsize	; point to next word
	Loop L2
	jmp  L4

	; 8-bit byte output, 16 bytes per line
L3:
	mov  al, [rsi]
	call HexByte
	inc  byteCount
	mWriteSpace 1
	inc  rsi

	; if( byteCount mod 16 == 0 ) call Crlf

	mov  dx,0
	mov  ax, byteCount
	mov  bx,16
	div  bx
	cmp  dx,0
	jne  L3B
	call	Crlf
L3B:
	Loop L3
	jmp  L4

L4:
	call	Crlf
	popaq
	ret
DumpMem ENDP


;---------------------------------------------------
DumpRegs PROC
;
; Displays RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP in
; hexadecimal. Also displays the Zero, Sign, Carry, and
; Overflow flags.
; Receives: nothing.
; Returns: nothing.
; Last update: 1/12/2020
;
; Warning: do not create any local variables or stack
; parameters, because they will alter the RBP register.
;---------------------------------------------------
.data?
saveIP  QWORD ?
saveRSP QWORD ?
.code
	pop saveIP	; get current RIP
	mov saveRSP, rsp	; save RSP's value at entry
	push saveIP	; replace it on stack
	push rax	; save RAX (restore on exit)

	pushfq	; push extended flags

	pushfq	; push flags again, and
	pop  rflags	; save them in a variable

	call	Crlf
	mShowRegister RAX, RAX
	mShowRegister RBX, RBX
	mShowRegister RCX, RCX
	mShowRegister RDX, RDX
	call	Crlf
	mShowRegister RSI, RSI
	mShowRegister RDI, RDI

	mShowRegister RBP, RBP

	mov rax,saveRSP
	mShowRegister RSP, RAX
	call	Crlf

	mov rax,saveIP
	mShowRegister RIP, RAX
	mov rax,rflags
	mShowRegister EFL, RAX

; Show the flags (using the rflags variable). The integer parameter indicates
; how many times RFLAGS must be shifted right to shift the selected flag 
; into the Carry flag.

	ShowFlag CF,1
	ShowFlag SF,8
	ShowFlag ZF,7
	ShowFlag OF,12
	ShowFlag AF,5
	ShowFlag PF,3

	call	Crlf
	call	Crlf

	pushaq
	pop rax
	ret
DumpRegs ENDP


;-------------------------------------------------------------
GetCommandTail PROC
;
; Copies the tail of the program command line into a buffer
; (after stripping off the first argument - the program's name)
; Receives: RDX points to a 129-byte buffer that will receive
; the data.
; Returns: Carry Flag = 1 if no command tail, otherwise CF=0
;
; Calls the WIN API function GetCommandLine, and scan_for_quote,
; a private helper procedure. Each argument in the command line tail 
; is followed by a space except for the last argument which is 
; followed only by null.
;
; Implementation notes:
;
; Running in a console window:
; When the command line is blank, GetCommandLine under Windows 95/98
; returns the program name followed by a space and a null. Windows 2000/XP
; returns the program name followed by only null (the space is omitted). 
;
; Running from an IDE such as TextPad or JCreator:
; When the command line is blank, GetCommandLine returns the program 
; name followed by a space and a null for all versions of Windows.
;
; Contributed by Gerald Cahill, 9/26/2002
; Modified by Kip Irvine, 6/13/2005.
;-------------------------------------------------------------

QUOTE_MARK = 22h

	pushaq
	mov r15, rdx
	INVOKE GetCommandLine   	; returns pointer in RAX
	mov rdx, r15
; Initialize first byte of user's buffer to null, in case the 
; buffer already contains text.

	mov	BYTE PTR [rdx],0

; Copy the command-line string to the array. Read past the program's 
; EXE filename (may include the path). This code will not work correctly 
; if the path contains an embedded space.

	mov	rsi, rax
L0:	mov	al, byte ptr [rsi]    	; strip off first argument
	inc	rsi
	.IF al == QUOTE_MARK	; quotation mark found?
	call	scan_for_quote	; scan until next quote mark
	jmp	LB	; and get the rest of the line
	.ENDIF
	cmp	al,' '      	; look for blank
	je 	LB	; found it
	cmp	al,1	; look for null
	jc	L2	; found it (set CF=1)
	jmp	L0	; not found yet

; Check if the rest of the tail is empty.

LB:	cmp	BYTE PTR [rsi], 1	; first byte in tail < 1?
	jc	L2	; the tail is empty (CF=1)

; Copy all bytes from the command tail to the buffer.

L1:	mov	al,[rsi]	; get byte from cmd tail
	mov	[rdx],al	; copy to buffer
	inc	rsi
	inc	rdx
	cmp	al,0      	; null byte found?
	jne	L1          	; no, loop
	
	clc		; CF=0 means a tail was found

L2:	popaq
	ret
GetCommandTail ENDP


;------------------------------------------------------------
scan_for_quote PROC PRIVATE
;
; Helper procedure that looks for a closing quotation mark. This 
; procedure lets us handle path names with embedded spaces.
; Called by: GetCommandTail
;
; Receives: RSI points to the current position in the command tail.
; Returns: RSI points one position beyond the quotation mark.
;------------------------------------------------------------

L0:	mov	al,[rsi]    	; get a byte
	inc	rsi	; point beyond it
	cmp	al,QUOTE_MARK	; quotation mark found?
	jne	L0	; not found yet

	ret 
scan_for_quote ENDP


;--------------------------------------------------
GetDateTime PROC,
	pDateTime:PTR QWORD
	LOCAL flTime:FILETIME
;
; Gets the current local date and time, storing it as a
; 64-bit integer (Win32 FILETIME format) in memory at 
; the address specified by the input parameter.
; Receives: pointer to a QWORD variable (inout parameter)
; Returns: nothing
; Updated 10/20/2002
;--------------------------------------------------
	pushaq

; Get the system local time.
	INVOKE GetLocalTime,
	  ADDR sysTime

; Convert the SYSTEMTIME to FILETIME.
	INVOKE SystemTimeToFileTime,
	  ADDR sysTime,
	  ADDR flTime

; Copy the FILETIME to a Quadword.
	mov rsi, pDateTime
	mov eax, flTime.loDateTime
	mov DWORD PTR [rsi],eax
	mov eax, flTime.hiDateTime
	mov DWORD PTR [rsi+4],eax

	popaq
	ret
GetDateTime ENDP


;----------------------------------------------------------------
GetMaxXY PROC
	LOCAL bufInfo:CONSOLE_SCREEN_BUFFER_INFO
;
; Returns the current columns (X) and rows (Y) of the console
; window buffer. These values can change while a program is running
; if the user modifies the properties of the application window.
; Receives: nothing
; Returns: AX = rows (Y); DX = columns (X)
;
;----------------------------------------------------------------
	CheckInit

	; Get the console buffer size and attributes
	pushaq
	INVOKE GetConsoleScreenBufferInfo, consoleOutHandle, ADDR bufInfo
	popaq

	mov dx,bufInfo.dwSize.X
	mov ax,bufInfo.dwSize.Y

	ret
GetMaxXY ENDP


;----------------------------------------------------------------
GetMseconds PROC USES rbx rdx
	LOCAL hours:DWORD, min:DWORD, sec:DWORD
;
Comment !
Returns the number of milliseconds that have elapsed past midnight.
Receives: nothing; Returns: milliseconds
Implementation Notes:
Calculation: ((hours * 3600) + (minutes * 60) + seconds)) * 1000 + milliseconds
Under Win NT/ 2000/ XT, the resolution is 10ms.  Under Win 98/ ME/ or any
DOS-based version, the resolution is 55ms (average).

Last update: 1/30/03
-----------------------------------------------------------------!
	pushaq
	INVOKE GetLocalTime,ADDR sysTime
	; convert hours to seconds
	popaq
	movzx rax,sysTime.wHour
	mov   rbx,3600
	mul   rbx
	mov   hours,eax

	; convert minutes to seconds
	movzx rax,sysTime.wMinute
	mov   rbx,60
	mul   rbx
	mov   min, eax

	; add seconds to total seconds
	movzx rax,sysTime.wSecond
	mov   sec,eax

	; multiply seconds by 1000
	mov   eax,hours
	add   eax,min
	add   eax,sec
	mov   rbx,1000
	mul   rbx

	; add milliseconds to total
	movzx rbx,sysTime.wMilliseconds
	add   rax,rbx

	ret
GetMseconds ENDP


;--------------------------------------------------
GetTextColor PROC
	LOCAL bufInfo:CONSOLE_SCREEN_BUFFER_INFO
;
;
; Get the console window's color attributes. 
; Receives: nothing
; Returns: AH = background color, AL = foreground 
;   color 
;--------------------------------------------------

	pushaq
	CheckInit

	; Get the console buffer size and attributes
	INVOKE GetConsoleScreenBufferInfo, consoleOutHandle, ADDR bufInfo
	popaq
	
	mov  ax,bufInfo.wAttributes
	ret
GetTextColor ENDP


;--------------------------------------------------
Gotoxy PROC
;
; Locate the cursor
; Receives: DH = screen row, DL = screen column
; Last update: 7/11/01
;--------------------------------------------------------
.data
_cursorPosition COORD <>
.code
	pushaq

	CheckInit	; was console initialized?
	movzx ax,dl
	mov _cursorPosition.X, ax
	movzx ax,dh
	mov _cursorPosition.Y, ax
	INVOKE SetConsoleCursorPosition, consoleOutHandle, _cursorPosition

	popaq
	ret
Gotoxy ENDP


;----------------------------------------------------
Initialize PROC private
;
; Get the standard console handles for input and output,
; and set a flag indicating that it has been done.
; Updated 03/17/2003
;----------------------------------------------------
	pushaq

	INVOKE GetStdHandle, STD_INPUT_HANDLE
	mov [consoleInHandle], rax

	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov [consoleOutHandle], rax

	mov InitFlag,1

	popaq
	ret
Initialize ENDP


;-----------------------------------------------
IsDigit PROC
;
; Determines whether the character in AL is a
; valid decimal digit.
; Receives: AL = character
; Returns: ZF=1 if AL contains a valid decimal
;   digit; otherwise, ZF=0.
;-----------------------------------------------
	 cmp   al,'0'
	 jb    ID1
	 cmp   al,'9'
	 ja    ID1
	 test  ax,0     		; set ZF = 1
ID1: ret
IsDigit ENDP


;-----------------------------------------------
MsgBox PROC
;
; Displays a popup message box.
; Receives: RDX = offset of message, RBX = 
; 	offset of caption (or 0 if no caption)
; Returns: nothing
;-----------------------------------------------
.data
zx02abc_def_caption BYTE " ",0
.code
	pushaq
	
	.IF rbx == 0
	  lea rbx, zx02abc_def_caption
	.ENDIF
	INVOKE MessageBox, 0, rdx, rbx, 0 

	popaq
	ret
MsgBox ENDP


;--------------------------------------------------
MsgBoxAsk PROC uses rbx rcx rdx rsi rdi
;
; Displays a message box with a question icon and 
;    Yes/No buttons.
; Receives: RDX = offset of message. For a blank
;   caption, set RBX to NULL; otherwise, RBX = offset 
;   of the caption string.
; Returns: RAX equals IDYES (6) or IDNO (7).
;--------------------------------------------------
.data
zq02abc_def_caption BYTE " ",0
.code
	.IF rbx == NULL
	  lea rbx, zq02abc_def_caption
	.ENDIF
	INVOKE MessageBox, NULL, rdx, rbx, 
		MB_YESNO + MB_ICONQUESTION
		
	ret
MsgBoxAsk ENDP


;------------------------------------------------------
OpenInputFile PROC
;
; Opens an existing file for input.
; Receives: RDX points to the filename.
; Returns: If the file was opened successfully, RAX 
; contains a valid file handle. Otherwise, RAX equals 
; INVALID_HANDLE_VALUE.
; Last update: 6/8/2005
;------------------------------------------------------

	INVOKE CreateFile,
	  rdx, GENERIC_READ, DO_NOT_SHARE, NULL,
	  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
	ret
OpenInputFile ENDP


;--------------------------------------------------------
ParseDecimal32 PROC USES rbx rcx rdx rsi
  LOCAL saveDigit:QWORD
;
; Converts (parses) a string containing an unsigned decimal
; integer, and converts it to binary. All valid digits occurring 
; before a non-numeric character are converted. 
; Leading spaces are ignored.

; Receives: RDX = offset of string, RCX = length 
; Returns:
;  If the integer is blank, RAX=0 and CF=1
;  If the integer contains only spaces, RAX=0 and CF=1
;  If the integer is larger than 2^32-1, RAX=0 and CF=1
;  Otherwise, RAX=converted integer, and CF=0
;
; Created 7/15/05 (from the old ReadDec procedure)
;--------------------------------------------------------

	mov   rsi,rdx           		; save offset in RSI

	cmp   rcx,0            		; length greater than zero?
	jne   L1              		; yes: continue
	mov   rax,0            		; no: set return value
	jmp   L5              		; and exit with CF=1

; Skip over leading spaces, tabs

L1:	mov   al,[rsi]         		; get a character from buffer
	cmp   al,' '        		; space character found?
	je	L1A		; yes: skip it
	cmp	al,TAB		; TAB found?
	je	L1A		; yes: skip it
	jmp   L2              		; no: goto next step
	
L1A:
	inc   rsi              		; yes: point to next char
	loop  L1		; continue searching until end of string
	jmp   L5		; exit with CF=1 if all spaces

; Replaced code (7/19/05)---------------------------------------------
;L1:mov   al,[rsi]         		; get a character from buffer
;	cmp   al,' '          		; space character found?
;	jne   L2              		; no: goto next step
;	inc   rsi              		; yes: point to next char
;	loop  L1		; all spaces?
;	jmp   L5		; yes: exit with CF=1
;---------------------------------------------------------------------

; Start to convert the number.

L2:	mov  rax,0           		; clear accumulator
	mov  rbx,10          		; RBX is the divisor

; Repeat loop for each digit.

L3:	mov  dl,[rsi]		; get character from buffer
	cmp  dl,'0'		; character < '0'?
	jb   L4
	cmp  dl,'9'		; character > '9'?
	ja   L4
	and  rdx,0Fh		; no: convert to binary

	mov  saveDigit,rdx
	mul  rbx		; RDX:RAX = RAX * RBX
	jc   L5		; quit if Carry (RDX > 0)
	mov  rdx,saveDigit
	add  rax,rdx         		; add new digit to sum
	jc   L5		; quit if Carry generated
	inc  rsi              		; point to next digit
	jmp  L3		; get next digit

L4:	clc			; succesful completion (CF=0)
	jmp  L6

L5:	mov  rax,0		; clear result to zero
	stc			; signal an error (CF=1)

L6:	ret
ParseDecimal32 ENDP


;--------------------------------------------------------
ParseInteger64 PROC USES rbx rcx rdx rsi
  LOCAL Lsign:SDWORD, saveDigit:DWORD
;
; Converts a string containing a signed decimal integer to
; binary. 
;
; All valid digits occurring before a non-numeric character
; are converted. Leading spaces are ignored, and an optional 
; leading + or - sign is permitted. If the string is blank, 
; a value of zero is returned.
;
; Receives: RDX = string offset, RCX = string length
; Returns:  If OF=0, the integer is valid, and RAX = binary value.
;   If OF=1, the integer is invalid and RAX = 0.
;
; Created 7/15/05, using Gerald Cahill's 10/10/03 corrections.
; Updated 7/19/05, to skip over tabs
; Comments updated 11/20/09
;--------------------------------------------------------
.data
overflow_msgL BYTE  " <32-bit integer overflow>",0
invalid_msgL  BYTE  " <invalid integer>",0
.code

	mov   Lsign,1                   ; assume number is positive
	mov   rsi,rdx                   ; save offset in SI

	cmp   rcx,0                     ; length greater than zero?
	jne   L1                        ; yes: continue
	mov   rax,0                     ; no: set return value
	jmp   L10                       ; and exit

; Skip over leading spaces and tabs.

L1:	mov   al,[rsi]         		; get a character from buffer
	cmp   al,' '        		; space character found?
	je	L1A		; yes: skip it
	cmp	al,TAB		; TAB found?
	je	L1A		; yes: skip it
	jmp   L2              		; no: goto next step
	
L1A:
	inc   rsi              		; yes: point to next char
	loop  L1		; continue searching until end of string
	mov	rax,0		; all spaces?
	jmp   L10		; return 0 as a valid value

;-- Replaced code (7/19/05)---------------------------------------
;L1:	mov   al,[rsi]                  ; get a character from buffer
;	cmp   al,' '                    ; space character found?
;	jne   L2                        ; no: check for a sign
;	inc   rsi                       ; yes: point to next char
;	loop  L1
;	mov   rax,0	  ; all spaces?
;	jmp   L10	  ; return zero as valid value
;------------------------------------------------------------------

; Check for a leading sign.

L2:	cmp   al,'-'                    ; minus sign found?
	jne   L3                        ; no: look for plus sign

	mov   Lsign,-1                  ; yes: sign is negative
	dec   rcx                       ; subtract from counter
	inc   rsi                       ; point to next char
	jmp   L3A

L3:	cmp   al,'+'                    ; plus sign found?
	jne   L3A               			; no: skip
	inc   rsi                       ; yes: move past the sign
	dec   rcx                       ; subtract from digit counter

; Test the first digit, and exit if nonnumeric.

L3A: mov  al,[rsi]          	; get first character
	call IsDigit            	; is it a digit?
	jnz  L7A                	; no: show error message

; Start to convert the number.

L4:	mov   rax,0                  	; clear accumulator
	mov   rbx,10                  ; RBX is the divisor

; Repeat loop for each digit.

L5:	mov  dl,[rsi]           	; get character from buffer
	cmp  dl,'0'             	; character < '0'?
	jb   L9
	cmp  dl,'9'             	; character > '9'?
	ja   L9
	and  rdx,0Fh            	; no: convert to binary

	mov  saveDigit,edx
	imul rbx               	; RDX:RAX = RAX * RBX
	mov  edx,saveDigit

	jo   L6                	; quit if overflow
	add  rax,rdx            	; add new digit to AX
	jo   L6                 	; quit if overflow
	inc  rsi                	; point to next digit
	jmp  L5                 	; get next digit

; Overflow has occured, unlesss RAX = 80000000h
; and the sign is negative:

L6:	cmp  rax,80000000h
	jne  L7
	cmp  Lsign,-1
    jne  L7                 	; overflow occurred
    jmp  L9                 	; the integer is valid

; Choose "integer overflow" messsage.

L7: lea  rdx, overflow_msgL
    jmp  L8

; Choose "invalid integer" message.

L7A:
    lea  rdx, invalid_msgL

; Display the error message pointed to by RDX, and set the Overflow flag.

L8:	call WriteString
    call Crlf
    mov al,127
    add al,1                	; set Overflow flag
    mov  rax,0              	; set return value to zero
    jmp  L10                	; and exit

; IMUL leaves the Sign flag in an undeterminate state, so the OR instruction
; determines the sign of the iteger in RAX.
L9:	imul Lsign                  	; RAX = RAX * sign
    or rax,rax              	; determine the number's Sign

L10:ret
ParseInteger64 ENDP


;--------------------------------------------------------------
Random32  PROC
;
; Generates an unsigned pseudo-random 32-bit integer
;   in the range 0 - FFFFFFFFh.
; Receives: nothing
; Returns: RAX = random integer
; Last update: 7/11/01
;--------------------------------------------------------------
.data
seed  QWORD 1
.code
	  push  rdx
	  mov   rax, 343FDh
	  imul  seed
	  add   rax, 269EC3h
	  mov   seed, rax    ; save the seed for the next call
	  ror   rax,8        ; rotate out the lowest digit (10/22/00)
	  pop   rdx

	  ret
Random32  ENDP


;--------------------------------------------------------------
RandomRange PROC
;
; Returns an unsigned pseudo-random 32-bit integer
; in RAX, between 0 and n-1. Input parameter:
; RAX = n.
; Last update: 09/06/2002
;--------------------------------------------------------------
	 push  rbx
	 push  rdx

	 mov   rbx,rax  ; maximum value
	 call  Random32 ; rax = random number
	 mov   rdx,0
	 div   rbx      ; divide by max value
	 mov   rax,rdx  ; return the remainder

	 pop   rdx
	 pop   rbx

	 ret
RandomRange ENDP


;--------------------------------------------------------
Randomize PROC
;
; Re-seeds the random number generator with the current time
; in seconds. Calls GetSystemTime, which is accurate to 10ms.
; Receives: nothing
; Returns: nothing
; Last update: 09/06/2002
;--------------------------------------------------------
	  pushaq

	  INVOKE GetSystemTime,ADDR sysTime
	  movzx rax,sysTime.wMilliseconds
	  mov   seed,rax

	  popaq
	  ret
Randomize ENDP


;------------------------------------------------------------
ReadChar PROC USES rbx rdx
;
; Reads one character from the keyboard. The character is
; not echoed on the screen. Waits for the character if none is
; currently in the input buffer.
; Returns:  AL = ASCII code, AH = scan code
; Last update: 7/6/05
;----------------------------------------------------------

L1:	mov  rax,10	; give Windows 10ms to process messages
	call Delay
	; call ReadKey	; look for key in buffer
	jz   L1	; no key in buffer if ZF=1

	ret
ReadChar ENDP


;--------------------------------------------------------
ReadDec PROC USES rcx rdx
;
; Reads a 32-bit unsigned decimal integer from the keyboard,
; stopping when the Enter key is pressed.All valid digits occurring 
; before a non-numeric character are converted to the integer value. 
; Leading spaces are ignored.

; Receives: nothing
; Returns:
;  If the integer is blank, RAX=0 and CF=1
;  If the integer contains only spaces, RAX=0 and CF=1
;  If the integer is larger than 2^32-1, RAX=0 and CF=1
;  Otherwise, RAX=converted integer, and CF=0
;
; Last update: 7/15/05
;--------------------------------------------------------

	lea   rdx, digitBuffer
	mov	  rcx, MAX_DIGITS
	call  ReadString
	mov	  rcx,rax	; save length

	call  ParseDecimal32	; returns RAX

	ret
ReadDec ENDP


;--------------------------------------------------------
ReadFromFile PROC
;
; Reads an input file into a buffer. 
; Receives: RAX = file handle, RDX = buffer offset,
;    RCX = number of bytes to read
; Returns: If CF = 0, RAX = number of bytes read; if
;    CF = 1, RAX contains the system error code returned
;    by the GetLastError Win32 API function.
; Last update: 7/6/2005
;--------------------------------------------------------
	mov r14, 0x0
	INVOKE ReadFile,
	    rax,	; file handle
	    rdx,	; buffer pointer
	    ecx,	; max bytes to read
	    ADDR bytesRead,	; number of bytes read
	    r14		; overlapped execution flag
	cmp	rax,0	; failed?
	jne	L1	; no: return bytesRead
	INVOKE GetLastError	; yes: RAX = error code
	stc		; set Carry flag
	jmp	L2
	    
L1:	mov	rax, bytesRead	; success
	clc		; clear Carry flag
	
L2:	ret
ReadFromFile ENDP


;--------------------------------------------------------
ReadHex PROC USES rbx rcx rdx rsi
;
; Reads a 32-bit hexadecimal integer from the keyboard,
; stopping when the Enter key is pressed.
; Receives: nothing
; Returns: RAX = binary integer value
; Returns:
;  If the integer is blank, RAX=0 and CF=1
;  If the integer contains only spaces, RAX=0 and CF=1
;  Otherwise, RAX=converted integer, and CF=0

; Remarks: No error checking performed for bad digits
; or excess digits.
; Last update: 7/19/05 (skip leading spaces and tabs)
;--------------------------------------------------------
.data
xbtable     BYTE 0,1,2,3,4,5,6,7,8,9,7 DUP(0FFh),10,11,12,13,14,15
.data?
numVal      QWORD ?
charVal     BYTE ?

.code
	lea   rdx, digitBuffer
	mov   rsi,rdx		; save in RSI also
	mov   rcx,MAX_DIGITS
	call  ReadString		; input the string
	mov   rcx,rax           		; save length in RCX
	cmp   rcx,0            		; greater than zero?
	jne   B1              		; yes: continue
	jmp   B8              		; no: exit with CF=1

; Skip over leading spaces and tabs.

B1:	mov   al,[rsi]         		; get a character from buffer
	cmp   al,' '        		; space character found?
	je	B1A		; yes: skip it
	cmp	al,TAB		; TAB found?
	je	B1A		; yes: skip it
	jmp   B4              		; no: goto next step
	
B1A:
	inc   rsi              		; yes: point to next char
	loop  B1		; all spaces?
	jmp   B8		; yes: exit with CF=1

;--- Replaced code (7/19/05)-------------------------------------
;B1:	mov   al,[rsi]         		; get a character from buffer
;	cmp   al,' '          		; space character found?
;	jne   B4              		; no: goto next step
;	inc   rsi              		; yes: point to next char
;	loop  B1		; all spaces?
;	jmp   B8		; yes: exit with CF=1
;------------------------------------------------------------------

	; Start to convert the number.

B4: mov  numVal,0		; clear accumulator
	lea  rbx, xbtable		; translate table

	; Repeat loop for each digit.

B5: mov  al,[rsi]	; get character from buffer
	cmp  al,'F'	; lowercase letter?
	jbe  B6	; no
	and  al,11011111b	; yes: convert to uppercase

B6:	sub  al,30h	; adjust for table
	xlat  	; translate to binary
	mov  charVal,al
	mov  rax,16	; numVal *= 16
	mul  numVal
	mov  numVal,rax
	movzx rax,charVal	; numVal += charVal
	add  numVal,rax
	inc  rsi	; point to next digit
	loop B5	; repeat, decrement counter

B7:	mov  rax,numVal	; return valid value
	clc	; CF=0
	jmp  B9

B8:	mov  rax,0	; error: return 0
	stc	; CF=1

B9:	ret
ReadHex ENDP


;--------------------------------------------------------
ReadInt PROC USES rcx rdx
;
; Reads a 32-bit signed decimal integer from standard
; input, stopping when the Enter key is pressed.
; All valid digits occurring before a non-numeric character
; are converted to the integer value. Leading spaces are
; ignored, and an optional leading + or - sign is permitted.
; All spaces return a valid integer, value zero.

; Receives: nothing
; Returns:  If OF=0, the integer is valid, and RAX = binary value.
;   If OF=1, the integer is invalid and RAX = 0.
;
; Updated: 7/15/05, comments updated 11/20/09
;--------------------------------------------------------

; Input a signed decimal string.

	lea   rdx, digitBuffer
	mov   rcx,MAX_DIGITS
	call  ReadString
	mov   rcx,rax	; save length in RCX

; Convert to binary (RDX -> string, RCX = length)
	
	call	ParseInteger64	; returns RAX, CF

	ret
ReadInt ENDP

;------------------------------------------------------------------------------
ReadKeyFlush PROC
; Flushes the console input buffer and clears our internal repeat counter.
; Can be used to get faster keyboard reponse in arcade-style games, where
; we don't want to processes accumulated keyboard data that would slow down
; the program's response time.
; Receives: nothing
; Returns: nothing
; By Richard Stam, used by permission.
; Modified 4/5/03 by Irvine
;------------------------------------------------------------------------------
	INVOKE FlushConsoleInputBuffer, consoleInHandle 	; Flush the buffer
	lea	   r9, evRepeat
	mov    word ptr [r9], 0							; Reset our repeat counter

	ret
ReadKeyFlush ENDP

;------------------------------------------------------------------------------
ReadKey PROC USES rcx r9
	LOCAL evEvents:DWORD, saveFlags:DWORD
;
; Performs a no-wait keyboard check and single character read if available.
; If Ascii is zero, special keys can be processed by checking scans and VKeys
; Receives: nothing
; Returns:  ZF is set if no keys are available, clear if we have read the key
;	al  = key Ascii code (is set to zero for special extended codes)
;	ah  = Keyboard scan code (as in inside cover of book)
;	dx  = Virtual key code
;	rbx = Keyboard flags (Alt,Ctrl,Caps,etc.)
; Upper halves of RAX and RDX are overwritten
;
; ** Note: calling ReadKey prevents Ctrl-C from being used to terminate a program.
;
; Written by Richard Stam, used by permission.
; Revision history:
;    Irvine, 6/21/05 -changed evEvents from WORD to DWORD
;------------------------------------------------------------------------------
.data
evBuffer INPUT_RECORD <>	; Buffers our key "INPUT_RECORD"

.code
	CheckInit	; call Inititialize, if not already called

	; Save console flags
	INVOKE GetConsoleMode, consoleInHandle, ADDR saveFlags

	; Clear console flags, making it possible to detect Ctrl-C and Ctrl-S.
	INVOKE SetConsoleMode, consoleInHandle, 0

	cmp evRepeat,0	; key already processed by previous call to this function?
	ja  HaveKey	; if so, process the key

Peek:
	; Peek to see if we have a pending event. If so, read it.
	INVOKE PeekConsoleInput, consoleInHandle, ADDR evBuffer, 1, ADDR evEvents
	test evEvents, 0FFFFh
	jz   NoKey						; No pending events, so done.

	INVOKE ReadConsoleInput, consoleInHandle, ADDR evBuffer, 1, ADDR evEvents
	
	test evEvents, 0FFFFh
	jz   NoKey						; No pending events, so done.

	cmp  evBuffer.eventType, KEY_EVENT					; Is it a key event?
	jne  Peek						                    ; No -> Peek for next event
	TEST evBuffer.Event.bKeyDown, KBDOWN_FLAG		     ; is it a key down event?
	jz   Peek						                    ; No -> Peek for next event

	mov  ax, evBuffer.Event.wRepeatCount				; Set our internal repeat counter
	mov  evRepeat, ax

HaveKey:
	mov  al,  evBuffer.Event.uChar.AsciiChar				; copy Ascii char to al
	mov  ah,  BYTE PTR [evBuffer.Event.wVirtualScanCode]	     ; copy Scan code to ah
	mov  dx,  evBuffer.Event.wVirtualKeyCode				; copy Virtual key code to dx
	mov  ebx, evBuffer.Event.dwControlKeyState			; copy keyboard flags to rbx

	; Ignore the key press events for Shift, Ctrl, Alt, etc.
	; Don't process them unless used in combination with another key
	.IF dx == VK_SHIFT || dx == VK_CONTROL || dx == VK_MENU || \
	  dx == VK_CAPITAL || dx == VK_NUMLOCK || dx == VK_SCROLL
	  jmp Peek					                    ; Don't process -> Peek for next event
	.ENDIF

	call  ReadKeyTranslate					          ; Translate scan code compatability

	dec  evRepeat					                    ; Decrement our repeat counter
	or   dx,dx					                    ; Have key: clear the Zero flag
	jmp  Done

NoKey:
	mov  evRepeat, 0					               ; Reset our repeat counter
	test rax,0					                    ; No key: set ZF=1 and quit

Done:
    pushfq 					                         ; save Zero flag
    pushaq
           					                         ; Restore Console mode
    INVOKE SetConsoleMode, consoleInHandle, saveFlags

    ; Unless we call ReadKeyFlush in Windows 98, the key we just read
    ; reappears the next time ReadString is called. We don't know why this happens.
    ; call ReadKeyFlush

    popaq
    popfq  					                         ; restore Zero flag
    ret
ReadKey ENDP

;------------------------------------------------------------------------------
ReadKeyTranslate PROC PRIVATE USES rbx rcx rdx rsi 
; Translates special scan codes to be compatible with DOS/BIOS return values.
; Called directly by ReadKey.
; Receives:
;	al  = key Ascii code
;	ah  = Virtual scan code
;	dx  = Virtual key code
;	rbx = Keyboard flags (Alt,Ctrl,Caps,etc.)
; Returns:
;	ah  = Updated scan code (for Alt/Ctrl/Shift & special cases)
;	al  = Updated key Ascii code (set to 0 for special keys)
; Written by Richard Stam, used by permission.
; Modified 4/5/03 by Irvine
;------------------------------------------------------------------------------

.data  ; Special key scan code translation table
; order: VirtualKey,NormalScan,CtrlScan,AltScan
SpecialCases \
	BYTE VK_LEFT,  4Bh, 73h,  4Bh
CaseSize = ($ - SpecialCases)			; Special case table element size
	BYTE VK_RIGHT, 4Dh, 74h,  4Dh
	BYTE VK_UP,    48h, 8Dh,  48h
	BYTE VK_DOWN,  50h, 91h,  50h
	BYTE VK_PRIOR, 49h, 84h,  49h 		; PgUp
	BYTE VK_NEXT,  51h, 76h,  51h 		; PgDn
	BYTE VK_HOME,  47h, 77h,  47h
	BYTE VK_END,   4Fh, 75h,  4Fh
	BYTE VK_INSERT,52h, 92h,  52h
	BYTE VK_DELETE,53h, 93h,  53h
	BYTE VK_ADD,   4Eh, 90h,  4Eh
	BYTE VK_SUBTRACT,4Ah,8Eh, 4Ah
	BYTE VK_F11,   85h, 85h,  85h
	BYTE VK_F12,   86h, 86h,  86h
	BYTE VK_11,    0Ch, 0Ch,  82h 		; see above
	BYTE VK_12,    0Dh, 0Dh,  83h 		; see above
	BYTE 0			; End of Table

.code
; Registers AH-DH may not be used with SPL-DIL or R8-R15
; the table index will be r9 and rsi will be use to store SpecialCases address due to 64bit encoding limitations.
	pushfq					                 ; Push flags to save ZF of ReadKey
	mov  r9, 0
	lea  rsi, SpecialCases
	; Search through the special cases table
Search:
	cmp  byte ptr [rsi+r9], 0					  ; Check for end of search table
	je   NotFound

	cmp  dl, [rsi+r9]				  ; Check if special case is found
	je   Found

	add  r9, CaseSize					       ; Increment our table index
	jmp  Search					            ; Continue searching

Found:
	.IF rbx & CTRL_MASK
	  mov  r10, rax
	  add  rsi, r9
	  mov  ah, 	[rsi+2]			  ; Specify the Ctrl scan code
	  mov  al,0					            ; Updated char for special keys
	.ELSEIF rbx & ALT_MASK
	  add  rsi, r9
	  mov  ah,[rsi+3]				  ; Specify the Alt scan code
	  mov  al,0					            ; Updated char for special keys
	.ELSE
	  add  rsi, r9
	  mov ah, [rsi+1]				  ; Specify the normal scan code
	.ENDIF
	jmp  Done

NotFound:
	.IF ! (rbx & KEY_MASKS)				       ; Done if not shift/ctrl/alt combo
	  jmp  Done
	.ENDIF

	.IF dx >= VK_F1 && dx <= VK_F10			  ; Check for F1 to F10 keys
	  .IF rbx & CTRL_MASK
	    add ah,23h					            ; 23h = Hex diff for Ctrl/Fn keys
	  .ELSEIF rbx & ALT_MASK
	    add ah,2Dh					            ; 2Dh = Hex diff for Alt/Fn keys
	  .ELSEIF rbx & SHIFT_MASK
	    add ah,19h					            ; 19h = Hex diff for Shift/Fn keys
	  .ENDIF
	.ELSEIF al >= '0' && al <= '9'			  ; Check for Alt/1 to Alt/9
	  .IF rbx & ALT_MASK
	    add ah,76h					            ; 76h = Hex diff for Alt/n keys
	    mov al,0
	  .ENDIF
	.ELSEIF dx == VK_TAB					  ; Check for Shift/Tab (backtab)
	  .IF rbx & SHIFT_MASK
	    mov al,0					            ; ah already has 0Fh, al=0 for special
	  .ENDIF
	.ENDIF

Done:
	pushaq					; Pop flags to restore ZF of ReadKey
	ret
ReadKeyTranslate ENDP


;--------------------------------------------------------
ReadString PROC
	LOCAL bufSize:DWORD, saveFlags:DWORD, junk:DWORD
;
; Reads a string from the keyboard and places the characters
; in a buffer.
; Receives: RDX offset of the input buffer
;           RCX = maximum characters to input (including terminal null)
; Returns:  RAX = size of the input string.
; Comments: Stops when Enter key (0Dh,0Ah) is pressed. If the user
; types more characters than (RCX-1), the excess characters
; are ignored.
; Written by Kip Irvine and Gerald Cahill
; Last update: 11/19/92, 03/20/2003
;--------------------------------------------------------
.data?
_$$temp DWORD ?		       ; added 03/20/03
.code
	pushaq
	CheckInit

	mov rdi,rdx		       ; set RDI to buffer offset
	mov bufSize, ecx		  ; save buffer size

	push rdx
	INVOKE ReadConsole,
	  consoleInHandle,		  ; console input handle
	  rdx,		            ; buffer offset
	  ecx,		            ; max count
	  ADDR bytesRead,
	  zeroVar
	pop rdx

	cmp bytesRead, 0
	jz  L5 		                ; skip move if zero chars input

	dec bytesRead		           ; make first adjustment to bytesRead
	cld		; search forward
	mov ecx,bufSize		      ; repetition count for SCASB
	mov al,0Ah		           ; scan for 0Ah (Line Feed) terminal character
	repne scasb
	jne L1		                ; if not found, jump to L1

	;if we reach this line, length of input string <= (bufsize - 2)

	dec bytesRead		           ; second adjustment to bytesRead
	sub rdi,2		                ; 0Ah found: back up two positions
	cmp rdi,rdx 		           ; don't back up to before the user's buffer
	jae L2
	mov rdi,rdx 		           ; 0Ah must be the only byte in the buffer
	jmp L2		                ; and jump to L2

L1:	mov rdi,rdx		           ; point to last byte in buffer
	add edi,bufSize
	dec rdi
	mov BYTE PTR [rdi],0    	      ; insert null byte

	; Save the current console mode
	INVOKE GetConsoleMode,consoleInHandle, ADDR saveFlags
	; Switch to single character mode
	INVOKE SetConsoleMode,consoleInHandle, 0

	; Clear excess characters from the buffer, 1 byte at a time
L6:	INVOKE ReadConsole,consoleInHandle, ADDR junk,1,ADDR _$$temp,0
	mov al, BYTE PTR junk
	cmp al,0Ah 		; the terminal line feed character
	jne L6     		; keep looking, it must be there somewhere

	INVOKE SetConsoleMode, consoleInHandle, saveFlags    ; restore console mode.
	jmp L5

L2:	mov BYTE PTR [rdi],0		; insert null byte at end of string

L5:	popaq
	mov rax,bytesRead
	ret
ReadString ENDP


;------------------------------------------------------------
SetTextColor PROC
;
; Change the color of all subsequent text output.
; Receives: AX = attribute. Bits 0-3 are the foreground
; 	color, and bits 4-7 are the background color.
; Returns: nothing
; Last update: 6/20/05
;------------------------------------------------------------

	pushaq
	CheckInit

 	INVOKE SetConsoleTextAttribute, consoleOutHandle, ax

	popaq
	ret
SetTextColor ENDP

;---------------------------------------------------------
StrLength PROC
;
; Returns the length of a null-terminated string.
; Receives: RDX points to the string.
; Returns: RAX = string length.
; Re-enabled 12/11/2011 (was previously commented out)
;---------------------------------------------------------
    push	rdx
    mov	rax,0     	       ; holds character count

L1: cmp	BYTE PTR [rdx],0	  ; end of string?
    je	L2	                 ; yes: quit
    inc	rdx	                 ; no: point to next
    inc	rax	                 ; add 1 to count
    jmp	L1

L2: pop	rdx
    ret
StrLength ENDP

;----------------------------------------------------------
Str_compare PROC USES rax rdx rsi rdi,
	string1:PTR BYTE,
	string2:PTR BYTE
;
; Compare two strings. Affects the Zero and Carry flags 
; exactly as they would be by the CMP instruction.
; Returns: nothing
;-----------------------------------------------------
    mov rsi,string1
    mov rdi,string2

L1: mov  al,[rsi]
    mov  dl,[rdi]
    cmp  al,0    		; end of string1?
    jne  L2      		; no
    cmp  dl,0    		; yes: end of string2?
    jne  L2      		; no
    jmp  L3      		; yes, exit with ZF = 1

L2: inc  rsi      		; point to next
    inc  rdi
    cmp  al,dl   		; chars equal?
    je   L1      		; yes: continue loop
                 		; no: exit with flags set
L3: ret
Str_compare ENDP


;---------------------------------------------------------
Str_copy PROC USES rax rcx rsi rdi,
 	source:PTR BYTE, 		; source string
 	target:PTR BYTE		; target string
;
; Copy a string from source to target.
; Returns: nothing
; Requires: the target string must contain enough
; space to hold a copy of the source string.
;----------------------------------------------------------
	INVOKE Str_length,source 		; RAX = length source
	mov rcx,rax		               ; REP count
	inc rcx         		          ; add 1 for null byte
	mov rsi,source
	mov rdi,target
	cld               		          ; direction = up
	rep movsb      		          ; copy the string
	ret
Str_copy ENDP


;---------------------------------------------------------
Str_length PROC USES r11,
	pString:PTR BYTE	; pointer to string
;
; Return the length of a null-terminated string.
; Receives: pString - pointer to a string
; Returns: RAX = string length
;---------------------------------------------------------
	mov r11,pString
	mov rax,0     	                ; character count
L1:
	cmp BYTE PTR [r11],0	      ; end of string?
	je  L2	                     ; yes: quit
	inc r11	                     ; no: point to next
	inc rax	                     ; add 1 to count
	jmp L1
L2: ret
Str_length ENDP

;-----------------------------------------------------------
Str_trim PROC USES rax rcx rdi,
	pString:PTR BYTE,			; points to string
	char:BYTE					; char to remove
;
; Remove all occurences of a given character from
; the end of a string. 
; Returns: nothing
; Last update: 5/2/09
;-----------------------------------------------------------
	mov  rdi,pString
	INVOKE Str_length,rdi         ; puts length in RAX
	cmp  rax,0                    ; length zero?
	je   L3                       ; yes: exit now
	mov  rcx,rax                  ; no: RCX = string length
	dec  rax                      
	add  rdi,rax                  ; point to null byte at end
	
L1:	mov  al,[rdi]				; get a character
    	cmp  al,char                  ; character to be trimmed?
    	jne  L2                       ; no: insert null byte
    	dec  rdi                      ; yes: keep backing up
     loop L1                       ; until beginning reached

L2:  mov  BYTE PTR [rdi+1],0       ; insert a null byte
L3:  ret
Str_trim ENDP


COMMENT !
******************* OLD VERSION - NO LONGER USED ************
;-----------------------------------------------------------
Str_trim PROC USES rax rcx rdi,
	pString:PTR BYTE,		; points to string
	char:BYTE		; char to remove
;
; Remove all occurences of a given character from
; the end of a string.
; Returns: nothing
; Last update: 1/18/02
;-----------------------------------------------------------
	mov  rdi,pString
	INVOKE Str_length,rdi		; returns length in RAX
	cmp  rax,0		; zero-length string?
	je   L2		; yes: exit
	mov  rcx,rax		; no: counter = string length
	dec  rax
	add  rdi,rax		; RDI points to last char
	mov  al,char		; char to trim
	std		; direction = reverse
	repe scasb		; skip past trim character
	jne  L1		; removed first character?
	dec  rdi		; adjust RDI: ZF=1 && RCX=0
L1:	mov  BYTE PTR [rdi+2],0		; insert null byte
L2:	ret
Str_trim ENDP
********************************************************** !

;---------------------------------------------------
Str_ucase PROC USES rax rsi,
	pString:PTR BYTE
; Convert a null-terminated string to upper case.
; Receives: pString - a pointer to the string
; Returns: nothing
; Last update: 1/18/02
;---------------------------------------------------
	mov rsi,pString
L1:
	mov al,[rsi]		; get char
	cmp al,0		; end of string?
	je  L3		; yes: quit
	cmp al,'a'		; below "a"?
	jb  L2
	cmp al,'z'		; above "z"?
	ja  L2
	and BYTE PTR [rsi], 11011111b	; convert the char

L2:	inc rsi		; next char
	jmp L1

L3: ret
Str_ucase ENDP


;------------------------------------------------------
WaitMsg PROC
;
; Displays a prompt and waits for the user to press a key.
; Receives: nothing
; Returns: nothing
; Last update: 6/9/05
;------------------------------------------------------
.data
waitmsgstr BYTE "Press any key to continue...",0
.code
	pushaq

	lea	rdx, [waitmsgstr]
	call	WriteString
	call	ReadChar	

	popaq
	ret
WaitMsg ENDP


;------------------------------------------------------
WriteBin PROC
;
; Writes a 32-bit integer to the console window in
; binary format. Converted to a shell that calls the
; WriteBinB procedure, to be compatible with the
; library documentation in Chapter 5.
; Receives: RAX = the integer to write
; Returns: nothing
;
; Last update: 11/18/02
;------------------------------------------------------

	push rbx
	mov  rbx,4	; select doubleword format
	call WriteBinB
	pop  rbx

	ret
WriteBin ENDP


;------------------------------------------------------
WriteBinB PROC
;
; Writes a 32-bit integer to the console window in
; binary format.
; Receives: RAX = the integer to write
;           RBX = display size (1,2,4)
; Returns: nothing
;
; Last update: 11/18/02  (added)
;------------------------------------------------------
	pushaq

    cmp   rbx,1   	; ensure RBX is 1, 2, or 4
    jz    WB0
    cmp   rbx,2
    jz    WB0
    mov   rbx,4   	; set to 4 (default) even if it was 4
WB0:
    mov   rcx,rbx
    shl   rcx,1   	; number of 4-bit groups in low end of RAX
    cmp   rbx,4
    jz    WB0A
    ror   rax,8   	; assume TYPE==1 and ROR byte
    cmp   rbx,1
    jz    WB0A    	; good assumption
    ror   rax,8   	; TYPE==2 so ROR another byte
WB0A:

	lea   rsi, buffer

WB1:
	push  rcx	; save loop count

	mov   rcx,4	; 4 bits in each group
WB1A:
	shl   rax,1	; shift RAX left into Carry flag
	mov   BYTE PTR [rsi],'0'	; choose '0' as default digit
	jnc   WB2	; if no carry, then jump to L2
	mov   BYTE PTR [rsi],'1'	; else move '1' to DL
WB2:
	inc   rsi
	Loop  WB1A	; go to next bit within group

	mov   BYTE PTR [rsi],' '  	; insert a blank space
	inc   rsi	; between groups
	pop   rcx	; restore outer loop count
	loop  WB1	; begin next 4-bit group

    dec  rsi    	; eliminate the trailing space
	mov  BYTE PTR [rsi],0	; insert null byte at end
    lea  rdx, buffer	; display the buffer
	call WriteString

	popaq
	ret
WriteBinB ENDP


;------------------------------------------------------
WriteChar PROC
;
; Write a character to the console window
; Recevies: AL = character
; Last update: 10/30/02
; Note: WriteConole will not work unless direction flag is clear.
;------------------------------------------------------
	pushaq
	pushfq	; save flags
	CheckInit

	mov  buffer,al

	cld	; clear direction flag
	INVOKE WriteConsole,
	  consoleOutHandle,	; console output handle
	  ADDR buffer,	; points to string
	  1,	; string length
	  ADDR bytesWritten,  	; returns number of bytes written
	  0

	pushaq	; restore flags
	popaq
	ret
WriteChar ENDP


;-----------------------------------------------------
WriteDec PROC
;
; Writes an unsigned 32-bit decimal number to
; the console window. Input parameters: RAX = the
; number to write.
; Last update: 6/8/2005
;------------------------------------------------------
.data
; There will be as many as 10 digits.
WDBUFFER_SIZE = 12

bufferL BYTE WDBUFFER_SIZE DUP(?),0

.code
	pushaq
	CheckInit

	mov   rcx,0           ; digit counter
	lea   rdi, bufferL
	add   rdi, (WDBUFFER_SIZE - 1)
	mov   rbx,10	; decimal number base

WI1:mov   rdx,0          	; clear dividend to zero
	div   rbx            	; divide RAX by the radix

	xchg  rax,rdx        	; swap quotient, remainder
	call  AsciiDigit     	; convert AL to ASCII
	mov   [rdi],al       	; save the digit
	dec   rdi            	; back up in buffer
	xchg  rax,rdx        	; swap quotient, remainder

	inc   rcx            	; increment digit count
	or    rax,rax        	; quotient = 0?
	jnz   WI1            	; no, divide again

	 ; Display the digits (CX = count)
WI3:
	 inc   rdi
	 mov   rdx,rdi
	 call  WriteString

WI4:
	 popaq	; restore 32-bit registers
	 ret
WriteDec ENDP


;------------------------------------------------------
WriteHex PROC
;
; Writes an unsigned 64-bit hexadecimal number to
; the console window.
; Input parameters: RAX = the number to write.
; Shell interface for WriteHexB, to retain compatibility
; with the documentation in Chapter 5.
;
; Last update: 11/18/02
;------------------------------------------------------
	push rbx
	mov  rbx,8
	call WriteHexB
	pop  rbx
	ret
WriteHex ENDP

;------------------------------------------------------
WriteHexB PROC
	LOCAL displaySize:QWORD
;
; Writes an unsigned 64-bit hexadecimal number to
; the console window.
; Receives: RAX = the number to write. RBX = display size (1,2,4,8)
; Returns: nothing
;
; Last update: 11/18/02
;------------------------------------------------------

QUADEWORD_BUFSIZE = 16

.data
bufferLHB BYTE QUADEWORD_BUFSIZE DUP(?),0

.code
	pushaq               	; save all 32-bit data registers
	mov displaySize, rbx	; save component size
	xor r11, r11			; empty rdi register

; Clear unused bits from RAX to avoid a divide overflow.
; Also, verify that RBX contains either 1, 2, or 4. If any
; other value is found, default to 4.

.IF RBX == 1						; check specified display size
	and  rax, 0FFh					; byte == 1
.ELSE
	.IF RBX == 2
	  and  rax, 0FFFFh				; word == 2
	.ELSE
		.IF RBX == 4
	  	  mov displaySize, 4			; default (doubleword) == 4
		.ELSE
		  mov displaySize, 8			; default (quadword) == 8
		.ENDIF
	.ENDIF
.ENDIF

	CheckInit
	
	lea	  r10, bufferLHB
	mov   r11, displaySize			; let RDI point to the end of the buffer:
	shl   r11, 1					; multiply by 2 (2 digits per byte)
	mov   byte ptr [r10+r11], 0 	; store null string terminator
	dec   r11						; back up one position

	xor   rcx, rcx           		; clear digit counter
	mov   rbx, 16					; hexadecimal base (divisor)

L1:
	xor   rdx, rdx          		; clear upper dividend
	div   rbx            			; divide RAX by the base

	xchg  rax, rdx        			; swap quotient, remainder
	call  AsciiDigit     			; convert AL to ASCII
	mov   byte ptr [r10+r11], al 	; save the digit
	dec   r11             			; back up in buffer
	xchg  rax, rdx        			; swap quotient, remainder

	inc   rcx             			; increment digit count
	or    rax, rax        			; quotient = 0?
	jnz   L1           				; no, divide again

	; Insert leading zeros

	mov   rax, displaySize	; set RAX to the
	shl   rax, 1			; number of digits to print
	sub   rax, rcx			; subtract the actual digit count
	jz    L3           		; display now if no leading zeros required
	mov   rcx, rax         	; CX = number of leading zeros to insert

L2:
	mov   byte ptr [r10+r11], '0'  ; insert a zero
	dec   r11                  	   ; back up
	loop  L2                	   ; continue the loop

	; Display the digits. RCX contains the number of
	; digits to display, and RDX points to the first digit.
L3:
	mov   rcx,displaySize	; output format size
	shl   rcx,1         	; multiply by 2
	inc   r11
	lea   rdx, bufferLHB
	add   rdx, r11
	call  WriteString

	popaq	; restore 32-bit registers
	ret
WriteHexB ENDP


;-----------------------------------------------------
WriteInt PROC
;
; Writes a 32-bit signed binary integer to the console window
; in ASCII decimal.
; Receives: RAX = the integer
; Returns:  nothing
; Comments: Displays a leading sign, no leading zeros.
; Last update: 7/11/01
;-----------------------------------------------------
WI_Bufsize = 12
true  =   1
false =   0
.data
buffer_B  BYTE  WI_Bufsize DUP(0),0  ; buffer to hold digits
.data?
neg_flag  BYTE  ?

.code
	pushaq
	CheckInit

	mov   neg_flag,false    ; assume neg_flag is false
	or    rax,rax             ; is AX positive?
	jns   WIS1              ; yes: jump to B1
	neg   rax                ; no: make it positive
	mov   neg_flag,true     ; set neg_flag to true

WIS1:
	mov   rcx,0              ; digit count = 0
	lea   rdi, buffer_B
	add   rdi,(WI_Bufsize-1)
	mov   rbx,10             ; will divide by 10

WIS2:
	mov   rdx,0              ; set dividend to 0
	div   rbx                ; divide AX by 10
	or    dl,30h            ; convert remainder to ASCII
	dec   rdi                ; reverse through the buffer
	mov   [rdi],dl           ; store ASCII digit
	inc   rcx                ; increment digit count
	or    rax,rax             ; quotient > 0?
	jnz   WIS2              ; yes: divide again

	; Insert the sign.

	dec   rdi	; back up in the buffer
	inc   rcx               	; increment counter
	mov   BYTE PTR [rdi],'+' 	; insert plus sign
	cmp   neg_flag,false    	; was the number positive?
	jz    WIS3              	; yes
	mov   BYTE PTR [rdi],'-' 	; no: insert negative sign

WIS3:	; Display the number
	mov  rdx,rdi
	call WriteString

	popaq
	ret
WriteInt ENDP

NoNameCode = 1;          Special nonprintable code to signal that
              ;          WriteStackFrame was called.
WriteStackFrameNameSize = 64   ; Size of WriteStackFrameName's stack frame
WriteStackFrameSize = 20       ; Size of WriteStackFrame's stack frame

.code
;---------------------------------------------------
WriteStackFrameName PROC USES RAX RBX RCX RDX RSI,
           numParam:DWORD,     ; number of parameters passed to the procedure
           numLocalVal: DWORD, ; number of DWord local variables
           numSavedReg: DWORD, ; number of saved registers
           procName: PTR BYTE  ; pointer to name of procedure
       LOCAL theReturn:  DWORD, theBase:  DWORD, \
             firstLocal: DWORD, firstSaved: DWORD, \
             specialFirstSaved: DWORD

; When called properly from a procedure with a stack frame, it prints
; out the stack frame for the procedure.  Each item is labeled with its
; purpose: parameter, return address, saved ebp, local variable or saved
; register.   The items pointed by ebp and esp are marked.

; Requires:  The procedure has a stack frame including the return address
;            and saved base pointer.
;            It is suffient that procedure's PROC statement includes either
;            at least one local variable or one parameter.  If the procedure's
;            PROC statement does not include either of these items, it is
;            sufficient if the procedure begins with
;                  push rbp
;                  mov  ebp, esp
;            and the stack frame is completed before this procedure is
;            INVOKEd providing the procedure does not have a USES clause.
;            If there is a USES clause, but no parameters or local variables,
;            the modified structure is printed
; Parameters passed on stack using STDCALL:
;            numParam:    number of parameters
;            numLocalVal: number of DWORDS of local variables
;            numSavedReg: number of saved registers
;            ptrProcName: pointer to name of procedure
; Returns:  nothing
; Sample use:
;         myProc PROC USES rbx, rcx, rdx      ; saves 3 registers
;                   val:DWORD;                ; has 1 parameter
;               LOCAL a:DWORD, b:DWORD        ; has 2 local varables
;         .data
;         myProcName  BYTE "myProc", 0
;         .code
;               INVOKE writeStackFrameName, 1, 2, 3, ADDR myProcName
;  Comment:  The number parameters are ordered by the order of the
;            corresponding items in the stack frame.
;
; Author:  James Brink, Pacific Lutheran University
; Last update: 4/6/2005
;---------------------------------------------------
.data
LblStack  BYTE "Stack Frame ",  0
LblFor    Byte "for ", 0
LblEbp    BYTE "  ebp", 0             ; used for offsets from ebp
LblParam  BYTE " (parameter)", 0
LblEbpPtr BYTE " (saved ebp) <--- ebp", 0
LblSaved  BYTE " (saved register)", 0
LblLocal  BYTE " (local variable)", 0
LblReturn BYTE " (return address)", 0
LblEsp    BYTE " <--- esp", 13, 10, 0 ; adds blank line at end of stack frame
BadStackFrameMsg BYTE "The stack frame is invalid", 0
.code
        ;  register usage:
        ;  rax:  value to be printed
        ;  rbx:  offset from ebp
        ;  rcx:  item counter
        ;  rdx:  location of string being printed
        ;  rsi:  memory location of stack frame item

        ; print title
	lea  rdx,  LblStack
	call writeString
	mov  rsi, procName
	          ; NOTE:  rsi must not be changed until we get to
	          ;        the section for calculating the location
	          ;        of the caller's ebp at L0a:
	cmp  BYTE PTR [rsi], 0      ; is the name string blank?
	je   L0                     ; if so, just go to a new line
	cmp  BYTE PTR [rsi], NoNameCode
	                            ; is the name the special code
	                            ; from WriteStackFrame?
	je   L0                     ; if so, just go to a new line
	lea  rdx,  LblFor     ; if not, add "for "
	call writeString
	mov  rdx, procName          ; and print name
	call writeString
L0:	call crlf
	call crlf

        mov  rcx, 0            ; initialize sum of items in stack frame
        mov  rbx, 0            ; initialize sum of items in stack frame
                               ;    preceding the base pointer

        ; check for special stack frame condition
        mov  eax, numLocalVal  ; Special condition:  numLocalVal = 0
        cmp  rax, 0
        ja   Normal

        mov  eax, numParam     ; Special condition:  numParm = 0
        cmp  rax, 0
        ja   Normal

        mov  eax, numSavedReg  ; Special condition:  numSaveReg > 0
        cmp  rax, 0
        ja   Special

Normal:	mov  eax, numSavedReg  ; get number of parameters
	add  rcx, rax          ; add to number of items in stack frame
	mov  firstSaved, ecx   ; save item number of the first saved register
	mov  specialFirstSaved, 0
	                       ; no special saved registers

	mov  eax, numLocalVal  ; get number of local variable DWords
	add  rcx, rax          ; add to number of items in stack frame
	mov  firstLocal, ecx   ; save item number of first local variable

	add  rcx, 1            ; add 1 for the saved ebp
	mov  theBase, ecx      ; save item number of the base pointer

	add  rcx, 1            ; add 1 for the return address
	add  rbx, 1            ; add 1 for items stored above ebp                                                                 ; add for the return address/preceding ebp
	mov  theReturn, ecx    ; save item number of the return pointer

	mov  eax, numParam     ; get number of parameters
	add  rcx, rax          ; add to number of items in stack frame
	add  rbx, rax          ; add for the parameters/preceding ebp

	jmp  L0z

Special:
        ; MASM does not create a stack frame under these conditions:
        ;   The number of parameters is 0
        ;   The number of local variables is 0
        ;   The number of saved (USES) registers is positive.
        ;   The following assumes the procedure processed ebp manually
        ;   because MASM does not push it under these conditions.
        mov  firstSaved, ecx   ; there are no "regular" saved registers
        mov  firstLocal, ecx   ; there are no local variables

        add  rcx, 1            ; add 1 for the saved ebp
        mov  theBase, ecx      ; save item number of the base pointer

        mov  eax, numSavedReg  ; get number of saved registers
        add  rcx, rax          ; add to number of items in the stack frame
        add  rbx, rax          ; add for the items preceding ebp
        mov  specialFirstSaved, ecx

	add  rcx, 1            ; add 1 for the return address
	add  rbx, 1            ; add 1 for items stored above ebp                                                                 ; add for the return address/preceding ebp
	mov  theReturn, ecx    ; save item number of the return pointer

        mov  rax, rsp
        add  rax, 44
        cmp  rax, rsi

L0z:
	;  rcx now contains the number of items in the stack frame
        ;  rbx now contains the number of items preceding the base pointer

	; determine the size of those items preceding the base pointer
	shl  rbx, 2            ; multiply by 4

        ; determine location of caller's saved ebp
L0a:	cmp  BYTE PTR [rsi], NoNameCode
	                       ; check for special code
L0b:    mov  rsi, [rbp]        ; get the ebp (1 indirection
                               ; mov does not change flags
        jne  L0c               ; if not special code, skip the next step
        mov  rsi, [rsi]        ; 2nd indirection if called by WriteStackFrame
L0c:                           ; rsi has pointer into caller's stack frame
;   At this point rsi contains the location for the caller's saved ebp

;   Check special case to make sure ebp and esp agree.
;   Printing the stack frame cannot be printed if ebp has not been pushed
        mov  eax, specialFirstSaved ; Was this a special case?
        cmp  rax, 0           ; If so specialFirstSaved would be 0
        je   L0e              ; If not, continue normal processing
        mov  rax, rsp         ; Calculate loc. of last entry before
                              ; of WriteStackFrameNames stack frame
        add  eax, WriteStackFrameNameSize
        cmp  rax, rsi         ; does it equal the location of the base pointer?
        je   L0e              ; if so, continue normal processing
                              ; if not chec to see if procedure was called
                              ; by writeStackFrame
        add  eax, WriteStackFrameSize
        cmp  rax, rsi         ; does it equal the location of the base pointer?
        jne  badStackFrame    ; if not, the stack frame is invalid
                              ; These are not perfect test as we haven't
                              ; checked to see which case we are in.

; Continue normal processing by calculating its stack frame size

L0e:    add  rsi, rbx          ; calculate beginning of the caller's stack
                               ; frame (highest memory used)


 	; *** loop to print stack frame
 	; Note:  the order of some the following checks is important                                                                                                                                                                                  ck frame  ***
L1:	; write value and beginning offset from basepointer
        mov  rax, [rsi]        ; write item in stack frame
	call writeHex
	lea  rdx, LblEbp ; write " ebp"
	call writeString
	mov  rax, rbx          ; write offset from base pointer
	call writeInt
	; check for special labels
	cmp  ecx, theReturn    ; check for return address item
	jne  L2
	lea  rdx, LblReturn
	jmp  LPrint

L2:     cmp  ecx, theBase      ; check for base pointer
        jne  L2a
        lea  rdx, LblEbpPtr
        jmp  LPrint

L2a:    cmp  ecx, specialFirstSaved ; Check for special saved registers
	ja   L3
	lea  rdx, LblSaved
	jmp  LPrint

L3:     cmp  ecx, firstSaved   ; check for saved registers
        ja   L4
        lea  rdx, LblSaved
        jmp  LPrint
L4:     cmp  ecx, firstLocal   ; check for local variables
        ja   L5
        lea  rdx, LblLocal
        jmp  LPrint
L5:     lea  rdx, LblParam
LPrint: call writeString
        cmp  rcx, 1            ; check for last item in stack frame
        jne  LDone
        lea  rdx, LblEsp
        call writeString
LDone:  ; complete output for line
	call crlf
	; get ready for the next line
	sub  rsi, 4         ; decrement memory location by 4
	sub  rbx, 4         ; decrement offset by 4
	loop LDoneX
	jmp  Return
LDoneX: jmp  L1
Return:
	ret

; Stack frame invalid
BadStackFrame:
        lea  rdx, BadStackFrameMsg
                            ; load message
        call writeString    ; write message
        call crlf
        ret	            ; return without printing stack frame

WriteStackFrameName ENDP

;---------------------------------------------------

WriteStackFrame PROC,
           numParam:DWORD,     ; number of parameters passed to the procedure
           numLocalVal: DWORD, ; number of DWord local variables
           numSavedReg: DWORD  ; number of saved registers

; When called properly from a procedure with a stack frame, it prints
; out the stack frame for the procedure.  Each item is labeled with its
; purpose: parameter, return address, saved ebp, local variable or saved
; register.   The items pointed by ebp and esp are marked.

; Requires:  The procedure has a stack frame including the return address
;            and saved base pointer.
;            It is suffient that procedure's PROC statement includes either
;            at least one local variable or one parameter.  If the procedure's
;            PROC statement does not include either of these items, it is
;            sufficient if the procedure begins with
;                  push rbp
;                  mov  ebp, esp
;            and the stack frame is completed before this procedure is
;            INVOKEd providing the procedure does not have a USES clause.
;            If there is a USES clause, but no parameters or local variables,
;            the modified structure is printed
; Parameters passed on stack using STDCALL:
;            numParam:    number of parameters
;            numLocalVal: number of DWORDS of local variables
;            numSavedReg: number of saved registers
; Returns:  nothing
; Sample use:
;         myProc PROC USES rbx, rcx, rdx      ; saves 3 registers
;                   val:DWORD;                ; has 1 parameter
;               LOCAL a:DWORD, b:DWORD        ; has 2 local varables
;         .data
;         myProcName  BYTE "myProc", 0
;         .code
;               INVOKE writeStackFrame, 1, 2, 3
;
; Comments:  The parameters are ordered by the order of the corresponding
;            items in the stack frame.
;
; Author:  James Brink, Pacific Lutheran University
; Last update: 4/6/2005
;---------------------------------------------------
.data
NoName  BYTE  NoNameCode
.code
        INVOKE WriteStackFrameName, numParam, numLocalVal, \
               NumSavedReg, ADDR NoName
                      ; NoNameCode
                      ; Special signal that WriteStackFrameName
                      ; is being called from WriteStackFrame
        ret
WriteStackFrame ENDP



;--------------------------------------------------------
WriteString PROC
;
; Writes a null-terminated string to standard
; output. Input parameter: RDX points to the
; string.
; Last update: 9/7/01
;--------------------------------------------------------
	pushaq

	CheckInit

	INVOKE Str_length, rdx   	; return length of string in RAX
	cld	; must do this before WriteConsole
	INVOKE WriteConsole,
	    consoleOutHandle,     	; console output handle
	    rdx,	; points to string
	    eax,	; string length
	    ADDR bytesWritten,  	; returns number of bytes written
	    0

	popaq
	ret
WriteString ENDP


;--------------------------------------------------------
WriteToFile PROC
;
; Writes a buffer to an output file.
; Receives: RAX = file handle, RDX = buffer offset,
;    RCX = number of bytes to write
; Returns: RAX = number of bytes written to the file.
; Last update: 6/8/2005
;--------------------------------------------------------
.data?
WriteToFile_1 DWORD ?    	; number of bytes written
.code
	mov r14, 0
	INVOKE WriteFile,	; write buffer to file
		rax,	; file handle
		rdx,	; buffer pointer
		ecx,	; number of bytes to write
		ADDR WriteToFile_1,	; number of bytes written
		r14	; overlapped execution flag
	mov	eax, WriteToFile_1	; return value
	ret
WriteToFile ENDP


;----------------------------------------------------
WriteWindowsMsg PROC USES rax rdx
;
; Displays a string containing the most recent error 
; generated by MS-Windows.
; Receives: nothing
; Returns: nothing
; Last updated: 6/10/05
;----------------------------------------------------
.data
WriteWindowsMsg_1 BYTE "Error ",0
WriteWindowsMsg_2 BYTE ": ",0
.data?
pErrorMsg HANDLE ?	; points to error message
messageId DWORD ?
.code
	call	GetLastError
	mov		messageId, eax

; Display the error number.
	lea		rdx, WriteWindowsMsg_1
	call	WriteString
	call	WriteDec	; show error number
	lea		rdx, WriteWindowsMsg_2
	call	WriteString

; Get the corresponding message string.
	INVOKE FormatMessage, FORMAT_MESSAGE_ALLOCATE_BUFFER + \
	  FORMAT_MESSAGE_FROM_SYSTEM, NULL, messageID, NULL,
	  ADDR pErrorMsg, NULL, NULL

; Display the error message generated by MS-Windows.
	mov		rdx, pErrorMsg
	call	WriteString

; Free the error message string.
	INVOKE LocalFree, pErrorMsg

	ret
WriteWindowsMsg ENDP


;*************************************************************
;*                    PRIVATE PROCEDURES                     *
;*************************************************************

; Convert AL to an ASCII digit. Used by WriteHex & WriteDec

AsciiDigit PROC PRIVATE
	 push   rbx
	 lea    rbx, xtable
	;  xlat alternative
	 movzx  rax, byte ptr [rbx + rax]
	 pop    rbx
	 ret
AsciiDigit ENDP


HexByte PROC PRIVATE
; Display the byte in AL in hexadecimal

	pushaq
	mov  dl,al

	rol  dl,4
	mov  al,dl
	and  al,0Fh
	lea  rbx, xtable
	xlat
	mov  buffer,al	; save first char
	rol  dl,4
	mov  al,dl
	and  al,0Fh
	xlat
	mov  [buffer+1],al	; save second char
	mov  [buffer+2],0	; null byte

	lea  rdx, buffer	; display the buffer
	call WriteString

	popaq
	ret
HexByte ENDP

END

