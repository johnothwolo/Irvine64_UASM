; Include file for Irvine32.lib             (Irvine32.inc)

; OPTION CASEMAP:NONE		; optional: make identifiers case-sensitive

INCLUDE SmallWin.inc		; MS-Windows prototypes, structures, and constants
INCLUDE VirtualKeys.inc

.NOLIST
; Last update: 7/29/05

;----------------------------------------
; Procedure Prototypes
;----------------------------------------
CloseFile PROTO		; close a file handle
Clrscr PROTO		; clear the screen
CreateOutputFile PROTO		; create file for writing
Crlf PROTO		; output carriage-return / linefeed
Delay PROTO		; delay for n milliseconds
DumpMem PROTO		; display memory dump
DumpRegs PROTO		; display register dump
GetCommandTail PROTO		; get command-line string
GetDateTime PROTO,		; get system date and time
  startTime:PTR QWORD
GetMaxXY PROTO		; get console rows and columns
GetMseconds PROTO		; get milliseconds past midnight
GetTextColor PROTO		; Get the console window's color attributes. 
Gotoxy PROTO		; set the cursor position
IsDigit PROTO		; return ZF=1 if AL is a decimal digit
MsgBox PROTO		; display popup message box
MsgBoxAsk PROTO		; display popup yes/no question box
OpenInputFile PROTO		; open file in input mode
ParseDecimal32 PROTO		; convert unsigned decimal string to 32-bit binary
ParseInteger64 PROTO		; convert signed decimal string to 32-bit binary
Randomize PROTO		; reseed random number generator
RandomRange PROTO		; generate random integer in specified range
Random32 PROTO		; generate 32-bit random integer
ReadInt PROTO		; read signed decimal integer from console
ReadChar PROTO		; reach single character from console
ReadDec PROTO		; read unsigned decimal integer from console
ReadFloat PROTO		; read floating-point value from keyboard
ReadFromFile PROTO   		; read buffer from input file
ReadHex PROTO		; read hexadecimal integer from console
ReadKey PROTO		; Reads keyboard input if available (4/6/03)
ReadKeyFlush PROTO		; Flush ReadKey buffer and repeat counter (4/6/03)
ReadString PROTO		; read string from console
SetTextColor PROTO		; set console text color
ShowFPUStack PROTO		; write floating-point stack to console window
StrLength PROTO		; returns the length of a string
WaitMsg PROTO		; display wait message, wait for Enter key
WriteBin PROTO		; write integer to output in binary format
WriteBinB PROTO		; write binary integer in byte, word,or doubleword format
WriteChar PROTO		; write single character to output
WriteDec PROTO		; write unsigned decimal integer to output
WriteFloat PROTO		; write ST(0) to console in floating-point format
WriteHex PROTO		; write hexadecimal integer to output
WriteHexB PROTO		; write hexadecimal integer in word or doubleword format
WriteInt PROTO		; write signed integer to output
;WriteStackFrame		; write stack frame data (James Brink--see proto later in this file)
;WriteStackFrameName		; write stack frame data with proc name (James Brink)
WriteString PROTO		; write null-terminated string to output
WriteToFile PROTO		; write a buffer to an output file
WriteWindowsMsg PROTO		; write last error message generated by MS-Windows


; Copy a source string to a target string.
Str_copy PROTO,
 	source:PTR BYTE,
 	target:PTR BYTE

; Return the length of a null-terminated string..
Str_length PROTO,
	pString:PTR BYTE

; Compare string1 to string2. Set the Zero and
; Carry flags in the same way as the CMP instruction.
Str_compare PROTO,
	string1:PTR BYTE,
	string2:PTR BYTE

; Trim a given trailing character from a string.
; The second argument is the character to trim.
Str_trim PROTO,
	pString:PTR BYTE,
	char:BYTE

; Convert a null-terminated string to upper case.
Str_ucase PROTO,
	pString:PTR BYTE

;******** Procedures by James Brink ********************************
; Used by permission.

WriteStackFrameName PROTO,
           numParam:DWORD,     ; number of parameters passed to the procedure
           numLocalVal: DWORD, ; number of DWordLocal variables
           numSavedReg: DWORD, ; number of saved registers
           procName: PTR BYTE

WriteStackFrame PROTO,
           numParam:DWORD,     ; number of parameters passed to the procedure
           numLocalVal: DWORD, ; number of DWordLocal variables
           numSavedReg: DWORD  ; number of saved registers


;-----------------------------------
; Standard 4-bit color definitions
;-----------------------------------
black        = 0000b
blue         = 0001b
green        = 0010b
cyan         = 0011b
red          = 0100b
magenta      = 0101b
brown        = 0110b
lightGray    = 0111b
gray         = 1000b
lightBlue    = 1001b
lightGreen   = 1010b
lightCyan    = 1011b
lightRed     = 1100b
lightMagenta = 1101b
yellow       = 1110b
white        = 1111b

; This structure is returned by the FSTSW
; instruction in protected mode:

FPU_ENVIRON STRUCT
	controlWord    WORD ?
	ALIGN DWORD
	statusWord     WORD ?
	ALIGN DWORD
	tagWord        WORD ?
	ALIGN DWORD
	instrPointerOffset     DWORD ?
	instrPointerSelector   DWORD ?
	operandPointerOffset   DWORD ?
	operandPointerSelector WORD ?
	WORD ?	; not used
FPU_ENVIRON ENDS

.LIST