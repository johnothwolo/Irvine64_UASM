.NOLIST
COMMENT !
; (Macros.inc) - Include file containing Macros
; for Assembly Language for Intel-Based Computers
; 7th Edition, by Kip Irvine. Copyright 2014, Prentice-Hall Inc,
; all rights reserved.
;
; Update history:
; 1/1/2014 - Revised for 7th Edition

; List of macros in this file:

mDump MACRO varName:REQ, useLabel
	-- Displays a variable, using its name and default attributes.
	-- useLabel is optional; if nonblank, the variable name will be displayed
	--    just before the dump.

mDumpMem MACRO address:REQ, itemCount:REQ, componentSize:REQ
	-- Displays a dump of a range of memory.

mGotoxy MACRO X:REQ, Y:REQ
	-- Sets the cursor position.

mReadString MACRO varName:REQ
	-- Reads a string from the keyboard.

mShow MACRO itsName:REQ, format:=<HIN>
	-- Displays a variable or register in various formats.

mShowRegister MACRO regName, regValue
	-- Displays a 64-bit register name and contents in hexadecimal.

mWrite MACRO text:REQ
	-- Writes a string literal to standard output.

mWriteLn MACRO text:REQ
	-- Writes a string literal to standard output.  *** DEPRECATED in 5th Edition ***

mWriteSpace MACRO count
	-- Writes one or more spaces to the console window.
	-- <count> is optional

mWriteString MACRO buffer:REQ
	-- Writes a string variable's contents to the console window.



END COMMENT ***************************************** !

; Introduced in Chapter 10 as an example of a Macro function.

IsDefined MACRO symbol
    IFDEF symbol
        EXITM <-1>              ;; True
    ELSE
        EXITM <0>               ;; False
    ENDIF
ENDM

;----------------------------------------------------
mDump MACRO varName:REQ, useLabel
;
; Displays a variable, using its known attributes
; Receives: varName, the name of a variable.
;    If useLabel is nonblank, the name of the
;    variable is displayed.
;----------------------------------------------------
	call Crlf
	IFNB <useLabel>
	  mWrite "Variable name: &varName"
	ELSE
	  mWrite " "
	ENDIF
	mDumpMem ADDR varName, LENGTHOF varName, TYPE varName
ENDM

;------------------------------------------------------
mDumpMem MACRO address:REQ, itemCount:REQ, componentSize:REQ
;
; Receives: memory offset, count of the number of items
;   to display, and the size of each memory component.
; Avoid passing RBX, RCX, and RSI as arguments.
;------------------------------------------------------
	push	rbx
	push	rcx
	push	rsi
	mov	rsi,address
	mov	rcx,itemCount
	mov	rbx,componentSize
	call	DumpMem
	pop	rsi
	pop	rcx
	pop	rbx
ENDM

;------------------------------------------------------
mGotoxy MACRO X:REQ, Y:REQ
;  
; Sets the cursor position. 
; Receives: X and Y coordinates (type BYTE). Avoid 
;    passing DH and DL as arguments.
;------------------------------------------------------
	push rdx
	mov  dh,Y
	mov  dl,X
	call Gotoxy
	pop  rdx
ENDM

;------------------------------------------------------
mReadString MACRO varName:REQ
;
; Reads from standard input into a buffer.
; Receives: the name of the buffer. Avoid passing 
;    RCX and RDX as arguments.
;------------------------------------------------------
	push rcx
	push rdx
	lea  rdx, varName
	mov  rcx,SIZEOF varName
	call ReadString
	pop  rdx
	pop  rcx
ENDM

;---------------------------------------------------
mShow MACRO itsName:REQ, format:=<HIN>
             LOCAL tempStr
; Displays a register's or variable's name and contents.
;
; Receives:
;	itsName is the name of a register or variable.
; 	format is an ordered string consisting of the format codes:
;	  H - hexadecimal
;	  D - unsigned decimal
;     I - signed decimal
;     B - binary
;     N - append a newline (CR/LF) (may appear multiple times)
;
; The default format is "HIN"
;
; The variable or register's value is displayed in each format,
; in the order specified by the caller, on a single output line.
; Create by Gerald Cahill and Kip Irvine.
;---------------------------------------------------
.data
tempStr BYTE "  &itsName = ",0
.code
    pushad

;;If itsName is a register name, define its type

IF (OPATTR (itsName)) AND 00010000b ;;true if register name

  MSHOWITSNAMETYPE = 0 ;; initialize TYPE to not found

  FOR reg8,<al,ah,bl,bh,cl,ch,dl,dh>
    IFIDNI <itsName>,<reg8>
      MSHOWITSNAMETYPE = 1
      movzx rcx,itsName  ;; get unsigned value
      movsx rdx,itsName  ;; get signed value
    ENDIF
  ENDM

  FOR reg16,<ax,bx,cx,dx,si,di,bp,sp>
    IFIDNI <itsName>,<reg16>
      MSHOWITSNAMETYPE = 2
      movzx rcx,itsName  ;; get unsigned value
      movsx rdx,itsName  ;; get signed value
    ENDIF
  ENDM

  FOR regseg,<cs,ds,es,fs,gs,ss>
    IFIDNI <itsName>,<regseg>
      MSHOWITSNAMETYPE = 2
      mov   ax,itsName  ;; get value into general purpose reg.
      movsx rdx,ax      ;; get signed value (who would want it?)
      movzx rcx,ax      ;; get unsigned value
    ENDIF
  ENDM

  FOR reg32,<rax,rbx,rcx,rdx,rsi,rdi,ebp,esp>
    IFIDNI <itsName>,<reg32>
      MSHOWITSNAMETYPE = 4
      mov   rcx,itsName  ;; get unsigned value
      mov   rdx,itsName  ;; get signed value
    ENDIF
  ENDM

ELSE ;; itsName is not a register name, assume variable name

  MSHOWITSNAMETYPE = TYPE itsName
  IF MSHOWITSNAMETYPE EQ 4
      mov   rcx,itsName  ;; get unsigned value
      mov   rdx,rcx      ;; get signed value
  ELSE
      movzx rcx,itsName  ;; get unsigned value
      movsx rdx,itsName  ;; get signed value
  ENDIF

ENDIF ;;OPATTR
                         ;; Display the register or variable's name
    push rdx
    lea  rdx, tempStr
    call WriteString
    pop  rdx

                        ;; Display the register or variable's contents
FORC fmt,<format>

  IFIDNI <fmt>,<H>      ;; H - write unsigned hex
    mov rax,rcx         ;; get unsigned
    mov rbx,MSHOWITSNAMETYPE
    call WriteHexB    	;; write in hexadecimal
    mWrite "h  "
  ENDIF

  IFIDNI <fmt>,<D>      ;; D - write unsigned dec
    mov rax,rcx         ;; get unsigned
    call WriteDec
    mWrite "d  "
  ENDIF

  IFIDNI <fmt>,<I>      ;; I - write signed Integer
    mov rax,rdx         ;; get signed
    call WriteInt
    mWrite "d  "
  ENDIF

  IFIDNI <fmt>,<B>      ;; B - write unsigned binary
    mov rax,rcx         ;; get unsigned
    mov rbx,MSHOWITSNAMETYPE
    call WriteBinB      ;; display binary
    mWrite "b  "
  ENDIF

  IFIDNI <fmt>,<N>      ;; N - write new line
    call Crlf
  ENDIF

ENDM ;end FORC

	popad
ENDM 	;; end mShow macro

;---------------------------------------------------
mShowRegister MACRO regName, regValue
LOCAL tempStr
;
; Displays a 32-bit register name and contents.
; Receives: the register name, the register value.
;---------------------------------------------------
.data
tempStr BYTE "  &regName=",0
.code
	push rax

; Display the register name
	push	rdx
	lea	rdx, tempStr
	call	WriteString
	pop	rdx

; Display the register contents
	mov	rax,regValue
	call	WriteHex
	pop	rax
ENDM

;------------------------------------------------------
mWrite MACRO text:REQ
;
; Writes a string literal to standard output.
; Receives: a string enclosed in single or double 
;   quotes (null terminator not required).
;------------------------------------------------------
LOCAL string
	.data		;; local data
	string BYTE text,0	;; define the string
	.code
	push	rdx
	lea	rdx, string
	call	WriteString
	pop	rdx
ENDM

;------------------------------------------------------
mWriteLn MACRO text:REQ
;
; Writes a string literal to standard output, followined by Crlf
; Receives: a string enclosed in single or double 
;   quotes (null terminator not required).
; DEPRECATED in the Fifth edition.
;------------------------------------------------------
	mWrite text
	call	Crlf
ENDM

;------------------------------------------------------
mWriteSpace MACRO count:=<1>
;
; Writes one or more spaces to standard output.
; Receives: an integer specifying the number of spaces.
;   If count is blank, a single space is written.
;------------------------------------------------------
LOCAL spaces
.data
spaces BYTE count DUP(' '),0
.code
	push	rdx
	lea	  rdx, spaces
	call	WriteString
	pop	  rdx
ENDM

;------------------------------------------------------
mWriteString MACRO buffer:REQ
;
; Writes a string variable to standard output.
; Receives: string variable name.
;------------------------------------------------------
	push rdx
	lea  rdx, buffer
	call WriteString
	pop  rdx
ENDM


Startup MACRO
	IF IsDefined( RealMode )
  	  mov ax,@data
	  mov ds,ax
	ENDIF
ENDM

;----------------------------------------------------------------------
; We may later decide to add some macros that are specific to 32-bit
; mode, or to 16-bit mode. The @MODEL value is defined either by
; Irvine16.inc or by Irvine32.inc, when they use the .MODEL directive.
;----------------------------------------------------------------------

IFDEF @MODEL	; @MODEL defined?
	IF @MODEL EQ 7	; 32-bit unique macros begin here...

	ELSE	; 16-bit unique macros begin here...

	ENDIF	; IF @MODEL...
ENDIF	; IFNDEF @MODEL

.LIST