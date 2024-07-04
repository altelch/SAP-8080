;===============================================================
; Stage 1 loader (BIOS/BDOS/CCP)
;
; Disk is organized in logical blocks. CP/M needs to translate
; Sectors and Tracks into LBA. Loader uses 256 Byte mode, LSB of
; Sector is MSB of Byte-Counter -> use only even sector numbers for
; 256 Byte blocks.
;
; Start   End     Contents
; 0x0000	0x00FF	CP/M low-memory, BIOS/BDOS jumps, etc
; 0x0100	0xEFFF	Transient Program Area (TPA)
; 0xE400	0xEBFF	CP/M Command Processor (CCP)       16 Sectors
; 0xEC00	0xF9FF	Basic Disk Operating System (BDOS) 28 Sectors
; 0xFA00	0xFFFF	Basic I/O System (BIOS)            10 Sectors
; 0xF800	0xFFFF	Monitor ROM                         1 kBytes (ROM Mode)
;
; -> requires 24 Blocks to be read
;
; Layout:
;
; Sector   Code
;  0    1  Loader
;  2 - 18  CCP
; 18 - 46  BDOS
; 46 - 56  BIOS
;
;===============================================================
; Revision History
;   1.00  02102023  H.K.
;     Created
;
; (Remember to update the Version String below)
;===============================================================
  .8080

include "location.asm"

START	equ	0000h	  ;Beginning of Loader
STACK equ 0200h   ;Stackpointer in lower RAM

;  CPM locations

CCP     equ CCP_ORG
BDOS    equ BDOS_ORG
BIOS    equ BIOS_ORG
ROM     equ 0F800h

;CCP 16 Sectors
CCPSTART   equ 2
CCPEND     equ CCPSTART+16

;BDOS 28 Sectors
BDOSSTART  equ CCPEND
BDOSEND    equ BDOSSTART+28

;BIOS 10 Sectors
BIOSSTART  equ BDOSEND
BIOSEND    equ BDOSEND+10

; 88-2SIO equates

CONS  equ  10h         ;console status port
COND  equ  11h         ;console data port
TBE   equ  2           ;transmit buffer entry
RDA   equ  1           ;receive data available

; ASCII characters

CR	equ	0Dh
LF	equ	0Ah

;==============================================================
; Start of CP/M Loader
;==============================================================
	org	START

;----------------------------------------------------------
; ATA Harddisk
;
; Error code:
; Bit: Condition:
; 
; 0    1 = DAM not found
; 1    1 = Track 000 not found
; 2    1 = Command aborted
; 3        Reserved
; 4    1 = ID not found
; 5        Reserved
; 6    1 = Uncorrectable ECC error
; 7    1 = Bad block detected
;
; LBA3 Reg:
; Bit 0:3 = LBA bits (24:27)
; Bit 4   = Select Master (0) or Slave (1) drive
; Bit 5   = Always set to 1
; Bit 6   = Always Set to 1 for LBA Mode Access
; Bit 7   = Always set to 1
;
; IDECOM Status:
; Bit: Name: Condition:
; 
; 0   ERR    1 = Previous command ended in an error
; 1   IDX        (not important)
; 2   CORR       (not important)
; 3   DRQ    1 = Data Request Ready (Sector buffer ready for transfer)
; 4   DSC        (not important)
; 5   DF     1 = Write Fault
; 6   RDY    1 = Ready for command
; 7   BUSY   1 = Controller is busy executing a command.
; 
;----------------------------------------------------------

IDEDATA  equ  08H       ;Data I/O Address (R/W)
IDEERR   equ  09H       ;Read: Error code (unused)
IDESEC   equ  0AH       ;Num. sectors multi sector transfer (unused)
SECTOR   equ  0BH       ;Write LBA Reg. 0
TRACK    equ  0CH       ;Write LBA Reg. 1
TRACK2   equ  0DH       ;Write LBA Reg. 2
DRIVE    equ  0EH       ;Write LBA Reg. 3 (Drive)
IDECOM   equ  0FH       ;Read: Status, Write: Issue Command

IDEREAD  equ  020H      ;Read sectors with retry
IDEWRITE equ  030H      ;Write sectors with retry
IDEDRVID equ  0ECH      ;Identify drive

DRVRDY   equ  01000000B ;Ready for command
DRVBUSY  equ  10000000B ;Busy
DRVDATA  equ  00001000B ;Data Ready

  lxi  sp,STACK

;----------------------------
; Print HDBL version message
; On Entry & Exit:
;  hl = 0
;----------------------------
  call PRINTF         ;print the following string
  db   CR,LF,'CP/M Loader 1.00: ',80h

;--------------------------------------------
; Load BIOS from Blocks 1-6
;--------------------------------------------
  lxi  h,BIOS           ;Load BIOS start address
	lxi  d,BIOSSTART      ;First Block of BIOS
LBIOS:
  call RLBA             ;Read  Block
  call MOVE             ;Move Sector data to dest.
  call PRINTF           ;print the following string
  db	'b'+80h

	                      ;H/L contains correct BIOS addr after MOVE
  inx  d                ;Next Block on Disk
  mvi  a,BIOSEND        ;End  Block
	cmp  e                ;Last Block?
	jnz  LBIOS            ;More to load?

;--------------------------------------------
; Load BDOS from Blocks 7-20
;--------------------------------------------
  lxi  h,BDOS           ;Load BIOS start address
	lxi  d,BDOSSTART      ;First Block of BDOS
LBDOS:
  call RLBA             ;Read  Block
  call MOVE             ;Move Sector data to dest.
  call PRINTF           ;print the following string
  db	'd'+80h

	                      ;H/L contains correct BDOS addr after MOVE
  inx  d                ;Next Block on Disk
  mvi  a,BDOSEND        ;End  Block
	cmp  e                ;Last Block?
	jnz  LBDOS            ;More to load?

;--------------------------------------------
; Load CCP from Blocks 21-28
;--------------------------------------------
  lxi  h,CCP            ;Load CCP start address
	lxi  d,CCPSTART       ;First Block of CCP
LCCP:
  call RLBA             ;Read  Block
  call MOVE             ;Move Sector data to dest.
  call PRINTF           ;print the following string
  db	'c'+80h

	                      ;H/L contains correct CCP addr after MOVE
  inx  d                ;Next Block on Disk
  mvi  a,CCPEND         ;End  Block
	cmp  e                ;Last Block?
	jnz  LCCP             ;More to load?

  jmp  BIOS             ;Run code

	hlt                   ;Should never return

;===Subroutine=================================================
;Move sector to memory destination in HL
;==============================================================
MOVE:
  mvi  b,0       ;Byte counter 0
LOOP:
  in   IDEDATA   ;Read byte from HD Buffer
  mov  m,a       ;Store in memory destination HL
  inx  h         ;increment HL
	inr  b         ;increment Byte counter
  mov  a,b       ;b (counter) to a
  cpi  128       ;read 128 Bytes
  jnz  LOOP      ;Repeat if not finished
  ret            ;return

;===Subroutine=================================================
;Set LBA and read sector (stored in D/E)
;==============================================================
RLBA:	
  mov	a,e        ;Sector
  out	SECTOR

  mov a,d        ;Track LSB
  out	TRACK

  xra a          ;a=0
  out TRACK2     ;Track MSB
  out DRIVE      ;Drive 0 (a still 0)

  mvi a,IDEREAD  ;Set Read Command (Fake), clear counter
  out IDECOM     ;.. to command Reg.

;===Subroutine=================================================
;Wait for Disk ready
;==============================================================

HDWAIT:	
  in  IDECOM	      ;Read drive status
  rlc               ;Rotate Busy bit to carry
  jc  HDWAIT        ;Still busy
  rlc               ;Rotate Drive Ready bit to carry
  jnc HDWAIT        ;no: wait for drive ready
  rrc               
  rrc               
  rrc               ;Rotate Error bit to carry
  rnc	            ;No errors: happy return

; 	Fall into error exit

;===Error Exit=================================================
; Report a load error and store error code in RAM at 0.
; Hang here forever, with the INTE light lit.
; On Entry:
;   a=error flag bits
;==============================================================
  sta	0		          ;save a=error flags
  call	LOADPF	    ;CR,LF,'LOAD', then string
  db	  ' ERR',80h

  ei			          ;INTE is error indicator light
FOREVR:
; jmp	FOREVR	      ;N: die here, INTE light lit
  jmp	ROM   	      ;Jump into ROM

;===Subroutine================================================
; Print inline string on the Terminal,
; preceeded by CR,LF,'LOAD'
; On Entry:
;    The string address is the "return address" on the stack.
;    The string is terminated by bit 7 set in its last chr.
;    The actual return address is the next address after the
;    last string character.
; On Exit:
;    Trashes a and flags, all other registers preserved.
;=============================================================
LOADPF:	
  call	PRINTF
  db	CR,LF,'LOAD',80h

;fall into PRINTF

;===Subroutine================================================
; Print inline string on the Terminal
; On Entry:
;    The string address is the "return address" on the stack.
;    The string is terminated by bit 7 set in its last chr.
;    The actual return address is the next address after the
;    last string character.
; On Exit:
;    Trashes a and flags, all other registers preserved.
;=============================================================
PRINTF:
  xthl			    ;get string address, save hl

PRNTLP:
  mov	a,m       ;get string character
  ani	7Fh		    ;strip end-of-string mark
  call	PRINTA	;and print it

  cmp	m		      ;end of string?
  inx	h		      ;point to next chr
  jz	PRNTLP	  ;No difference: keep going

  xthl			    ;restore hl, put return address
  ret			      ;..onto stack, and go there

;===Subroutine==================
; Print a on the Terminal
; On Entry:
;   a=chr to print
; On Exit:
;    all registers preserved.
;===============================
PRINTA:
  push	psw		;save chr to print

PALOOP:
  in	CONS		;Wait for TX to be ready
  ani	TBE
  jz	PALOOP

  pop	psw
  out	COND		;and send chr
  ret

  end
