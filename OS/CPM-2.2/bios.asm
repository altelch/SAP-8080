;	Skeletal CBIOS for first level of CP/M 2.0 alteration
;
include "location.asm"
;
;	"bias" is address offset from 3400H for memory systems
;	less than 16K (referred to as "b" throughout the text).
;
;number of disk drives in system
; 
NDISK equ  4           ;Number of Drives

; 88-2SIO equates

CONS  equ  10h         ;console status port
COND  equ  11h         ;console data port
TBE   equ  2           ;transmit buffer entry
RDA   equ  1           ;receive data available

ccp	  equ	CCP_ORG
bdos	equ	BDOS_ORG
bios	equ	BIOS_ORG

; CPM page zero equates
wbootv equ	00h		;warm boot vector location
ioByte equ	03h		;CPM IOBYTE
cdisk  equ	04h		;CPM current disk
bdosv  equ	05h		;bdos entry vector location
defDma equ	80h		;default dma address

; misc equates

cr	equ	13		;ascii for carriage return
lf	equ	10		;ascii for line feed
JMP equ 0c3h  ;jmp opcode
SEC equ 128   ;sector size

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
SETSECT  equ  0BH       ;Write LBA Reg. 0
SETTRK1  equ  0CH       ;Write LBA Reg. 1
SETTRK2  equ  0DH       ;Write LBA Reg. 2
SETDRV   equ  0EH       ;Write LBA Reg. 3 (Drive)
IDECOM   equ  0FH       ;Read: Status, Write: Issue Command

IDEREAD  equ  020H      ;Read sectors with retry
IDEWRITE equ  030H      ;Write sectors with retry
IDEDRVID equ  0ECH      ;Identify drive

DRVRDY   equ  01000000B ;Ready for command
DRVBUSY  equ  10000000B ;Busy
DRVDATA  equ  00001000B ;Data Ready
;
  org bios    ;origin of this program
;
nsects   equ  ($-ccp)/SEC ;warm start sector count
;
;	jump vector for individual subroutines
  jmp	boot		;cold start
wboote:
  jmp	wboot		;warm start
  jmp	const		;console status
  jmp	conin		;console character in
  jmp	conout	;console character out
  jmp	list		;list character out
  jmp	punch		;punch character out
  jmp	reader	;reader character out
  jmp	home		;move head to home position
  jmp	seldsk	;select disk
  jmp	settrk	;set track number
  jmp	setsec	;set sector number
  jmp	setdma	;set dma address
  jmp	read		;read disk
  jmp	write		;write disk
  jmp	listst	;return list status
  jmp	sectran	;sector translate
;
;       parameter-list-i takes the form
;               dn,fsc,lsc,[skf],bls,dks,dir,cks,ofs,[0]
;       where
;       dn      is the disk number 0,1,...,n-1
;       fsc     is the first sector number (usually 0 or 1)
;       lsc     is the last sector number on a track
;       skf     is optional "skew factor" for sector translate
;       bls     is the data block size (1024,2048,...,16384)
;       dks     is the disk size in bls increments (word)
;       dir     is the number of directory elements (word)
;       cks     is the number of dir elements to checksum
;       ofs     is the number of tracks to skip (word)
;       [0]     is an optional 0 which forces 16K/directory entry
;
  maclib  diskdef ;load the disk definition library
  disks   NDISK   ;four disks
  diskdef 0,0,255,0,4096,256,256,0,1
  diskdef 1,0,255,0,4096,256,256,0,0
  diskdef 2,1
  diskdef 3,1
;
;  CPM Welcome Message

cpmStr:
	db	cr, 'CP/M 2.2b v1.0 for HDSK', cr, 0
	ds	64-($-cpmStr)	;adjust to make 64 byte stack
lclStk	equ	$		;local stack
;
;	individual subroutines to perform each function
boot:	;simplest case is to just perform parameter initialization
  xra	a            ;zero in the accum
  sta	iobyte       ;clear the iobyte
  sta	cdisk        ;select disk zero
  lxi h,cpmStr     ;hl = address of cpm welcome message

welOut:
  mov	c,m
  push	h
  call	conout
  pop	h
  inx	h
  mov	a,m
  ora	a
  jnz	welOut		;string ends with terminating null
  jmp	gocpm    	;initialize and go to cp/m
;
wboot:
  lxi	sp,defDma	;use space below buffer for stack
  mvi	c,0		    ;select disk 0
  call	seldsk
  call	home		;go to track 00
	mvi	b,nsects	;b counts # of sectors to load
	mvi	d,2		;d has the next sector to read
;	note that we begin by reading track 0, sector 2 since sector 0/1
;	contains the cold start loader, which is skipped in a warm start
	lxi	h,ccp		;base of cp/m (initial load point)
load1:	;load one more sector
	push	b	;save sector count, current track b/c
	push	d	;save next sector to read d/e
	push	h	;save dma address h/l
	mov	c,d	;get sector address to register c
	call	setsec	;set sector address from register c
	pop	b	;recall dma address to b,c
	push	b	;replace on stack for later recall
	call	setdma	;set dma address from b,c
;
;	drive set to 0, track set, sector set, dma address set
	call	read
	cpi	00h	;any errors?
	jnz	wboot	;retry the entire boot if an error occurs
;
;	no error, move to next sector
	pop	h	;recall dma address
	lxi	d,128	;dma=dma+128
	dad	d	;new dma address is in h,l
	pop	d	;recall sector address
	pop	b	;recall number of sectors remaining, and current trk
	inr d ;next sector
	dcr	b	;sectors=sectors-1
	jnz	load1	;next sector
;
;	end of load operation, set parameters and go to cp/m
gocpm:
  lxi	b,defDma   ;default dma address is 80h
  call	setdma
  mvi	a,JMP      ;c3 is a jmp instruction
  sta	wbootv     ;store in 1st byte of warm boot vecctor
  sta	bdosv      ;and 1st byte of bdos entry vecctor
  lxi	h,wboote   ;get the warm boot address
  shld	wbootv+1 ;and put it after the jump
  lxi	h,bdos+6   ;BDOS entry address
  shld	bdosv+1  ;put it after the jump opcode
  lda	cdisk      ;get current disk number
  mov	c,a        ;pass it to CPM in register c
  jmp	ccp        ;enter ccp
;
;
;	simple i/o handlers (must be filled in by user)
;	in each case, the entry point is provided, with space reserved
;	to insert your own code
;

const:	         ;console status, return 0ffh if character ready, 00h if not
  in  CONS       ;read keyboard status
  rrc            ;data available flag in carry
  jnc no_char
	mvi	a,0ffh     ;char ready
	ret
no_char:
	mvi	a,00h      ;no char
	ret
;
conin:	         ;console character into register a
  in  CONS       ;read keyboard status
  rrc            ;data available flag in carry
  jnc conin      ;loop until char ready
  in  COND       ;read from keyboard
  ani	7fh	       ;strip parity bit
  ret
;
conout: ;console character output from register c
  in  CONS       ;read keyboard status
  ani TBE        ;Ready
  jz  conout     ;Loop until port ready
  mov	a,c	       ;get the char
  out COND 
  ret
;
list:	;list character from register c
	mov	a,c	;character to register a
	ret		;null subroutine
;
listst:	;return list status (0 if not ready, 1 if ready)
	xra	a	;0 is always ok to return
	ret
;
punch:	;punch character from register c
	mov	a,c	;character to register a
	ret		;null subroutine
;
;
reader: ;read character into register a from reader device
	mvi	a,1ah	;enter end of file for now (replace later)
	ani	7fh	;remember to strip parity bit
	ret
;
;
;	i/o drivers for the disk follow
;	for now, we will simply store the parameters away for use
;	in the read and write subroutines
;
home:	;move to the track 00 position of current drive
;	translate this call into a settrk call with parameter 00
	mvi	c,0	;select track 0
	call	settrk
	ret		;we will move to 00 on first read/write
;
seldsk:	;select disk given by register C
	lxi h,0000h     ;return error code in HL
  mov	a,c         ;move drive to a
  sta	diskno      ;store diskno
  cpi	NDISK       ;must be between 0 and NDISK
  jnc seldskreset ;no carry if 4,5,...
;	disk number is in the proper range
  rlc             ;rotate 4 times left to
  rlc             ;get diskno into upper 4
  rlc             ;bits
  rlc
  out SETDRV      ;set disk number
;	compute proper disk parameter header address
  lda	diskno
  mov	l,a         ;L=disk number 0,1,2,3
  mvi	h,0         ;high order zero
  dad	h           ;*2
  dad	h           ;*4
  dad	h           ;*8
  dad	h           ;*16 (size of each header)
  lxi	d,dpbase
  dad	d           ;HL=.dpbase(diskno*16)
  ret
;
seldskreset:  ;reset to drive 0 after invalid drive
  xra a       ;reset default disk back to zero
	sta cdisk   ;store current disk
	ret
;
settrk:	;set track given by register c
  mov	a,c
  sta	track
  out SETTRK2  ;..to LBA Reg 2
  ret
;
setsec:	;set sector given by register c
  mov	a,c
  sta	sector
  out SETTRK1  ;..to LBA Reg 1
  ret
;
sectran:
  ;translate the sector given by BC using the
  ;translate table given by DE
  mov a,d ;do we have a translation table?
  ora e
  jnz sectran1 ;yes, translate
  mov l,c ;no, return untranslated
  mov h,b
	;inx h
	ret
sectran1:
  xchg		;HL=.trans
  dad	b	  ;HL=.trans(sector)
  mov	l,m	;L = trans(sector)
  mvi	h,0	;HL= trans(sector)
  ret		  ;with value in HL
;
setdma:	;set dma address given by registers b and c
  mov	h,b	    ;high order address
  mov	l,c	    ;low order address
  shld	dmaad	;save the address
  ret
;
read:	;perform read operation
  lda sector
  out SETSECT    ;..to LBA Reg 0

  lda track
  out SETTRK1    ;..to LBA Reg 1

  xra a          ;a=0
  out SETTRK2    ;LBA Reg 2 always Zero (no real IDE)

  mvi a,IDEREAD  ;Set Read Command (Fake)
  out IDECOM     ;.. to command Reg.

  lhld dmaad     ;get the address
  mvi  b,SEC     ;sector size to b
	mvi  c,0       ;start value for byte counter

readloop:
  in  IDEDATA    ;get Data
  mov m,a        ;store in Memory
  inx h          ;increment H/L
	inr c          ;increment byte counter
  mov a,c        ;counter -> a
  cmp b          ;compare with sector size
  jnz readloop   ;no, read next byte
  mvi	a,0        ;error condition
  ret

;
write:	  ;perform a write operation
  lda sector
  out SETSECT	   ;set sector

  lda track
  out SETTRK1    ;set track lsb

  xra a          ;a=0
  out SETTRK2    ;set track msb

  mvi a,IDEWRITE ;Set Write Command (Fake) / clear counter
  out IDECOM     ;.. to command Reg.

  lhld dmaad     ;get the address
  mvi  b,SEC     ;sector size to b
	mvi  c,0       ;start value for byte counter
writeloop:
  mov a,m        ;store in Memory
  out IDEDATA    ;write Data
  inx h          ;increment H/L
	inr c          ;increment byte counter
  mov a,c        ;counter -> a
  cmp b          ;compare with sector size
  jnz writeloop  ;no, write next byte
  mvi	a,0      ;error condition
  ret	         ;replaced when filled-in
;
;
track:	ds	2	;two bytes for expansion
sector:	ds	2	;two bytes for expansion
dmaad:	ds	2	;direct memory address
diskno:	ds	1	;disk number 0-15
;
;	scratch ram area for BDOS use
begdat	equ	$	;beginning of data area
dirbuf: ds	128	;scratch directory area
alv0:   ds	31	;allocation vector 0
alv1:   ds	31	;allocation vector 1
alv2:   ds	31	;allocation vector 2
alv3:   ds	31	;allocation vector 3
csv0:   ds	16	;check vector 0
csv1:   ds	16	;check vector 1
csv2:   ds	16	;check vector 2
csv3:   ds	16	;check vector 3
;
enddat	equ	$	;end of data area
datsiz	equ	$-begdat;size of data area

	end
