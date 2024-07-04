;---------------------------------------------------------------------
;
; Sieve of Eratosthenes benchmark test published in the September 1981
;   issue of BYTE magazine. This version takes advantage of assembly
;   language optimzations a programmer might make as opposed to trying
;   to write the code a C-compiler might have generated.
;
;   This test runs in 7.6 seconds on an Altair 8800, 2 MHz
;
;   Mike Douglas, August 2023
;
;---------------------------------------------------------------------
;
; Byte Sieve in C
;
; #define true 1
; #define false 0
; #define size 8190
; #define sizepl 8191
; char flags[sizepl];
;
; main() {
;     int i, prime, k, count, iter;
;     printf("10 iterations\n");
;     for (iter = 0; iter < 10; iter++) {
;         count=0;
;         for (i = 0; i <= size; i++)
;             flags[i] = true;
;         for (i = 0; i <= size; i++) {
;             if (flags[i]) {
;                 prime = i + i + 3;
;                 k = i + prime;
;                 while (k <= size) {
;                     flags[k] = false;
;                     k += prime;
;                 }
;                 count = count + 1;
;             }
;         }
;     }
;     printf("\n%d primes", count);
; }
;
;-----------------------------------------------------------------------
  .8080

ITERS	equ	10			;repeat test 10 times

; This program REQUIRES the flags table to be at address zero

	org	0
flags	ds	8192
ENDMSB	equ	$/256			;MSB of table end address

; Program entry at 2000h

start	lxi	sp,STACK		;init stack pointer
	lxi	h,mIters		;display "10 iterations"
	call	dspMsg
	mvi	a,ITERS			;init iteration counter
	sta	iterCnt

; Outer iteration loop

iterLp	lxi	h,iterCnt		;count iterations of the test
	dcr	m
	jm	exit			;all done

	lxi	b,0			;set prime count to zero

; Init all flags to true

init	lxi	h,flags			;HL->flags table
	mvi	d,1			;non-zero = TRUE
	mvi	a,ENDMSB		;for end address comparison

iniLoop	mov	m,d			;set all flags true
	inx	h
	cmp	h			;MSB past end?
	jnz	iniLoop			;no

; for (i = 0; i <= size; i++) {
;   if (flags[i]) {

	lxi	h,flags-1		;HL->flags table		

tblLoop	inx	h			;move to next table entry
	mvi	a,ENDMSB		;reach the end?
	cmp	h			;done?
	jz	iterLp			;yes, do another iteration

	mov	a,m			;flag TRUE?
	ora	a
	jz	tblLoop			;no

; prime = i + i + 3;

	mov	d,h			;DE=i
	mov	e,l
	
	dad	h			;prime=i+i+3
	inx	h
	inx	h
	inx	h
	xchg				;DE=prime, HL=i
	
; k = i + prime

	push	h			;preserve i 
	dad	d			;HL=i+prime

; while (k <= size) {
;   flags[k] = false;
;   k += prime;

	mvi	a,ENDMSB-1		;for end address comparison

clrLoop	cmp	h			;past end of table?
	jc	cntPrim			;yes, count the prime

	mvi	m,0			;flag[k]=FALSE
	dad	d			;k=k+prime
	jmp	clrLoop

; count = count + 1;

cntPrim	inx	b			;count maintained in b
	pop	h			;restore HL with i
	jmp	tblLoop

; Display result and exit

exit	lxi	h,mPrimes		;display result and exit
	call	dspMsg

	call	dspByte			;display MSB of count (b)
	mov	b,c			;display LSB of count (c)
	call	dspByte
die	jmp	die

; dspByte - Display byte as two ASCII hex digits

dspByte	mov	a,b			;do MSN
	rrc
	rrc
	rrc
	rrc
	call	dspNib

	mov	a,b			;do LSN

dspNib	ani	0Fh
	adi	'0'
	cpi	3Ah
	jc	dspChar
	adi	7			;fall into dspChar

; dspChar - display character in A. Clobbers B

dspChar	push	psw			;save char to display

dcWait	in	10h
	ani	02h
	jz	dcWait

	pop	psw
	out	11h
	ret

; dspMsg - Display null terminated string from HL

dspMsg	mov	a,m			;get next character
	ora	a			;null?
	rz				;yes, exit
	call	dspChar	
	inx	h		
	jmp	dspMsg

;-----------------------------------------------------------
; Data area
;-----------------------------------------------------------
mIters	db	13,10,'10 Iterations',13,10,10,0

mPrimes	db	'Primes found (in hex): ',0

iterCnt	ds	1
	ds	64
STACK	equ	$

	end

