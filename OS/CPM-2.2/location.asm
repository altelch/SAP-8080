;
msize	equ	64;cp/m version memory size in kilobytes
;
;	"bias" is address offset from 3400H for memory systems
;	less than 16K (referred to as "b" throughout the text).
;

bias      equ (msize-20)*1024; (64-20) * 1024 = 0B000h
addr_offs equ 3400H
bdos_offs equ 0800H
bios_offs equ 1600H

CCP_ORG   equ   addr_offs+bias      ;base of ccp
BDOS_ORG  equ   CCP_ORG+bdos_offs   ;base of bdos
BIOS_ORG  equ   CCP_ORG+bios_offs   ;base of bios
