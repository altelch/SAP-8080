.DEFAULT_GOAL := all

AS    = ../Tools/zmac/zmac
ASOPT = -8 --oo hex,cim --od .

all: altmon

altmon: altmon.asm
	$(AS) $(ASOPT) altmon.asm

clean:
	rm -f *.cim *.hex
