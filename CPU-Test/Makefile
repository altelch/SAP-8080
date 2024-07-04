AS    = ../Tools/zmac/zmac
ASOPT = -8 #--oo hex --od .

all: cpudiag.cim sieve.cim 8080EXM.hex

cpudiag.cim: cpudiag.asm
	$(AS) $(ASOPT) cpudiag.asm -o cpudiag.hex

sieve.cim: sieve.asm
	$(AS) $(ASOPT) sieve.asm -o sieve.hex

8080EXM.hex: 8080EXM.MAC
	$(AS) -8 8080EXM.MAC -o 8080EXM.HEX

clean:
	rm -f *.HEX *.cim
	rm -rf zout
