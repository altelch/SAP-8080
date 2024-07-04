all: altmon.cim cpudiag.cim 8080EXM.hex Microcode

altmon.cim: ROM/altmon.asm
	cd ROM; make

cpudiag.cim: CPU-Test/cpudiag.asm
	cd CPU-Test; make

8080EXM.hex: CPU-Test/8080EXM.MAC
	cd CPU-Test; make

conv-win:
	sed -i 's^Microcode/^H:\\work\\SAP-8080\\Microcode\\^g' SAP-8080.dig
	sed -i 's^ROM/^H:\\work\\SAP-8080\\ROM\\^g' SAP-8080.dig

conv-lin:
	sed -i 's^H:\\work\\SAP-8080\\Microcode\\^Microcode/^g' SAP-8080.dig
	sed -i 's^H:\\work\\SAP-8080\\ROM\\^ROM/^g' SAP-8080.dig

clean:
	cd CPU-Test; make clean
	cd ROM; make clean
