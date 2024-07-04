AS    = ../../Tools/zmac/zmac
ASOPT = -8 --oo cim --od .
GENFS = ../../Tools/cpmfs-2.00/cpmfs
DISK  = SSD1
COM   = ../Source/CPM-2.2
IMG   = disk0.img

all: loader.cim bios.cim bdos.cim ccp.cim disk0.img

bios.cim: bios.asm
	$(AS) $(ASOPT) bios.asm

bdos.cim: bdos.asm
	$(AS) $(ASOPT) bdos.asm

ccp.cim: ccp.asm
	$(AS) $(ASOPT) ccp.asm

loader.cim: loader.asm
	$(AS) $(ASOPT) loader.asm

$(IMG): loader.cim bios.cim bdos.cim ccp.cim
	rm -f $(IMG)
	$(GENFS) -t $(DISK) $(IMG) init
	$(GENFS) -t $(DISK) $(IMG) ws 0 2   loader.cim
	$(GENFS) -t $(DISK) $(IMG) ws 2 16  ccp.cim
	$(GENFS) -t $(DISK) $(IMG) ws 18 28 bdos.cim
	$(GENFS) -t $(DISK) $(IMG) ws 46 10 bios.cim
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/STAT.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/DUMP.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/ED.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/LS.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/MBASIC.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/PIP.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/SUBMIT.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/TST8080.COM
	$(GENFS) -t $(DISK) $(IMG) w $(COM)/XSUB.COM

clean:
	rm -f *.cim *.img
