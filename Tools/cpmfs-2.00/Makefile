all: cpmfs

cpmfs: cpmfs.c  misc.c  os_linux.c
	gcc -o cpmfs cpmfs.c misc.c os_linux.c

clean:
	rm -f cpmfs *.o core
