CC=gcc -m32 -c

all: clean unicode

unicode: bsearch.o WCsubst.o

clean: 
	rm -f *.o

bsearch.o:
	$(CC) $(CFLAGS) -c bsearch.c

WCsubst.o:
	$(CC) $(CFLAGS) -c WCsubst.c
