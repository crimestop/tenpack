include ../../../make.inc

libname=libtime

file := \
time.o

all:	$(file)
	(ar rc $(libname).a *.o ;)

%.o:	source/%.c
	$(CC) $(CFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
