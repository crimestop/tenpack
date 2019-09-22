include ../../make.inc

file := \
time.o

all:	$(file)

%.o:	source/%.c
	$(CC) $(CFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
