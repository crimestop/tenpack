include ../../../make.inc

libname=libcumu

file := \
cumulator.o

all:	$(file)
	(ar rc $(libname).a *.o ;)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
