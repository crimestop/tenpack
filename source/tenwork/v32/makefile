include ../../../make.inc

libname=libtenwork

file := \
lattice.o \
tn_tensor.o \
lattice_clt.o

all: 	$(file)
	(ar rc $(libname).a *.o ;)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
