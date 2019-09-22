include ../../make.inc

file := \
error.o

all:	$(file)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
