include ../../make.inc

file := \
unidic.o

all:	$(file)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
