include ../../make.inc

libname=libtracer

file := \
tracer.o \
main.o

all:	run.out
	(ar rc $(libname).a *.o ;)

run.out:$(file)
	$(FC) $(FFLAGS) -o $@ $^ 

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
