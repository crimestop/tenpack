include ../../make.inc

libname=libtracer

file := \
tracer.o 

all:	run.out

run.out:$(file)
	$(FC) $(FFLAGS) -o $@ $^ 

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
