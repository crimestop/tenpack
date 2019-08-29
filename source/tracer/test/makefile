include ../make.inc

libname=libtracer

file := \
tracer.o\
main.o

run.out: $(file)
	$(FC) $(FFLAGS) -o $@ $^ $(LINKOPT)
%.o:	../source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
%.o:	%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
lib: 
	(cp *.mod ../include ;)
	(ar rc $(libname).a *.o ;)
	(mv $(libname).a ../lib ;)
