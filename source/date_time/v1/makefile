include ../../../make.inc

libname=libdate_time

file := \
date_time.o

all:	$(file)
	(ar rc $(libname).a *.o ;)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
