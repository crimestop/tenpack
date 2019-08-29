FC=mpif90 

UNITEST := \
stack.o\
test_stack.o

FFLAGS := -I ../../include
LINKOPT := -L ../../lib -lerror -lfunc

run.out:	$(UNITEST)
	$(FC) $(FFLAGS) -o $@ $^ $(LINKOPT) 
%.o :	%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<) -O3
%.o :	../source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<) -O3
clean:
	rm *.o *.mod run.out 
