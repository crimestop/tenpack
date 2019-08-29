FC=mpif90 

UNITEST := \
test.o

FFLAGS :=  -I ~/Desktop/program/pack/include
PMKL:= -limf -lmkl_intel_lp64 -lmkl_intel_thread -lmkl_core -liomp5 -lpthread
PLAPACK:= -llapack -lblas
LINKOPT :=  -L ~/Desktop/program/pack/lib -lmpi_info -lstring -ldate_time -lerror -ltools $(PLAPACK)

run.out:	$(UNITEST)
	$(FC) $(FFLAGS) -o $@ $^ $(LINKOPT) 
%.o :	%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<) -O3
clean:
	rm *.o *.mod run.out 
