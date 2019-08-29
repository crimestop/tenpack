include ../../make.inc

tn := \
Tools.o     \
Dimension.o      \
print_in_TData.o \
element_in_TData.o \
modify_in_TData.o \
permutation_in_TData.o  \
TData.o      \
Tensor.o      \
parameter.o	\
OtherFunction.o

all:	$(tn)
	(ar rc libtnsp.a *.o ;)
	(ar rc libtools.a Tools.o ;)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
