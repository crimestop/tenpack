include ../../../make.inc

tn := \
QuantumNumber.o\
SymDimension.o\
U1.o\
Parity.o\
SymTensor.o\
fTensor.o

all:	$(tn)
	(ar rc libsymtnsp.a *.o ;)

%.o:	source/%.f90
	$(FC) $(FFLAGS) -o $@ -c $(<)
clean:
	rm -rf *.o *.mod *.a
