VPATH=utilfort:spmat
.SUFFIXES:
.SUFFIXES: .f90 .o
.PHONY: clean
FC=gfortran
FCFLAG=

OBJECT =\
	bitset.o\
	array.o\
	matcrs.o\
	part.o

TARGET = part.exe

$(TARGET): $(OBJECT)
	$(FC) -o $@ $(OBJECT) $(FCFLAG)

.f.o:
	$(FC) -c $< $(FCFLAG)

.f90.o:
	$(FC) -c $< $(FCFLAG)

clean:
	rm -f *.exe *.o *.mod *~
