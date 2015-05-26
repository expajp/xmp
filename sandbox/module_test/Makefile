#FC = gfortran
FC = xmpf90

FFLAGS =  -O2

PSRCS	= multifile.f90 initu.f90 initv.f90 initw.f90

POBJS   = $(PSRCS:.f90=.o)

PMOD = multimod.f90
PMODO = multimod.o
ALLOBJS = multi

# suffix rule
.SUFFIXES: .o .f90

# primary target
$(ALLOBJS): $(PMODO) $(POBJS)
	$(FC) $(FFLAGS) -o $@ $^

.f90.o:
	$(FC) $(FFLAGS) -c $<

$(PMODO): $(PMOD)
	$(FC) $(FFLAGS) -c $^

clean:
	rm -f core *.o *mod jobname.o* $(ALLOBJS) *~ #*#
