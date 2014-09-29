#FC = gfortran
FC = xmpf90
#FC = mpifrtpx

FFLAGS =  -O2

PSRCS	= miflow.f90 clearv.f90 datain.f90 initvr.f90\
	  mkcoef.f90 setbnd.f90 calcu1.f90 calcv1.f90 calcw1.f90\
	  caluvw.f90 setbcv.f90 lsor4c.f90

POBJS   = $(PSRCS:.f90=.o)

PMOD = cmmod.f90
PMODO = cmmod.o
ALLOBJS = miflow

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
	rm -f core *.o *.mod jobname.o* core-F_Front.* $(ALLOBJS) *~ #*#
