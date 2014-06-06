FC = xmpf90
FFLAGS =  -O2
#FFLAGS =  -g

PSRCS	= cmmod.f90 miflow.f90 clearv.f90 datain.f90 initvr.f90 \
	  mkcoef.f90 setbnd.f90 calcu1.f90 calcv1.f90 calcw1.f90 \
	  caluvw.f90 setbcv.f90 lsor4c.f90

POBJS   = $(PSRCS:.f90=.o)

ALLOBJS = miflow

miflow: $(POBJS)
	$(FC) $(FFLAGS) -o $@ $(POBJS)

$(POBJS): $(PSRCS)
	$(FC) $(FFLAGS) -c $(PSRCS)

clean:
	rm -f core *.o *mod jobname.o* $(ALLOBJS) *~ #*#
