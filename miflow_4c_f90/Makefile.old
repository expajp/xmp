FC = gfortran
FFLAGS =  -O2
#FFLAGS =  -g

PSRCS	= miflow.f90 clearv.f90 datain.f90 initvr.f90 mkcoef.f90 \
	  setbnd.f90 calcu1.f90 calcv1.f90 calcw1.f90 caluvw.f90 \
	  setbcv.f90 lsor4c.f90

MSRC = cmmod.f90

miflow: $(PSRCS)
	$(FC) $(FFLAGS) -o $@ $(MSRC) $(PSRCS)

clean:
	rm -f miflow *.o *.mod $(ALLOBJS) *~ #*#
