FC = gfortran
FFLAGS =  -O
#FFLAGS =  -g

PSRCS	= miflow.f clearv.f datain.f initvr.f mkcoef.f \
	  setbnd.f calcu1.f calcv1.f calcw1.f caluvw.f \
	  setbcv.f lsor4c.f

POBJS	= ${PSRCS:.f=.o}

miflow: $(POBJS)
	$(FC) -o $@ $(POBJS)

clean:
	rm -f core *.o $(ALLOBJS) *~ #*#
