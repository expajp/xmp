FC = gfortran
FFLAGS =  -O -fopenmp
#FFLAGS =  -g

PSRCS	= miflow.f clearv.f datain.f initvr.f mkcoef.f \
	  setbnd.f calcu1.f calcv1.f calcw1.f caluvw.f \
	  setbcv.f linsor.f linpcg.f ilvpcg.f lpcghp.f \
	  lsorrb.f lsor4c.f

POBJS	= ${PSRCS:.f=.o}
MOBJS	= ${MSRCS:.f=.o}
ALLOBJS = miflow

all:	$(ALLOBJS)

miflow: $(POBJS)
	$(FC) -o $@ $(POBJS)

clean:
	rm -f core *.o $(ALLOBJS) *~ #*#

test:
	env
