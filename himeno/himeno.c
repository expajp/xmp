/********************************************************************

 This benchmark test program is measuring a cpu performance
 of floating point operation by a Poisson equation solver.

 If you have any question, please ask me via email.
 written by Ryutaro HIMENO, November 26, 2001.
 Version 3.0
 ----------------------------------------------
 Ryutaro Himeno, Dr. of Eng.
 Head of Computer Information Division,
 RIKEN (The Institute of Pysical and Chemical Research)
 Email : himeno@postman.riken.go.jp
 ---------------------------------------------------------------
 You can adjust the size of this benchmark code to fit your target
 computer. In that case, please chose following sets of
 (mimax,mjmax,mkmax):
 small : 33,33,65
 small : 65,65,129
 midium: 129,129,257
 large : 257,257,513
 ext.large: 513,513,1025
 This program is to measure a computer performance in MFLOPS
 by using a kernel which appears in a linear solver of pressure
 Poisson eq. which appears in an incompressible Navier-Stokes solver.
 A point-Jacobi method is employed in this solver as this method can 
 be easyly vectrized and be parallelized.
 ------------------
 Finite-difference method, curvilinear coodinate system
 Vectorizable and parallelizable on each grid point
 No. of grid points : imax x jmax x kmax including boundaries
 ------------------
 A,B,C:coefficient matrix, wrk1: source term of Poisson equation
 wrk2 : working area, OMEGA : relaxation parameter
 BND:control variable for boundaries and objects ( = 0 or 1)
 P: pressure
********************************************************************/

#include <stdio.h>

#define SMALL

#ifdef SSMALL
#define MIMAX            32
#define MJMAX            32
#define MKMAX            64
#endif

#ifdef SMALL
#define MIMAX            64
#define MJMAX            64
#define MKMAX            128
#endif

#ifdef MIDDLE
#define MIMAX            128
#define MJMAX            128
#define MKMAX            256
#endif

#ifdef LARGE
#define MIMAX            256
#define MJMAX            256
#define MKMAX            512
#endif

#ifdef ELARGE
#define MIMAX            512
#define MJMAX            512
#define MKMAX            1024
#endif

#define N 2

float jacobi();
void initmt();
double fflop(int,int,int);
double mflops(int,double,double);

int xmp_node_num();
double xmp_wtime();

static float  p[MIMAX][MJMAX][MKMAX];
static float  a[4][MIMAX][MJMAX][MKMAX],
              b[3][MIMAX][MJMAX][MKMAX],
              c[3][MIMAX][MJMAX][MKMAX];
static float  bnd[MIMAX][MJMAX][MKMAX];
static float  wrk1[MIMAX][MJMAX][MKMAX],
              wrk2[MIMAX][MJMAX][MKMAX];

#pragma xmp nodes n(4,2)
#pragma xmp template t(0:256-1,0:256-1)
#pragma xmp distribute t(block,block) onto n
#pragma xmp align p[i][j][*] with t(i,j)
#pragma xmp align a[*][i][j][*] with t(i,j)
#pragma xmp align b[*][i][j][*] with t(i,j)
#pragma xmp align c[*][i][j][*] with t(i,j)
#pragma xmp align bnd[i][j][*] with t(i,j)
#pragma xmp align wrk1[i][j][*] with t(i,j)
#pragma xmp align wrk2[i][j][*] with t(i,j)
#pragma xmp shadow p[1:1][1:1][0]

static int imax, jmax, kmax;
static float omega;

int
main()
{
  int    i, j, k, nn;
  float  gosa;
  double cpu, cpu0, cpu1, flop, target;

  int    myrank = xmp_node_num() - 1;

  target = 60.0;
  omega = 0.8;
  imax = MIMAX;
  jmax = MJMAX;
  kmax = MKMAX;

  /*
   *    Initializing matrixes
   */
  initmt();

  if (myrank == 0) {
    printf("mimax = %d mjmax = %d mkmax = %d\n",MIMAX, MJMAX, MKMAX);
    printf("imax = %d jmax = %d kmax =%d\n",imax,jmax,kmax);
  }

  nn= 3;

  if (myrank == 0) {
    printf(" Start rehearsal measurement process.\n");
    printf(" Measure the performance in %d times.\n\n",nn);
  }

  cpu0= xmp_wtime();
  gosa= jacobi(nn);
  cpu1= xmp_wtime();
  cpu= cpu1 - cpu0;

  flop= fflop(imax,jmax,kmax);

  if (myrank == 0) {
    printf(" MFLOPS: %f time(s): %f %e\n\n", mflops(nn,cpu,flop),cpu,gosa);
  }

  nn= (int)(target/(cpu/3.0));
#pragma xmp reduction (max:nn)

  if (myrank == 0) {
    printf(" Now, start the actual measurement process.\n");
    printf(" The loop will be excuted in %d times\n",nn);
    printf(" This will take about one minute.\n");
    printf(" Wait for a while\n\n");
  }

  /*
   *    Start measuring
   */
  cpu0 = xmp_wtime();
  gosa = jacobi(nn);
  cpu1 = xmp_wtime();

  cpu= cpu1 - cpu0;
  
  if (myrank == 0) {
    printf(" Loop executed for %d times\n",nn);
    printf(" Gosa : %e \n",gosa);
    printf(" MFLOPS measured : %f\tcpu : %f\n",mflops(nn,cpu,flop),cpu);
    printf(" Score based on Pentium III 600MHz : %f\n",
	   mflops(nn,cpu,flop)/82.84);
  }
  
  return (0);
}

void
initmt()
{
	int i,j,k;

/*   for(i=0 ; i<MIMAX ; i++) */
/*     for(j=0 ; j<MJMAX ; j++) */
/*       for(k=0 ; k<MKMAX ; k++){ */
/*         a[0][i][j][k]=0.0; */
/*         a[1][i][j][k]=0.0; */
/*         a[2][i][j][k]=0.0; */
/*         a[3][i][j][k]=0.0; */
/*         b[0][i][j][k]=0.0; */
/*         b[1][i][j][k]=0.0; */
/*         b[2][i][j][k]=0.0; */
/*         c[0][i][j][k]=0.0; */
/*         c[1][i][j][k]=0.0; */
/*         c[2][i][j][k]=0.0; */
/*         p[i][j][k]=0.0; */
/*         wrk1[i][j][k]=0.0; */
/*         bnd[i][j][k]=0.0; */
/*       } */

#pragma xmp loop (i,j) on t(i,j)
  for(i=0 ; i<imax ; i++)
    for(j=0 ; j<jmax ; j++)
      for(k=0 ; k<kmax ; k++){
        a[0][i][j][k]=1.0;
        a[1][i][j][k]=1.0;
        a[2][i][j][k]=1.0;
        a[3][i][j][k]=1.0/6.0;
        b[0][i][j][k]=0.0;
        b[1][i][j][k]=0.0;
        b[2][i][j][k]=0.0;
        c[0][i][j][k]=1.0;
        c[1][i][j][k]=1.0;
        c[2][i][j][k]=1.0;
        p[i][j][k]=(float)(i*i)/(float)((imax-1)*(imax-1));
        wrk1[i][j][k]=0.0;
        bnd[i][j][k]=1.0;
      }
}

float
jacobi(int nn)
{
  int i,j,k,n;
  float gosa, s0, ss;

  for(n=0 ; n<nn ; ++n){
    gosa = 0.0;

#pragma xmp reflect (p)

#pragma xmp loop (i,j) on t(i,j) reduction(+:gosa)
    for(i=1 ; i<imax-1 ; i++)
      for(j=1 ; j<jmax-1 ; j++)
        for(k=1 ; k<kmax-1 ; k++){
          s0 = a[0][i][j][k] * p[i+1][j  ][k  ]
             + a[1][i][j][k] * p[i  ][j+1][k  ]
             + a[2][i][j][k] * p[i  ][j  ][k+1]
             + b[0][i][j][k] * ( p[i+1][j+1][k  ] - p[i+1][j-1][k  ]
                              - p[i-1][j+1][k  ] + p[i-1][j-1][k  ] )
             + b[1][i][j][k] * ( p[i  ][j+1][k+1] - p[i  ][j-1][k+1]
                               - p[i  ][j+1][k-1] + p[i  ][j-1][k-1] )
             + b[2][i][j][k] * ( p[i+1][j  ][k+1] - p[i-1][j  ][k+1]
                               - p[i+1][j  ][k-1] + p[i-1][j  ][k-1] )
             + c[0][i][j][k] * p[i-1][j  ][k  ]
             + c[1][i][j][k] * p[i  ][j-1][k  ]
             + c[2][i][j][k] * p[i  ][j  ][k-1]
             + wrk1[i][j][k];

          ss = ( s0 * a[3][i][j][k] - p[i][j][k] ) * bnd[i][j][k];

          gosa+= ss*ss;
          /* gosa= (gosa > ss*ss) ? a : b; */

          wrk2[i][j][k] = p[i][j][k] + omega * ss;
        }

#pragma xmp loop (i,j) on t(i,j)
    for(i=1 ; i<imax-1 ; ++i)
      for(j=1 ; j<jmax-1 ; ++j)
        for(k=1 ; k<kmax-1 ; ++k)
          p[i][j][k] = wrk2[i][j][k];
    
  } /* end n loop */

  return(gosa);
}

double
fflop(int mx,int my, int mz)
{
  return((double)(mz-2)*(double)(my-2)*(double)(mx-2)*34.0);
}

double
mflops(int nn,double cpu,double flop)
{
  return(flop/cpu*1.e-6*(double)nn);
}
