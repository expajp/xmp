      program  varmem
      
      character  swcalc*2


      call  getenv( 'LCALC', swcalc )


      if( swcalc .eq. ' ')  then

         norvar  =  2*l1*m2*n2 + 2*l2*m1*n2 + 2*l2*m2*n1 + l2*m2*n2 &
            + 4*l1*m1*n1 + 16*lmn + 5*lmn2 + lm2*n2 + 35
         noivar  =  lmn + nls + 9
   
         mem     =  8*norvar + 4*noivar 
   
         write(6,6000)  norvar, noivar, mem
 6000    format(//5x,'==================='/ &
            5x,'  Current Setting'/ &
            5x,'==================='// &
            5x,'Number of real variables              : ',i12/ &
            5x,'Number of integer variables           : ',i12// &
            5x,'Approximate memory size (in real*8)   : ',i12, &
               ' (bytes)'//)
   
         call  datain

      else

         write(6,6200) 
 6200    format(//'Please enter lattice size (l,m,n) ', &
            'to be simulated ...')
         read(5,*) i, j, k

 10      continue
   
           i1   =  i + 1
           j1   =  j + 1
           k1   =  k + 1
           i2   =  i + 2
           j2   =  j + 2
           k2   =  k + 2
           ijk  =  i*j*k
           ijk2 =  ijk + 2*i*j
           ij2  =  i*j + 2*i
           nlsz =  i + j + k + 2
     
           norvar  =  2*i1*j2*k2 + 2*i2*j1*k2 + 2*i2*j2*k1 + i2*j2*k2 &
              + 4*i1*j1*k1 + 16*ijk + 5*ijk2 + ij2*k2 + 35
           noivar  =  ijk + nlsz + 9
     
           mem     =  8*norvar + 4*noivar 
     
           write(6,6100)  norvar, noivar, mem
 6100      format(//5x,'==================='/ &
              5x,'    New Setting'/ &
              5x,'==================='// &
              5x,'Number of real variables              : ',i12/ &
              5x,'Number of integer variables           : ',i12// &
              5x,'Approximate memory size (in real*8)   : ',i12, &
                 ' (bytes)'//)
   
           call  cldata( i, j, k )
  
           write(6,6200) 
           read(5,*) i, j, k
  
         if( i .gt. 0 )  go to 10

      endif


      stop
      end
