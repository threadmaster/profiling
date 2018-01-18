#include "fpapi.h"

      program fdriver 

* F Driver Program to demonstrate lbstime.a library for timing C and Fortan Codes
*
* Andrew J. Pounds, Ph.D.
* Departments of Chemistry and Computer Science
* Mercer University
* Spring 2017


      parameter (MAXSIZE=1000)
      real A(MAXSIZE,MAXSIZE)
      real B(MAXSIZE,MAXSIZE)
      real C(MAXSIZE,MAXSIZE)
      real sum
      real total_ops

* Now define the papi variables
      real real_time, proc_time, mflops
      integer*8 flpops
      integer check

      double precision  wall, cpu 
* Need to define walltime and cputime as double precision because that
* is what is returned by both functions  
      double precision walltime, cputime

* Similarly need to define them as external functions
      external walltime, cputime

      do 10 i = 1, MAXSIZE
         do 20 j = 1, MAXSIZE
            if (i.ne.j) then 
              A(j,i)= 0.0 
              B(j,i)= 1.0 
            else 
              A(j,i)= 1.0 
              B(j,i)= 1.0 
            endif
20       continue
10    continue

      idim = MAXSIZE 

* Call then once to get time since Epoch
      wall = walltime()
      cpu  = cputime()
    
      call PAPIF_library_init( check ) 
      call PAPIF_flips( real_time, proc_time, flpops, mflops, check)

      CALL mmult(idim,A,B,C)

      call PAPIF_flips( real_time, proc_time, flpops, mflops, check)

* Call them again to get second time and take difference
      wall = walltime() - wall 
      cpu  = cputime() - cpu

      SUM = 0.0
      do 30 i = 1, MAXSIZE
        SUM = SUM + C(i,i)
30    continue

      print *, idim, ' ', sum, ' ', wall, ' ', cpu 

      totalops = 2*dble(MAXSIZE)**3 
      print *, "Estimated megaflops = ", 
     +              (totalops/cpu)/1000000
      print *, "PAPI megaflops = ", mflops

      end 

     
      subroutine mmult (N, A, B, C)
* Simple matrix routine
      real A(N,*), B(N,*), C(N,*)
      real dot

      do 10 i = 1, N
         do 20 j = 1, N
           dot = 0.0
           do 30 k = 1, N
              dot = dot + A(k,j) * B(i,k)
30         continue
           C(j,i) = dot
20       continue
10     continue

       end
 
 
