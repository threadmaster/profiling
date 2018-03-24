// Fortran Driver Program to demonstrate low level PAPI calls 
//
// Andrew J. Pounds, Ph.D.
// Departments of Chemistry and Computer Science
// Mercer University
// Spring 2018 

#include "fpapi.h"

 program llfdriver
 
    integer :: i, j, k, mdim 
    integer, parameter :: MAXSIZE = 1000
    integer, parameter :: NUM_EVENTS = 2 
    real (KIND=4), dimension(MAXSIZE,MAXSIZE) ::  A, B, C;
    real (kind=4) :: sum 
    real (kind=8) :: wall, cpu;

    ! PAPI Variables
    C_FLOAT :: rtime, ptime, mflops
    C_LONG_LONG ::  flpops
    C_INT :: check, eventSet
    C_LONG_LONG, dimension(NUM_EVENTS) :: sp_ops

    mdim = MAXSIZE

    do i=1,mdim
      do j = 1, mdim 
         if ( i .ne. j) then
            A(j,i) = 0.0
            B(j,i) = 1.0
         else
            A(j,i) = 1.0
            B(j,i) = 1.0
         endif
      enddo
    enddo


     ! Initialize the PAPI library
     PAPIF_library_init(check);
     if (check .ne. PAPI_VER_CURRENT .and. retval .gt. 0) then 
         print *, "PAPI library version mismatch!"
         exit() 
     endif

     if (check .lt. 0) then 
         print *, "PAPI initialization error."
         exit()
     endif

     PAPIF_is_initialized(check);
     if (check .ne.  PAPI_LOW_LEVEL_INITED) {
         print *, "PAPI low level initialization failed."
         exit()
     }
    
     ! Create a PAPI Event Set 
     eventSet = PAPI_NULL; 

     PAPIF_create_eventset(eventSet,check)
     if (check .ne. PAPI_OK ) then 
         print *, "Could not create PAPI event set."
         exit()
     endif 

     ! Add the particular events to be counted to each event set
     PAPIF_add_event(eventSet, PAPI_SP_OPS, check)
     if (check .ne. PAPI_OK ) then 
         print *, "Could not create PAPI_SP_OPS event."
         exit()
     endif 

     ! Start the counters in each event set 
     PAPIF_start(eventSet,check)
     if ( check .ne. PAPI_OK ) then 
         printf("Could not start PAPI_SP_OPS counter.\n");
         exit()
     endif 

     ! Note: call lbstime walltime_ and cputime_ 
     ! (walltime and cputime in Fortran) 
     wall = walltime();
     cpu  = cputime();

     ! Read set and set array back to zero 
     PAPIF_accum(eventSet, sp_ops, check ) 
     if ( check .ne. PAPI_OK ) then 
         printf("Could not read first event set.\n");
         exit();
     endif 

     mmult(idim, A, B, C);

     PAPIF_read(eventSet, sp_ops, check)
     if ( check .ne. PAPI_OK ) then  
         printf("Could not read first event set.\n");
         exit()
     endif   

     ! Make a second call to PAPI_flops to recover the value since the last call
     PAPIF_flips( rtime, ptime, flpops, mflops, check);     


     wall = walltime() - wall;
     cpu  = cputime() - cpu;
    
     sum = 0.0;
     do i = 1, MAXSIZE
         sum = sum + C(i,i)
     enddo 

     print *, mdim, sum, wall, cpu
     
     ! each operation is a multipy and add, so to compute the flops in a matrix
     ! multiply you need to multiply by two.  Divide by 1000000 to get megaflops.
     
     print *, " Estimated megaflops = ", &
          (2 * (double) (MAXSIZE*MAXSIZE*MAXSIZE) / cpu ) / 1000000.0

     print *, "SP_OPS Count from PAPI = %15lld\n", sp_ops(1)

     ! Compute and print megaflopw based on PAPI counters
     mflops = (sp_ops(1)/cpu)/1000000.0
     print *, " PAPI megaflops = ", mflops

end program

/* Simple Matrix Multiplication Function */
subroutine mmult ( N, A, B, C )

     integer :: N
     real (kind=4) :: A(N,*), B(N,*), C(N,*)
     real (kind=4) :: dot

     do i=1, N
       do j = 1, N
          dot = 0.0
          do k = 1, N
            dot = dot + A(j,k) * B(k,i)
          enddo
          C(j,i) = dot;
       enddo
     enddo
end subroutine


