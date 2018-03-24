! Fortran Driver Program to demonstrate low level PAPI calls 
!
! Andrew J. Pounds, Ph.D.
! Departments of Chemistry and Computer Science
! Mercer University
! Spring 2018 


program llfdriver
 
#include "f90papi.h"

    integer :: i, j, k, mdim 
    integer, parameter :: MAXSIZE = 1000
    integer, parameter :: NUM_EVENTS = 2 
    real (KIND=4), dimension(MAXSIZE,MAXSIZE) ::  A, B, C;
    real (kind=4) :: sum 
    real (kind=8) :: wall, cpu;

    ! PAPI Variables
    ! c_int is a an normal fortran integet (kind=4)
    ! long long in fortran is a (kind=8) integer
    real (kind=4) :: rtime, ptime, mflops
    integer (kind=8) ::  flpops
    integer (kind=4) :: check, eventSet
    integer (kind=8), dimension(NUM_EVENTS) :: sp_ops

    ! Define external functions in lbstiming library
    real (kind=8) :: cputime, walltime
    external cputime, walltime

 
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

     ! Initilialize the check variable
     check = PAPI_VER_CURRENT  

     ! Initialize the PAPI library
     call PAPIF_library_init(check);
     if ((check .ne. PAPI_VER_CURRENT) .and. (check .gt. 0)) then 
         print *, "PAPI library version mismatch!"
         call exit() 
     endif

     print *, "Check = ", check, PAPI_VER_CURRENT

     if (check .lt. 0) then 
         print *, "PAPI initialization error."
         call exit()
     endif

     call PAPIF_is_initialized(check);
     if (check .ne.  PAPI_LOW_LEVEL_INITED )  then
         print *, "PAPI low level initialization failed."
         call exit()
     endif 
  
     ! Create a PAPI Event Set 
     eventSet = PAPI_NULL; 

     call PAPIF_create_eventset(eventSet,check)
     if (check .ne. PAPI_OK ) then 
         print *, "Could not create PAPI event set."
         call exit()
     endif 

     ! Add the particular events to be counted to each event set
     call PAPIF_add_event(eventSet, PAPI_SP_OPS, check)
     if (check .ne. PAPI_OK ) then 
         print *, "Could not create PAPI_SP_OPS event."
         call exit()
     endif 

     ! Start the counters in each event set 
     call PAPIF_start(eventSet,check)
     if ( check .ne. PAPI_OK ) then 
         print *, "Could not start PAPI_SP_OPS counter.";
         call exit()
     endif 

     ! Note: call lbstime walltime_ and cputime_ 
     ! (walltime and cputime in Fortran) 
     wall = walltime();
     cpu  = cputime();

     ! Read set and set array back to zero 
     call PAPIF_accum(eventSet, sp_ops, check ) 
     if ( check .ne. PAPI_OK ) then 
         print *, "Could not read first event set.";
         call exit()
     endif 

     call mmult(mdim, A, B, C);

     call PAPIF_read(eventSet, sp_ops, check)
     if ( check .ne. PAPI_OK ) then  
         print *, "Could not read first event set.";
         call exit()
     endif   

     ! Make a second call to PAPI_flops to recover the value since the last call
     call PAPIF_flips( rtime, ptime, flpops, mflops, check);     


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
          (2 * dble(MAXSIZE*MAXSIZE*MAXSIZE) / cpu ) / 1000000.0

     print *, "SP_OPS Count from PAPI = ", sp_ops(1)

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


