// C Driver Program to demonstrate low level PAPI calls  timing C 
//
// Andrew J. Pounds, Ph.D.
// Departments of Chemistry and Computer Science
// Mercer University
// Spring 2007

#include <stdio.h>
#include <stdlib.h>
#include <papi.h>
#define MAXSIZE 1000
#define NUM_EVENTS 2

/* Function Prototypes */
void mmult( int N, float *A, float *B, float *C);
double walltime_();
double cputime_();


int main() {
  
    int i, j, k, idim; 
    float *A, *B, *C;
    float sum;
    double wall, cpu;

    // PAPI Variables
    float rtime, ptime, mflops;
    long long  flpops;
    int check;

    long long sp_ops[NUM_EVENTS]; 

    // Create space for matrices  
    A = (float *) malloc(MAXSIZE*MAXSIZE*sizeof(float));
    B = (float *) malloc(MAXSIZE*MAXSIZE*sizeof(float));
    C = (float *) malloc(MAXSIZE*MAXSIZE*sizeof(float));

    // Create matrices 
    for (i=0;i<MAXSIZE;i++){
        for (j=0;j<MAXSIZE;j++) {
           if ( i != j ) {
               *(A+MAXSIZE*i+j) = 0.0; 
               *(B+MAXSIZE*i+j) = 1.0; 
               }
           else {
               *(A+MAXSIZE*i+j) = 1.0; 
               *(B+MAXSIZE*i+j) = 1.0; 
                }
        }
     }

     idim = MAXSIZE;
     int retval;

     // Initialize the PAPI library
     retval = PAPI_library_init(PAPI_VER_CURRENT);
     if (retval != PAPI_VER_CURRENT && retval > 0) {
         fprintf(stderr,"PAPI library version mismatch!\n");
         exit(1); 
         }

     if (retval < 0) {
         printf("PAPI initialization error.\n");
         exit(retval);
         }
     retval = PAPI_is_initialized();
     if (retval != PAPI_LOW_LEVEL_INITED) {
         printf("PAPI low level initialization failed.\n");
         exit(retval);
     }
    
     // Create a PAPI Event Set 
     int eventSet = PAPI_NULL; 

     if (PAPI_create_eventset(&eventSet) != PAPI_OK ) { 
         printf("Could not create PAPI event set.\n");
         exit(1);
         }

     // Add the particular events to be counted to each event set
     if (PAPI_add_event(eventSet, PAPI_SP_OPS) != PAPI_OK ) { 
         printf("Could not create PAPI_SP_OPS event.\n");
         exit(1);
         }

     // Start the counters in each event set 
     if (PAPI_start(eventSet) != PAPI_OK ) { 
         printf("Could not start PAPI_SP_OPS counter.\n");
         exit(1);
         }

     // Note: call lbstime walltime_ and cputime_ 
     // (walltime and cputime in Fortran) 
     wall = walltime_();
     cpu  = cputime_();

     // Read set and set array back to zero 
     if (PAPI_accum(eventSet, sp_ops ) != PAPI_OK ) { 
         printf("Could not read first event set.\n");
         exit(1);
         }

     mmult(idim, A, B, C);

     if (PAPI_read(eventSet, sp_ops) != PAPI_OK ) { 
         printf("Could not read first event set.\n");
         exit(1);
         }

     // Make a second call to PAPI_flops to recover the value since the last call
     check = PAPI_flips( &rtime, &ptime, &flpops, &mflops);     


     wall = walltime_() - wall;
     cpu  = cputime_() - cpu;
    
     sum = 0.0;
     for (i=0;i<MAXSIZE;i++) {
         sum = sum + *(C+i*MAXSIZE+i);
         }

     printf(" %i  %f  %f  %f\n", idim, sum, wall, cpu);

     
     // each operation is a multipy and add, so to compute the flops in a matrix
     // multiply you need to multiply by two.  Divide by 1000000 to get megaflops.
     
     printf(" Estimated megaflops = %f\n", (2 * (double) (MAXSIZE*MAXSIZE*MAXSIZE) / cpu ) / 1000000.0);

     printf("SP_OPS Count from PAPI = %15lld\n", sp_ops[0]);

     // Compute and print megaflopw based on PAPI counters
     mflops = (sp_ops[0]/cpu)/1000000.0;
     printf(" PAPI megaflops = %f\n", mflops);

     free(A);
     free(B);
     free(C);

}

/* Simple Matrix Multiplication Function */
void mmult ( int N, float *A, float *B, float *C ){

     int i, j, k;
     float dot;

     for (i=0;i<N;i++) {
       for (j=0;j<N;j++ ) {
         dot = 0.0;
         for (k=0;k<N;k++) {
            dot = dot + *(A+i*N+k) * *(B+j*N+k);
            }
         *(C+i*N+j) = dot;
       }
     }
} 


