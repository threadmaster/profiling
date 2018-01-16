// C Driver Program to demonstrate lbstime.a library for timing C and Fortan Codes 
//
// Andrew J. Pounds, Ph.D.
// Departments of Chemistry and Computer Science
// Mercer University
// Spring 2007

#include <stdio.h>
#include <stdlib.h>
#define MAXSIZE 1000

/* Function Prototypes */
void mmult( int N, float *A, float *B, float *C);
double walltime_();
double cputime_();


int main() {
  
    int i, j, k, idim; 
    float *A, *B, *C;
    float sum;
    double wall, cpu;

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

     // Note: call walltime_ and cputime_ (walltime and cputime in Fortran) 
     wall = walltime_();
     cpu  = cputime_();
     
     mmult(idim, A, B, C);

     wall = walltime_() - wall;
     cpu  = cputime_() - cpu;
    
     sum = 0.0;
     for (i=0;i<MAXSIZE;i++) {
         sum = sum + *(C+i*MAXSIZE+i);
         }

     printf(" %i  %f  %f  %f\n", idim, sum, wall, cpu);

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


