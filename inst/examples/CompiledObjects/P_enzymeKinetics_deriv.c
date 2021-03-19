#include <R.h>
 #include <math.h>
 void P_enzymeKinetics_deriv_iztgdb0j ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0]) ;
y[7+i**l] = exp(p[1]) ;
y[14+i**l] = 1.0 ;
y[21+i**l] = 1.0 ;
y[28+i**l] = 1.0 ;
y[35+i**l] = 1.0 ; 
}
}