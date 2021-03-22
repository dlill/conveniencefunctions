#include <R.h>
 #include <math.h>
 void P_enzymeKinetics_deriv_359v3q9i ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0]) ;
y[9+i**l] = exp(p[1]) ;
y[18+i**l] = 1.0 ;
y[27+i**l] = 1.0 ;
y[36+i**l] = exp(p[4]) ;
y[45+i**l] = exp(p[5]) ;
y[54+i**l] = exp(p[6]) ;
y[63+i**l] = 1.0 ; 
}
}