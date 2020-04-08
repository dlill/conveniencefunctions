#include <R.h>
 #include <math.h>
 void p_deriv_muz791o6 ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0]) ;
y[11+i**l] = exp(p[1]) ;
y[22+i**l] = exp(p[2]) ;
y[33+i**l] = exp(p[3]) ;
y[44+i**l] = exp(p[4]) ;
y[55+i**l] = exp(p[5]) ;
y[66+i**l] = exp(p[6]) ;
y[77+i**l] = exp(p[7]) ;
y[88+i**l] = exp(p[8]) ;
y[99+i**l] = exp(p[9]) ; 
}
}