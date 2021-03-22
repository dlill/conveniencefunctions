#include <R.h>
 #include <math.h>
 void P_enzymeKinetics_deriv_22fgeqn8 ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0]) ;
y[13+i**l] = exp(p[1]) ;
y[26+i**l] = 1.0 ;
y[39+i**l] = 1.0 ;
y[52+i**l] = exp(p[4]) ;
y[65+i**l] = exp(p[5]) ;
y[78+i**l] = exp(p[6]) ;
y[91+i**l] = 1.0 ;
y[104+i**l] = 1.0 ;
y[117+i**l] = 1.0 ;
y[130+i**l] = 1.0 ;
y[143+i**l] = 1.0 ; 
}
}