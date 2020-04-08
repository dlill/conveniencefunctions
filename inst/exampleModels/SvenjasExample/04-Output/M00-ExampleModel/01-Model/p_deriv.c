#include <R.h>
 #include <math.h>
 void p_deriv_djaa274k ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0]) ;
y[13+i**l] = exp(p[1]) ;
y[24+i**l] = exp(p[2]) ;
y[35+i**l] = exp(p[3]) ;
y[46+i**l] = exp(p[4]) ;
y[57+i**l] = exp(p[5]) ;
y[68+i**l] = exp(p[6]) ;
y[79+i**l] = exp(p[7]) ; 
}
}