#include <R.h>
 #include <math.h>
 void P_enzymeKinetics_2cswms35 ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0]) ;
y[1+i**l] = exp(p[1]) ;
y[2+i**l] = p[2] ;
y[3+i**l] = p[3] ;
y[4+i**l] = exp(p[4]) ;
y[5+i**l] = exp(p[5]) ;
y[6+i**l] = exp(p[6]) ;
y[7+i**l] = p[7] ; 
}
}