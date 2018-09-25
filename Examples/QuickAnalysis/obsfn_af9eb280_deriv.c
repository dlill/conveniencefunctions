#include <R.h>
 #include <math.h>
 void obsfn_af9eb280_deriv_a3e7a05a ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = (p[3])*(x[4+i**k]) ;
y[1+i**l] = (p[3])*(x[7+i**k]) ;
y[2+i**l] = (p[3])*(x[10+i**k]) ;
y[3+i**l] = (p[3])*(x[13+i**k]) ;
y[4+i**l] = (p[3])*(x[16+i**k]) ;
y[5+i**l] = (p[3])*(x[19+i**k]) ;
y[6+i**l] = (p[3])*(x[22+i**k])+x[1+i**k] ; 
}
}