#include <R.h>
 #include <math.h>
 void g_deriv_z5485d1b ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = (p[0])*(x[5+i**k]) ;
y[1+i**l] = (p[0])*(x[9+i**k]) ;
y[2+i**l] = (p[0])*(x[13+i**k]) ;
y[3+i**l] = (p[0])*(x[17+i**k]) ;
y[4+i**l] = (p[0])*(x[21+i**k])+x[1+i**k] ;
y[5+i**l] = (p[0])*(x[25+i**k])+1.0 ;
y[6+i**l] = (p[0])*(x[29+i**k]) ;
y[7+i**l] = (p[0])*(x[33+i**k]) ;
y[8+i**l] = (p[0])*(x[37+i**k]) ;
y[9+i**l] = (p[0])*(x[41+i**k]) ; 
}
}