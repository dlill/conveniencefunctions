#include <R.h>
 #include <math.h>
 void obsfn_af9eb280_eb30a16a ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = p[3]*x[1+i**k] ; 
}
}