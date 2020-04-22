/** Code auto-generated by cOde 1.1.0 **/
#include <R.h> 
 #include <math.h> 

static double parms[9];
static double forc[0];
static double cons[0];
static double eventcounter[1];
static double range[2];

#define nGridpoints 2 
#define nSplines 0 
#define precision 1e-05 

#define phospho_AKT parms[0] 
 #define phospho_AKT_add parms[1] 
 #define mutation_on parms[2] 
 #define dephospho_AKT parms[3] 
 #define dephospho_AKT_add parms[4] 
 #define mutation_off parms[5] 
 #define y0_0 parms[6] 
 #define y1_0 parms[7] 
 #define y2_0 parms[8] 
#define tmin range[0]
#define tmax range[1]


void x_initmod(void (* odeparms)(int *, double *)) {
	 int N=9;
	 odeparms(&N, parms);
	 for(int i=0; i<1; ++i) eventcounter[i] = 0;
}

void x_initforc(void (* odeforcs)(int *, double *)) {
	 int N=0;
	 odeforcs(&N, forc);
}

/** Derivatives (ODE system) **/
void x_derivs (int *n, double *t, double *y, double *ydot, double *RPAR, int *IPAR) {

	 double time = *t;

	 ydot[0] = -1.0*((phospho_AKT+phospho_AKT_add*mutation_on)*y[0]*y[2]);
 	 ydot[1] = 1.0*((phospho_AKT+phospho_AKT_add*mutation_on)*y[0]*y[2])-1.0*((dephospho_AKT+dephospho_AKT_add*mutation_off)*y[1]);
 	 ydot[2] = 1.0*(0.0);

}

/** Event function **/
void x_myevent(int *n, double *t, double *y) {

	 double time = *t;

	 if(*t == 60.0 & eventcounter[0] == 0) {
		y[2] = 1.0;
		eventcounter[0] = eventcounter[0] + 1.;
	 }


}
