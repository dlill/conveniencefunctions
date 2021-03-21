/** Code auto-generated by cOde 1.1.0 **/
#include <R.h> 
 #include <math.h> 

static double parms[16];
static double forc[0];
static double cons[0];
static double range[2];

#define nGridpoints 2 
#define nSplines 0 
#define precision 1e-05 

#define Epo_degradation_BaF3 parms[0] 
 #define k_phos parms[1] 
 #define k_exp_homo parms[2] 
 #define nuc parms[3] 
 #define cyt parms[4] 
 #define k_exp_hetero parms[5] 
 #define k_imp_homo parms[6] 
 #define k_imp_hetero parms[7] 
 #define y0_0 parms[8] 
 #define y1_0 parms[9] 
 #define y2_0 parms[10] 
 #define y3_0 parms[11] 
 #define y4_0 parms[12] 
 #define y5_0 parms[13] 
 #define y6_0 parms[14] 
 #define y7_0 parms[15] 
#define tmin range[0]
#define tmax range[1]


void odemodel_Boehm_JProteomeRes2014_initmod(void (* odeparms)(int *, double *)) {
	 int N=16;
	 odeparms(&N, parms);
}

void odemodel_Boehm_JProteomeRes2014_initforc(void (* odeforcs)(int *, double *)) {
	 int N=0;
	 odeforcs(&N, forc);
}

/** Derivatives (ODE system) **/
void odemodel_Boehm_JProteomeRes2014_derivs (int *n, double *t, double *y, double *ydot, double *RPAR, int *IPAR) {

	 double time = *t;

	 ydot[0] = -2.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*(pow(y[0],2.00e+00))*k_phos)-1.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*y[0]*y[2]*k_phos)+2.00e+00*(1.00e+00*k_exp_homo*y[5])*(nuc/cyt)+1.00e+00*(1.00e+00*k_exp_hetero*y[6])*(nuc/cyt);
 	 ydot[1] = 1.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*(pow(y[0],2.00e+00))*k_phos)-1.00e+00*(1.00e+00*k_imp_homo*y[1]);
 	 ydot[2] = -1.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*y[0]*y[2]*k_phos)-2.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*(pow(y[2],2.00e+00))*k_phos)+1.00e+00*(1.00e+00*k_exp_hetero*y[6])*(nuc/cyt)+2.00e+00*(1.00e+00*k_exp_homo*y[7])*(nuc/cyt);
 	 ydot[3] = 1.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*y[0]*y[2]*k_phos)-1.00e+00*(1.00e+00*k_imp_hetero*y[3]);
 	 ydot[4] = 1.00e+00*(1.00e+00*(1.25e-07*exp(-1.00e+00*Epo_degradation_BaF3*time))*(pow(y[2],2.00e+00))*k_phos)-1.00e+00*(1.00e+00*k_imp_homo*y[4]);
 	 ydot[5] = 1.00e+00*(1.00e+00*k_imp_homo*y[1])*(cyt/nuc)-1.00e+00*(1.00e+00*k_exp_homo*y[5]);
 	 ydot[6] = 1.00e+00*(1.00e+00*k_imp_hetero*y[3])*(cyt/nuc)-1.00e+00*(1.00e+00*k_exp_hetero*y[6]);
 	 ydot[7] = 1.00e+00*(1.00e+00*k_imp_homo*y[4])*(cyt/nuc)-1.00e+00*(1.00e+00*k_exp_homo*y[7]);

}

