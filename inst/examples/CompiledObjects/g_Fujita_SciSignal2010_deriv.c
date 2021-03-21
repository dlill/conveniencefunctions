#include <R.h>
 #include <math.h>
 void g_Fujita_SciSignal2010_deriv_diqyliis ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = (p[0])*(x[16+i**k])+(p[0])*(x[18+i**k]) ;
y[1+i**l] = (p[1])*(x[13+i**k])+(p[1])*(x[15+i**k]) ;
y[2+i**l] = (p[2])*(x[19+i**k]) ;
y[3+i**l] = (p[0])*(x[27+i**k])+(p[0])*(x[29+i**k]) ;
y[4+i**l] = (p[1])*(x[24+i**k])+(p[1])*(x[26+i**k]) ;
y[5+i**l] = (p[2])*(x[30+i**k]) ;
y[6+i**l] = (p[0])*(x[38+i**k])+(p[0])*(x[40+i**k]) ;
y[7+i**l] = (p[1])*(x[35+i**k])+(p[1])*(x[37+i**k]) ;
y[8+i**l] = (p[2])*(x[41+i**k]) ;
y[9+i**l] = (p[0])*(x[49+i**k])+(p[0])*(x[51+i**k]) ;
y[10+i**l] = (p[1])*(x[46+i**k])+(p[1])*(x[48+i**k]) ;
y[11+i**l] = (p[2])*(x[52+i**k]) ;
y[12+i**l] = (p[0])*(x[60+i**k])+(p[0])*(x[62+i**k]) ;
y[13+i**l] = (p[1])*(x[57+i**k])+(p[1])*(x[59+i**k]) ;
y[14+i**l] = (p[2])*(x[63+i**k]) ;
y[15+i**l] = (p[0])*(x[71+i**k])+(p[0])*(x[73+i**k]) ;
y[16+i**l] = (p[1])*(x[68+i**k])+(p[1])*(x[70+i**k]) ;
y[17+i**l] = (p[2])*(x[74+i**k]) ;
y[18+i**l] = (p[0])*(x[82+i**k])+(p[0])*(x[84+i**k]) ;
y[19+i**l] = (p[1])*(x[79+i**k])+(p[1])*(x[81+i**k]) ;
y[20+i**l] = (p[2])*(x[85+i**k]) ;
y[21+i**l] = (p[0])*(x[93+i**k])+(p[0])*(x[95+i**k]) ;
y[22+i**l] = (p[1])*(x[90+i**k])+(p[1])*(x[92+i**k]) ;
y[23+i**l] = (p[2])*(x[96+i**k]) ;
y[24+i**l] = (p[0])*(x[104+i**k])+(p[0])*(x[106+i**k]) ;
y[25+i**l] = (p[1])*(x[101+i**k])+(p[1])*(x[103+i**k]) ;
y[26+i**l] = (p[2])*(x[107+i**k]) ;
y[27+i**l] = (p[0])*(x[115+i**k])+(p[0])*(x[117+i**k]) ;
y[28+i**l] = (p[1])*(x[112+i**k])+(p[1])*(x[114+i**k]) ;
y[29+i**l] = (p[2])*(x[118+i**k]) ;
y[30+i**l] = (p[0])*(x[126+i**k])+(p[0])*(x[128+i**k]) ;
y[31+i**l] = (p[1])*(x[123+i**k])+(p[1])*(x[125+i**k]) ;
y[32+i**l] = (p[2])*(x[129+i**k]) ;
y[33+i**l] = (p[0])*(x[137+i**k])+(p[0])*(x[139+i**k])+(x[5+i**k]+x[7+i**k]) ;
y[34+i**l] = (p[1])*(x[134+i**k])+(p[1])*(x[136+i**k]) ;
y[35+i**l] = (p[2])*(x[140+i**k]) ;
y[36+i**l] = (p[0])*(x[148+i**k])+(p[0])*(x[150+i**k]) ;
y[37+i**l] = (p[1])*(x[145+i**k])+(p[1])*(x[147+i**k])+(x[2+i**k]+x[4+i**k]) ;
y[38+i**l] = (p[2])*(x[151+i**k]) ;
y[39+i**l] = (p[0])*(x[159+i**k])+(p[0])*(x[161+i**k]) ;
y[40+i**l] = (p[1])*(x[156+i**k])+(p[1])*(x[158+i**k]) ;
y[41+i**l] = (p[2])*(x[162+i**k])+x[8+i**k] ;
y[42+i**l] = (p[0])*(x[170+i**k])+(p[0])*(x[172+i**k]) ;
y[43+i**l] = (p[1])*(x[167+i**k])+(p[1])*(x[169+i**k]) ;
y[44+i**l] = (p[2])*(x[173+i**k]) ;
y[45+i**l] = (p[0])*(x[181+i**k])+(p[0])*(x[183+i**k]) ;
y[46+i**l] = (p[1])*(x[178+i**k])+(p[1])*(x[180+i**k]) ;
y[47+i**l] = (p[2])*(x[184+i**k]) ;
y[48+i**l] = (p[0])*(x[192+i**k])+(p[0])*(x[194+i**k]) ;
y[49+i**l] = (p[1])*(x[189+i**k])+(p[1])*(x[191+i**k]) ;
y[50+i**l] = (p[2])*(x[195+i**k]) ;
y[51+i**l] = (p[0])*(x[203+i**k])+(p[0])*(x[205+i**k]) ;
y[52+i**l] = (p[1])*(x[200+i**k])+(p[1])*(x[202+i**k]) ;
y[53+i**l] = (p[2])*(x[206+i**k]) ;
y[54+i**l] = (p[0])*(x[214+i**k])+(p[0])*(x[216+i**k]) ;
y[55+i**l] = (p[1])*(x[211+i**k])+(p[1])*(x[213+i**k]) ;
y[56+i**l] = (p[2])*(x[217+i**k]) ;
y[57+i**l] = (p[0])*(x[225+i**k])+(p[0])*(x[227+i**k]) ;
y[58+i**l] = (p[1])*(x[222+i**k])+(p[1])*(x[224+i**k]) ;
y[59+i**l] = (p[2])*(x[228+i**k]) ;
y[60+i**l] = (p[0])*(x[236+i**k])+(p[0])*(x[238+i**k]) ;
y[61+i**l] = (p[1])*(x[233+i**k])+(p[1])*(x[235+i**k]) ;
y[62+i**l] = (p[2])*(x[239+i**k]) ;
y[63+i**l] = (p[0])*(x[247+i**k])+(p[0])*(x[249+i**k]) ;
y[64+i**l] = (p[1])*(x[244+i**k])+(p[1])*(x[246+i**k]) ;
y[65+i**l] = (p[2])*(x[250+i**k]) ;
y[66+i**l] = (p[0])*(x[258+i**k])+(p[0])*(x[260+i**k]) ;
y[67+i**l] = (p[1])*(x[255+i**k])+(p[1])*(x[257+i**k]) ;
y[68+i**l] = (p[2])*(x[261+i**k]) ;
y[69+i**l] = (p[0])*(x[269+i**k])+(p[0])*(x[271+i**k]) ;
y[70+i**l] = (p[1])*(x[266+i**k])+(p[1])*(x[268+i**k]) ;
y[71+i**l] = (p[2])*(x[272+i**k]) ;
y[72+i**l] = (p[0])*(x[280+i**k])+(p[0])*(x[282+i**k]) ;
y[73+i**l] = (p[1])*(x[277+i**k])+(p[1])*(x[279+i**k]) ;
y[74+i**l] = (p[2])*(x[283+i**k]) ;
y[75+i**l] = (p[0])*(x[291+i**k])+(p[0])*(x[293+i**k]) ;
y[76+i**l] = (p[1])*(x[288+i**k])+(p[1])*(x[290+i**k]) ;
y[77+i**l] = (p[2])*(x[294+i**k]) ;
y[78+i**l] = (p[0])*(x[302+i**k])+(p[0])*(x[304+i**k]) ;
y[79+i**l] = (p[1])*(x[299+i**k])+(p[1])*(x[301+i**k]) ;
y[80+i**l] = (p[2])*(x[305+i**k]) ;
y[81+i**l] = (p[0])*(x[313+i**k])+(p[0])*(x[315+i**k]) ;
y[82+i**l] = (p[1])*(x[310+i**k])+(p[1])*(x[312+i**k]) ;
y[83+i**l] = (p[2])*(x[316+i**k]) ;
y[84+i**l] = (p[0])*(x[324+i**k])+(p[0])*(x[326+i**k]) ;
y[85+i**l] = (p[1])*(x[321+i**k])+(p[1])*(x[323+i**k]) ;
y[86+i**l] = (p[2])*(x[327+i**k]) ; 
}
}