*****************************
********** Per kg ***********
*****************************

Sets
i    'raw materials'                             /1*10/
j    'basic chemicals'                           /1*13/
l    'raw materail conversion tech'               /1*6/
m    'plastics and alternative mat.'             /1*19/
n    'plastic debris size'                          /1/
o    'energy type'                                /1,2/
p    'environmental indicators'                   /1/
q    'chemical recycling prod. sale'              /1*3/
r    'plastic and alternative material use'      /1*13/
t    'time period'                               /1*20/
ss   'Time point (2023~2060)'                    /1*38/
u                                                 /1*2/
nn   'No.+1 datapoints in pareto optimal curves' /1*51/
IBIO(i)    'biomass raw materials'                /1*7/
IFOS(i)    'fossil-based raw materials'          /8,9,10/
MPL(m)     'plastic materials'                  /12*19/
MPA(m)     'alternative materials'               /1*11/
MAT(m)                                            /1*4/
MAAA(m)                                          /5*11/
MPPP(m)                                          /8,9,12*19/
CHM(j)                                           /1*4,5,8/;

alias(m,m1);

Parameters
AA(m,t)
AB(m)                                             /12*19 0.06/;

$call gdxxrw.exe sample_1.xlsx par=AR rng=Sheet1!A1:H68 rDim=2 cDim=1
*=== Now import data from GDX
Parameter AR(i,j,l);
$gdxin sample_1.gdx
$load AR
$gdxin
                                      
Parameters
BB(m,t);

$call gdxxrw.exe sample_1.xlsx par=C rng=Sheet2!A1:K10 
*=== Now import data from GDX
Parameter C(j,m);
$gdxin sample_1.gdx
$load C
$gdxin

$call gdxxrw.exe sample_1.xlsx par=CC rng=Sheet3!A1:H8 
*=== Now import data from GDX
Parameter CC(i,m);
$gdxin sample_1.gdx
$load CC
$gdxin

Parameters
CCR                                              /0.08/;

$call gdxxrw.exe sample_1.xlsx par=DD rng=Sheet32!A1:B19 rDim=1 
*=== Now import data from GDX
Parameter DD(m);
$gdxin sample_1.gdx
$load DD
$gdxin

Parameters
D(m)                                             
DEP                                              /0.03/

DDD(m);

loop(m,DDD(m) = DD(m););

$call gdxxrw.exe sample_1.xlsx par=DDDD rng=Sheet33!A1:B19 rDim=1
*=== Now import data from GDX
Parameter DDDD(m);
$gdxin sample_1.gdx
$load DDDD
$gdxin                                           

loop(m,DDDD(m) = DDDD(m)*1.2;);

$call gdxxrw.exe sample_1.xlsx par=ENAVV rng=Sheet4!A1:B16 
*=== Now import data from GDX
Parameter ENAVV(m,p);
$gdxin sample_1.gdx
$load ENAVV
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENMPV rng=Sheet5!A1:C9 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENMPV(m,n,p);
$gdxin sample_1.gdx
$load ENMPV
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENINV rng=Sheet6!A1:B17 
*=== Now import data from GDX
Parameter ENINV(m,p);
$gdxin sample_1.gdx
$load ENINV
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENRV rng=Sheet7!A1:C32 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENRV(i,l,p);
$gdxin sample_1.gdx
$load ENRV
$gdxin

Parameters
ENVCO(p)        /1 1/;

$call gdxxrw.exe sample_1.xlsx par=ENVJP rng=Sheet8!A1:C33 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENVJP(i,m,p);
$gdxin sample_1.gdx
$load ENVJP
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENVPP rng=Sheet9!A1:C63 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENVPP(j,m,p);
$gdxin sample_1.gdx
$load ENVPP
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENVPR rng=Sheet10!A1:B20
*=== Now import data from GDX
Parameter ENVPR(m,p);
$gdxin sample_1.gdx
$load ENVPR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENVSR rng=Sheet11!A1:B20
*=== Now import data from GDX
Parameter ENVSR(m,p);
$gdxin sample_1.gdx
$load ENVSR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENVTR rng=Sheet12!A1:B20
*=== Now import data from GDX
Parameter ENVTR(m,p);
$gdxin sample_1.gdx
$load ENVTR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENIN rng=Sheet13!A1:C20
*=== Now import data from GDX
Parameter ENIN(m,o);
$gdxin sample_1.gdx
$load ENIN
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENJP rng=Sheet14!A1:D16 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENJP(i,m,o);
$gdxin sample_1.gdx
$load ENJP
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENP rng=Sheet15!A1:D20 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENP(j,m,o);
$gdxin sample_1.gdx
$load ENP
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENPR rng=Sheet16!A1:C20
*=== Now import data from GDX
Parameter ENPR(m,o);
$gdxin sample_1.gdx
$load ENPR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENR rng=Sheet17!A1:D32 rDim=2 cDim=1
*=== Now import data from GDX
Parameter ENR(i,l,o);
$gdxin sample_1.gdx
$load ENR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENSR rng=Sheet18!A1:C20
*=== Now import data from GDX
Parameter ENSR(m,o);
$gdxin sample_1.gdx
$load ENSR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENTR rng=Sheet19!A1:C20
*=== Now import data from GDX
Parameter ENTR(m,o);
$gdxin sample_1.gdx
$load ENTR
$gdxin

$call gdxxrw.exe sample_1.xlsx par=ENWT rng=Sheet20!A1:C20
*=== Now import data from GDX
Parameter ENWT(m,o);
$gdxin sample_1.gdx
$load ENWT
$gdxin

Parameters
G(m,n)      /12*19.1 0.1/
GENN        /0.1/
H(n)        /1 0.25/;

$call gdxxrw.exe sample_1.xlsx par=II rng=Sheet34!A1:B16 rDim=1
*=== Now import data from GDX
Parameter II(m);
$gdxin sample_1.gdx
$load II
$gdxin

Parameters
IR          /0.1/
IPP         /0.02/
INC
INCEE(m)    /12*19 0.15/
;

$call gdxxrw.exe sample_1.xlsx par=INFF rng=Sheet21!A1:AM3
*=== Now import data from GDX
Parameter INFF(u,ss);
$gdxin sample_1.gdx
$load INFF
$gdxin

Parameters
JJ(m,n)      /12*19.1 1e-7/
KK(m,n)      /12*19.1 1e-5/
LL(n)        /1 0.095/
OCC         /0.17/
OPIN(m)     /1*19 0.14/;    

$call gdxxrw.exe sample_1.xlsx par=OPJP rng=Sheet22!A1:H8
*=== Now import data from GDX
Parameter OPJP(i,m);
$gdxin sample_1.gdx
$load OPJP
$gdxin

Parameter
OPMP(m,n)   /12*19.1 5.4/;

$call gdxxrw.exe sample_1.xlsx par=OPP rng=Sheet23!A1:K9
*=== Now import data from GDX
Parameter OPP(j,m);
$gdxin sample_1.gdx
$load OPP
$gdxin

Parameter
OPPR(m)     /1*19 0.2/;

$call gdxxrw.exe sample_1.xlsx par=OPR rng=Sheet24!A1:G11
*=== Now import data from GDX
Parameter OPR(i,l);
$gdxin sample_1.gdx
$load OPR
$gdxin

Parameters
OPRM(m)     /12*19 0.719/
OPSR(m)     /1*19 0.2/
OPTR(m)     /4*19 0.036/
PLL(m)      
PLU(m);

$call gdxxrw.exe sample_1.xlsx par=PLLL rng=Sheet25!A1:AM9
*=== Now import data from GDX
Parameter PLLL(m,ss);
$gdxin sample_1.gdx
$load PLLL
$gdxin

Parameters
PPP(r)
PR(q)       /1 1.391,2 0.115/
PRE(o)      /1 0.1268,2 0.01/
PT          
PW(m,n)               

$call gdxxrw.exe sample_1.xlsx par=T1 rng=Sheet36!A1:B19 rDim=1
*=== Now import data from GDX
Parameter T1(m);
$gdxin sample_1.gdx
$load T1
$gdxin

$call gdxxrw.exe sample_1.xlsx par=TT rng=Sheet37!A1:B19 rDim=1
*=== Now import data from GDX
Parameter TT(m);
$gdxin sample_1.gdx
$load TT
$gdxin

Parameters
TIPLF       /0.07/
UB          /1e12/       
X(r,m);

$call gdxxrw.exe sample_1.xlsx par=XX rng=Sheet26!A1:C15
*=== Now import data from GDX
Parameter XX(m,q);
$gdxin sample_1.gdx
$load XX
$gdxin

$call gdxxrw.exe sample_1.xlsx par=XXX rng=Sheet27!A1:L10
*=== Now import data from GDX
Parameter XXX(r,m);
$gdxin sample_1.gdx
$load XXX
$gdxin

Parameters
YR          /20/;

$call gdxxrw.exe sample_1.xlsx par=ENMMM rng=Sheet28!A1:B5
*=== Now import data from GDX
Parameter ENMMM(m,p);
$gdxin sample_1.gdx
$load ENMMM
$gdxin

$call gdxxrw.exe sample_1.xlsx par=OPMMM rng=Sheet38!A1:B11 rDim=1
*=== Now import data from GDX
Parameter OPMMM(m);
$gdxin sample_1.gdx
$load OPMMM
$gdxin

$call gdxxrw.exe sample_1.xlsx par=YY rng=Sheet29!A1:G16
*=== Now import data from GDX
Parameter YY(m,j);
$gdxin sample_1.gdx
$load YY
$gdxin
;

$call gdxxrw.exe sample_1.xlsx par=PPPP rng=Sheet30!A1:AM10
*=== Now import data from GDX
Parameter PPPP(r,ss);
$gdxin sample_1.gdx
$load PPPP
$gdxin
;

$call gdxxrw.exe sample_1.xlsx par=ENLFV rng=Sheet31!A1:B19
*=== Now import data from GDX
Parameter ENLFV(m,p);
$gdxin sample_1.gdx
$load ENLFV
$gdxin
;

$call gdxxrw.exe sample_1.xlsx par=OPWT rng=Sheet39!A1:B8 rDim=1
*=== Now import data from GDX
Parameter OPWT(m);
$gdxin sample_1.gdx
$load OPWT
$gdxin
;

$call gdxxrw.exe sample_1.xlsx par=PRR rng=Sheet35!A1:B19 rDim=1
*=== Now import data from GDX
Parameter PRR(m);
$gdxin sample_1.gdx
$load PRR
$gdxin
;

$call gdxxrw.exe sample_1.xlsx par=PPRR rng=Sheet40!A1:B10 rDim=1
*=== Now import data from GDX
Parameter PPRR(i);
$gdxin sample_1.gdx
$load PPRR
$gdxin
;

$call gdxxrw.exe sample_1.xlsx par=ELEEE rng=Sheet41!A1:B38 rDim=1
*=== Now import data from GDX
Parameter ELEEE(ss);
$gdxin sample_1.gdx
$load ELEEE
$gdxin
;

Parameters
PTAX      /0.337/

********** Parameter Change ***********
SUB         /1/
***************************************

ESP       /0.1/
CPRC(m)
ENRC(m,o)
ENVRC(m,p)
GEN
PA(m,n)
PLR(m)
RRR(j)
RPL(m)
RPP(m)
WA(m)
WPU(m)
INCE
WASPL(m);

Parameters
epss_min
epss_max
ELEE

****************************************
********** Current Scenarios ***********
****************************************
epsss
npvv(nn)
envv(p,nn)
zz(i,j,l,nn)
wasee(m,nn)

*******************************************
********** Different Time Point ***********
*******************************************
npvvv(ss,nn)
envvv(ss,nn)
zzz(i,j,l,ss,nn)
waseee(m,ss,nn)
pllllll(m,ss,nn)
plllll(m,nn);

loop(m,PLL(m) = PLLL(m,'1'););
loop(r,PPP(r) = PPPP(r,'1'););
                
loop((r,m),X(r,m) = XXX(r,m)*SUB;);
                
loop(m,AA(m,'1') = (1-DD(m))*DDD(m);
       BB(m,'1') = (1-DD(m))*(1-DDD(m))*DDDD(m););
                
loop((m,t)$(ord(t)<=T1(m) and ord(t)>1),AA(m,t) = (DD(m)**(ord(t)-1)+AA(m,t-1))*AA(m,'1'););                                          
loop((m,t)$(ord(t)<=TT(m) and ord(t)>1),BB(m,t) = (DD(m)**(ord(t)-1)+sum((j,m1),(YY(m1,j)*C(j,m1)*BB(m1,t-1))))*BB(m,'1'););

loop(m,D(m) = sum(t$(ord(t)<=T1(m)),DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t))+sum(t$(ord(t)<=TT(m)),BB(m,t)););
       
loop(m$(MPL(m)),PLR(m) = (1-SUB)*PLL(m);
                PLU(m) = 1+D(m););

loop(m$(MPA(m)),PLR(m) = sum(r,(X(r,m)*PPP(r)));     
                PLU(m) = 1+D(m););

loop(m,RPP(m) = (1-sum((j,m1),(YY(m1,j)*C(j,m))))*BB(m,'1')*sum(t$(ord(t)<=TT(m)),BB(m,t))*PLR(m);
       CPRC(m) = (sum(t$(ord(t)<=T1(m)),OPPR(m)*DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t)*OPSR(m))+sum(t$(ord(t)<=TT(m)),BB(m,t)*OPTR(m)))*PLR(m);
       RPL(m) = PLR(m)*D(m)/PLU(m);
       WA(m) = (PLR(m)-RPL(m))*II(m);                  
       WPU(m) = PLR(m)*AB(m);
       WASPL(m) = PLR(m)-RPL(m)-WA(m));       

loop(j,RRR(j) = sum(m,RPP(m)*YY(m,j)););

loop((m,n)$(MPL(m)),PA(m,n) = WPU(m)+WA(m)*JJ(m,n);
                    PW(m,n) = PLR(m)*KK(m,n)+WPU(m););
                    
loop((m,o),ENRC(m,o) = (sum(t$(ord(t)<=T1(m)),ENPR(m,o)*DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t)*ENSR(m,o))+sum(t$(ord(t)<=TT(m)),BB(m,t)*ENTR(m,o)))*PLR(m););    
                      
INC = (sum(m$(MPL(m)),((1+D(m))*PLR(m)*PRR(m)+sum(q,RPP(m)*XX(m,q)*PR(q)))/PLU(m))+SUB*sum(m$(MPA(m)),((1+D(m))*PLR(m)*PRR(m)+sum(q,RPP(m)*XX(m,q)*PR(q)))/PLU(m)));
GEN = GENN*INC;
INCE = sum(m,RPL(m)*(INCEE(m)+TIPLF));
PT = PTAX*sum(m,PLR(m));

loop((m,p),ENVRC(m,p) = (sum(t$(ord(t)<=T1(m)),ENVPR(m,p)*DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t)*ENVSR(m,p))+sum(t$(ord(t)<=TT(m)),BB(m,t)*ENVTR(m,p)))*PLR(m););

integer variable
z(i,j,l);

$onText
loop((i,j,l),z.l(i,j,l) = 1;);
$offText

Variables
acff;

Positive Variables
capr
cccc
cpp(m)
cprt
cpw(m)
depr
enex
enpp(m,o)
ene(o)
enrr(o)
enw(m,o)
inp
mm(i)
ma(i)
mp(i,j,l)
omm
pc(m,n)
pd(m,n)
rrrr(j)
ri(j)
wase(m)
wasex(m)
wpl(m)
enva(p)
envp(m,p)
envr(p)
envw(m,p)
pllll(m);

$onText
loop(m,cpp.l(m) = 1;
     cpw.l(m) = 1;
     wpl.l(m) = 1;);
     
cprt.l = 1;
depr.l = 1;
enex.l = 1;

loop((m,o),enpp.l(m,o) = 1;
           enw.l(m,o) = 1;);
           
loop(o,ene.l(o) = 1;
       enrr.l(o) = 1;);
       
inp.l = 1;

loop (i,mm.l(i) = 1;
        ma.l(i) = 1;);
        
loop ((i,j,l),mp.l(i,j,l) = 1;);

omm.l = 1;

loop ((m,n),pc.l(m,n) = 1;
            pd.l(m,n) = 1;);
            
loop (j,rrrr.l(j) = 1;
        ri.l(j) = 1;);

loop ((m,p),envp.l(m,p) = 1;
            envw.l(m,p) = 1;);

loop (p,envr.l(p) = 1;
        enva.l(p) = 1;
        env.l(p) = 1;);
$offText

Variables
npv
epss
env(p);

$onText
epss.l = 1;
$offText

Equations
Pre_eq1(i)
Pre_eq2(i)
Pre_eq3(j)
Pre_eq4(i,j,l)
Pre_eq5(i,j)
Pre_eq6(m)
Pre_eq7(m)
Pre_eq8(m)
Pre_eq9(m)
**Pre_eq10(m)
**Pre_eq11(r,m)
**Pre_eq12(m)
**Pre_eq13(m)
**Pre_eq14(m)
**Post_eq15(m)
**Post_eq16(m)
Post_eq17(m)
Post_eq177(m)
Post_eq18(m)
Post_eq189(m)
**Post_eq19(m)
**Post_eq20(m)
**Post_eq21(m)
**Post_eq22(m,t)
**Post_eq23(m)
**Post_eq24(m,t)
**Post_eq25(m)
**Post_eq26(j)
Post_eq27(j)
Post_eq277(j)
**Post_eq28(m)
Post_eq29(m,n)
**Post_eq30(m,n)
**Post_eq31(m,n)
Post_eq32(m,n)
Ener_eq33(o)
Ener_eq34(m,o)
**Ener_eq35(m,o)
Ener_eq36(m,o)
Ener_eq37(o)
Econ_eq38
Econ_eq39
**Econ_eq40
Econ_eq41
Econ_eq42
Econ_eq43
Econ_eq44
Econ_eq45
**Econ_eq46
**Econ_eq47
**Econ_eq48
Econ_eq49
**Econ_eq50
Econ_eq51
Econ_eq52
Econ_eq53
Envv_eq54(p)
Envv_eq55(m,p)
**Envv_eq56(m,p)
Envv_eq57(m,p)
Envv_eq58(p)
Envv_eq59(p)
Envv_eq60
Cons_max_eco
Cons_epss
**EEE
;

Pre_eq1(i)$(IBIO(i))..     mm(i) =e= sum((j,l), mp(i,j,l))+ma(i);
Pre_eq2(i)$(IFOS(i))..     mm(i) =e= sum((j,l), mp(i,j,l));
Pre_eq3(j)..               ri(j) =l= sum((i,l), mp(i,j,l)*AR(i,j,l));
Pre_eq4(i,j,l)..           mp(i,j,l) =l= UB*z(i,j,l);
Pre_eq5(i,j)..             sum(l, z(i,j,l)) =l= 1;

**Pre_eq6(m)$(MPL(m))..      PLR(m) =l= sum(j,(rrrr(j)*C(j,m)));
**Pre_eq7(m)$(MAAA(m))..     PLR(m) =l= sum(j,(rrrr(j)*C(j,m)))+sum(i$(IBIO(i)),(ma(i)*CC(i,m)));

Pre_eq6(m)$(MPL(m))..      pllll(m) =e= sum(j,(rrrr(j)*C(j,m)));
Pre_eq7(m)$(MAAA(m))..     pllll(m) =e= (sum(j,(rrrr(j)*C(j,m)))+sum(i$(IBIO(i)),(ma(i)*CC(i,m))));
Pre_eq8(m)$(MPL(m))..      PLR(m) + ESP * PLL(m) =l= pllll(m);
Pre_eq9(m)$(MAAA(m))..     PLR(m) =l= pllll(m);
**Pre_eq8(m)..               2*PLR(m) =g= PLL(m);
**Pre_eq12(m)$(MPL(m))..     PLR(m) =e= sum(m1$(MPA(m)),(XXX(m1,m)*pl(m1)));


Post_eq17(m)..             wpl(m) =e= sum(j,(rrrr(j)*(1-C(j,m)))*C(j,m)/(C(j,m)+1e-3));
Post_eq177(m)$(not MPPP(m))..   wpl(m) =e= 0;
**Post_eq189(m)..            wasex(m) =e= (pllll(m)*(1-D(m)/PLU(m))*(1-II(m)));
Post_eq189(m)..            wasex(m) =e= PLR(m)-RPL(m)-WA(m)+wpl(m);


Post_eq27(j)$(CHM(j))..        rrrr(j) =e= ri(j)+RRR(j)*sum((i,l),z(i,j,l));
Post_eq277(j)$(not CHM(j))..   rrrr(j) =e= ri(j);

Post_eq29(m,n)..           pd(m,n) =e= wasex(m)*G(m,n);

Post_eq32(m,n)..           pc(m,n) =e= pd(m,n)*H(n)+PA(m,n)*LL(n)+PW(m,n);

Ener_eq33(o)..             enrr(o) =e= sum((i,j,l),mp(i,j,l)*AR(i,j,l)*ENR(i,l,o));
Ener_eq34(m,o)..           enpp(m,o) =e= (sum(j,ri(j)*C(j,m)*ENP(j,m,o))+sum(i$IBIO(i),ma(i)*CC(i,m)*ENJP(i,m,o)));

Ener_eq36(m,o)..           enw(m,o) =e= WA(m)*ENIN(m,o)+wasex(m)*ENWT(m,o);
Ener_eq37(o)..             ene(o) =e= enrr(o)+(1-SUB)*sum(m$(MPL(m)),(enpp(m,o)+ENRC(m,o))/PLU(m)+enw(m,o))+SUB*sum(m$(MPA(m)),(enpp(m,o)+ENRC(m,o))/PLU(m)+enw(m,o));

Econ_eq38..                capr =e= sum((i,j,l),mp(i,j,l)*AR(i,j,l)*OPR(i,l));
Econ_eq39(m)..             cpp(m) =e= (sum(j,ri(j)*C(j,m)*OPP(j,m))+sum(i$IBIO(i),ma(i)*CC(i,m)*OPJP(i,m)));

Econ_eq41(m)..             cpw(m) =e= WA(m)*OPIN(m)+wasex(m)*(1-sum(n,G(m,n)))*OPWT(m)+sum(n,pc(m,n)*OPMP(m,n));
Econ_eq42..                cprt =e= capr+(1-SUB)*sum(m$(MPL(m)),(cpp(m)+CPRC(m))/PLU(m)+cpw(m))+SUB*(sum(m$(MPA(m)),(cpp(m)+CPRC(m))/PLU(m)+cpw(m))+sum(m$MAT(m),OPMMM(m)*RPL(m)));
Econ_eq43..                enex =e= sum(o,ene(o)*PRE(o));
Econ_eq44..                omm =e= OCC*cprt;
Econ_eq45..                inp =e= IPP*cprt;

Econ_eq49..                cccc =e= mm('8')*CCR;

Econ_eq51..                depr =e= DEP*cprt;
Econ_eq52..                acff =e= INC-GEN-depr-omm-inp-enex+INCE+cccc-PT-sum((i,j,l),mp(i,j,l)*PPRR(i));
Econ_eq53..                npv =e= (acff*(1-1/((1+IR)**YR))/IR-cprt)/(YR*sum(m$(MPL(m)),PLL(m)));
**EEE..                      npv =g= 0;
**Envv_eq60..                mm('8') =l= 1e9;


Envv_eq54(p)..             envr(p) =e= sum((i,j,l),mp(i,j,l)*AR(i,j,l)*ENRV(i,l,p));
Envv_eq55(m,p)..           envp(m,p) =e= (sum(j,ri(j)*C(j,m)*ENVPP(j,m,p))+sum(i$IBIO(i),ma(i)*CC(i,m)*ENVJP(i,m,p)));

Envv_eq57(m,p)..           envw(m,p) =e= WA(m)*ENINV(m,p)+wasex(m)*ENLFV(m,p)+sum(n,pc(m,n)*ENMPV(m,n,p));
Envv_eq58(p)..             enva(p) =e= mm('8')*ENVCO(p)+(1-SUB)*sum(m$(MPL(m)),(RPP(m)*ENAVV(m,p))+ENAVV(m,p)*pllll(m))+SUB*sum(m$(MPA(m)),(RPP(m)*ENAVV(m,p))+ENAVV(m,p)*pllll(m));
Envv_eq59(p)..             env(p) =e= envr(p)+(1-SUB)*sum(m$(MPL(m)),(envp(m,p)+ENVRC(m,p))/PLU(m)+envw(m,p))+SUB*(sum(m$(MPA(m)),(envp(m,p)+ENVRC(m,p))/PLU(m)+envw(m,p))+sum(m$MAT(m),ENMMM(m,p)*RPL(m)))-enva(p)-ene('1')*(0.481662062-ELEE);
**Envv_eq60..                mm('8') =l= 1e9;

Cons_max_eco..             env('1')/sum(m$(MPL(m)),PLL(m)) =e= epss;
Cons_epss..                env('1')/sum(m$(MPL(m)),PLL(m)) =l= epsss;

****************************************
********** Current Scenarios ***********
****************************************



model Plastic_value_chain /Pre_eq1,Pre_eq2,Pre_eq3,Pre_eq4,Pre_eq5,Pre_eq6,
                           Pre_eq7,Pre_eq8,Pre_eq9,Post_eq17,Post_eq277,Post_eq189,Post_eq27,Post_eq29,
                           Post_eq32,
                           Ener_eq33,Ener_eq34,Ener_eq36,Ener_eq37,
                           Econ_eq38,Econ_eq39,Econ_eq41,Econ_eq42,
                           Econ_eq43,Econ_eq44,Econ_eq45,Econ_eq49,Econ_eq51,Econ_eq52,
                           Econ_eq53,
                           Envv_eq54,Envv_eq55,Envv_eq57,Envv_eq58,
                           Envv_eq59
                           Cons_max_eco/;
Option mip = cplex;
Option optcr = 0;
option decimals = 8;
option threads = 0;

$onText
model Plastic_value_chain /Pre_eq1,Pre_eq2,Pre_eq3,Pre_eq4,Pre_eq5,Pre_eq6,
                           Pre_eq7,Pre_eq8,Pre_eq9,Post_eq17,Post_eq277,Post_eq189,Post_eq27,Post_eq29,
                           Post_eq32,
                           Ener_eq33,Ener_eq34,Ener_eq36,Ener_eq37,
                           Econ_eq38,Econ_eq39,Econ_eq41,Econ_eq42,
                           Econ_eq43,Econ_eq44,Econ_eq45,Econ_eq49,Econ_eq51,Econ_eq52,
                           Econ_eq53
                           /;


ELEE = 0.481662062;
Option mip = cplex;
Option optcr = 0;
option decimals = 8;
option threads = 0;

solve Plastic_value_chain using mip maximizing npv;
epss_max = epss.l;
display npv.l,acff.l,cprt.l;
display z.l,mp.l,ri.l,mm.l,ma.l,rrrr.l,pllll.l;
display depr.l,omm.l,inp.l,enex.l,cccc.l;
display epss.l,wpl.l,enva.l,envp.l,envr.l,envw.l,acff.l,cprt.l,env.l,wasex.l;
display D,AA,BB,PLU,INC,GEN,INCE,PT,PLL,PLR,RPL,WASPL;

$onText
solve Plastic_value_chain using mip minimizing npv;
**epss_max = epss.l;
display npv.l,acff.l,cprt.l;
display z.l,mp.l,ri.l,mm.l,ma.l,rrrr.l,pllll.l;
display depr.l,omm.l,inp.l,enex.l,cccc.l;
**display epss.l,wpl.l,enva.l,envp.l,envr.l,envw.l,acff.l,cprt.l,env.l,wasex.l;
display D,AA,BB,PLU,INC,GEN,INCE,PT,PLL,PLR;


solve Plastic_value_chain using mip minimizing epss;
epss_min = epss.l;
display npv.l,acff.l,cprt.l;
display z.l,epss.l,mp.l,ri.l,mm.l,wpl.l,ma.l,rrrr.l,pllll.l;
display depr.l,omm.l,inp.l,enex.l,cccc.l;
display enva.l,envp.l,envr.l,envw.l,acff.l,cprt.l,env.l,wasex.l;
display D,AA,BB,PLU,INC,GEN,INCE,PT,PLL,PLR,RPL,WASPL;
$offText

model Plastic_Pareto       /Pre_eq1,Pre_eq2,Pre_eq3,Pre_eq4,Pre_eq5,Pre_eq6,
                           Pre_eq7,Pre_eq8,Pre_eq9,Post_eq17,Post_eq277,Post_eq189,Post_eq27,Post_eq29,
                           Post_eq32,
                           Ener_eq33,Ener_eq34,Ener_eq36,Ener_eq37,
                           Econ_eq38,Econ_eq39,Econ_eq41,Econ_eq42,
                           Econ_eq43,Econ_eq44,Econ_eq45,Econ_eq49,Econ_eq51,Econ_eq52,
                           Econ_eq53,
                           Envv_eq54,Envv_eq55,Envv_eq57,Envv_eq58,
                           Envv_eq59
                           Cons_epss/;

$onText
loop(nn,epsss = epss_min + (ord(nn)-1)*(epss_max - epss_min)/(card(nn)-1);
solve Plastic_Pareto using mip maximizing npv;
npvv(nn) = npv.l;
loop(p,envv(p,nn) = env.l(p)/sum(m$(MPL(m)),PLL(m)););
loop((i,j,l),zz(i,j,l,nn) = z.l(i,j,l););
loop(m,wasee(m,nn) = wasex.l(m);plllll(m,nn) = pllll.l(m););
);

**display npvv;
**display envv;
**display zz;

execute_unload 'results_3.gdx' npvv envv zz wasee plllll
execute 'gdxxrw.exe results_3.gdx o=results_3.xlsx par=npvv rng=NewSheet1!';
execute 'gdxxrw.exe results_3.gdx o=results_3.xlsx par=envv rng=NewSheet2!';
execute 'gdxxrw.exe results_3.gdx o=results_3.xlsx par=zz rng=NewSheet3!';
execute 'gdxxrw.exe results_3.gdx o=results_3.xlsx par=wasee rng=NewSheet4!';
execute 'gdxxrw.exe results_3.gdx o=results_3.xlsx par=plllll rng=NewSheet5!';
$offText


*******************************************
********** Different Time Point ***********
*******************************************

loop (ss,
OPR('8','1') = OPR('8','1')*INFF('2',ss);ELEE = ELEEE(ss);
loop (m,OPTR(m) = OPTR(m)*INFF('1',ss); OPMP(m,'1') = OPMP(m,'1')*INFF('1',ss););
loop (m$(MPL(m)),PLL(m) = PLLL(m,ss););
loop (r,PPP(r) = PPPP(r,ss););

loop((r,m),X(r,m) = XXX(r,m)*SUB;);
                
loop(m,AA(m,'1') = (1-DD(m))*DDD(m);
       BB(m,'1') = (1-DD(m))*(1-DDD(m))*DDDD(m););
                
loop((m,t)$(ord(t)<=T1(m) and ord(t)>1),AA(m,t) = (DD(m)**(ord(t)-1)+AA(m,t-1))*AA(m,'1'););                                          
loop((m,t)$(ord(t)<=TT(m) and ord(t)>1),BB(m,t) = (DD(m)**(ord(t)-1)+sum((j,m1),(YY(m1,j)*C(j,m1)*BB(m1,t-1))))*BB(m,'1'););

loop(m,D(m) = sum(t$(ord(t)<=T1(m)),DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t))+sum(t$(ord(t)<=TT(m)),BB(m,t)););
       
loop(m$(MPL(m)),PLR(m) = (1-SUB)*PLL(m);
                PLU(m) = 1+D(m););

loop(m$(MPA(m)),PLR(m) = sum(r,(X(r,m)*PPP(r)));     
                PLU(m) = 1+D(m););

loop(m,RPP(m) = (1-sum((j,m1),(YY(m1,j)*C(j,m))))*BB(m,'1')*sum(t$(ord(t)<=TT(m)),BB(m,t))*PLR(m);
       CPRC(m) = (sum(t$(ord(t)<=T1(m)),OPPR(m)*DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t)*OPSR(m))+sum(t$(ord(t)<=TT(m)),BB(m,t)*OPTR(m)))*PLR(m);
       RPL(m) = PLR(m)*D(m)/PLU(m);
       WA(m) = (PLR(m)-RPL(m))*II(m);                  
       WPU(m) = PLR(m)*AB(m););       

loop(j,RRR(j) = sum(m,RPP(m)*YY(m,j)););

loop((m,n)$(MPL(m)),PA(m,n) = WPU(m)+WA(m)*JJ(m,n);
                    PW(m,n) = PLR(m)*KK(m,n)+WPU(m););
                    
loop((m,o),ENRC(m,o) = (sum(t$(ord(t)<=T1(m)),ENPR(m,o)*DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t)*ENSR(m,o))+sum(t$(ord(t)<=TT(m)),BB(m,t)*ENTR(m,o)))*PLR(m););    
                      
INC = (sum(m$(MPL(m)),((1+D(m))*PLR(m)*PRR(m)+sum(q,RPP(m)*XX(m,q)*PR(q)))/PLU(m))+SUB*sum(m$(MPA(m)),((1+D(m))*PLR(m)*PRR(m)+sum(q,RPP(m)*XX(m,q)*PR(q)))/PLU(m)));
GEN = GENN*INC;
INCE = sum(m,RPL(m)*(INCEE(m)+TIPLF));
PT = PTAX*sum(m,PLR(m));

loop((m,p),ENVRC(m,p) = (sum(t$(ord(t)<=T1(m)),ENVPR(m,p)*DD(m)**ord(t))+sum(t$(ord(t)<=T1(m)),AA(m,t)*ENVSR(m,p))+sum(t$(ord(t)<=TT(m)),BB(m,t)*ENVTR(m,p)))*PLR(m););

solve Plastic_value_chain using mip maximizing npv;
epss_max = epss.l;
**display npv.l;z.l;
solve Plastic_value_chain using mip minimizing epss;
epss_min = epss.l;

loop(nn,epsss = epss_min + (ord(nn)-1)*(epss_max - epss_min)/(card(nn)-1);
solve Plastic_Pareto using mip maximizing npv; 
npvvv(ss,nn) = npv.l;
envvv(ss,nn) = env.l('1')/sum(m$(MPL(m)),PLL(m));
loop((i,j,l),zzz(i,j,l,ss,nn) = z.l(i,j,l););
loop(m,waseee(m,ss,nn) = wasex.l(m);pllllll(m,ss,nn) = pllll.l(m););
);
);

execute_unload 'results_80.gdx' npvvv envvv zzz waseee pllllll
execute 'gdxxrw.exe results_80.gdx o=results_80.xlsx par=npvvv rng=Sheet1_sub1tra0.1!';
execute 'gdxxrw.exe results_80.gdx o=results_80.xlsx par=envvv rng=Sheet2_sub1tra0.1!';
execute 'gdxxrw.exe results_80.gdx o=results_80.xlsx par=zzz rng=Sheet3_sub1tra0.1!';
execute 'gdxxrw.exe results_80.gdx o=results_80.xlsx par=waseee rng=Sheet4_sub1tra0.1!';
execute 'gdxxrw.exe results_80.gdx o=results_80.xlsx par=pllllll rng=Sheet5_sub1tra0.1!';
