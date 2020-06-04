/* table.g   This program creates output for the tables in the paper
with Tom Fomby on global warming 2/12/99 */

#lineson;
new;

ofile1="table1.out";
ofile2="table2.out";
glsdum=0;  /* set to 0 for AR(1) GLS and 1 for ARMA(1,1) GLS */

newfile=1;
kernel=1;  prewhite=0;
bigm=-1;  /* set bigm=-1 to use automatic bandwidth*/

/* critical values for right tailed 5% 2.5% and 1% test and one-sided
   confidence intervals */

cvthac=1.645~1.96~2.33;
cvtps=1.720~2.152~2.647;  btps=0.716~0.995~1.501;
cvtstar=6.46~8.22~10.5; btstar=0.681~0.975~1.36;
cvtwt=2.298~2.835~3.479;
Jcv=0.488~0.678~0.908;

load data[140,6]=trends93.prn;
load data1[141,2]=jones.prn;
load data2[340,14]=long.prn;
load data3[145,14]=jobp.prn;
load data4[289,13]=dutch.prn;

i=2;
longser=data2[1,2:13]';
do while i<=rows(data2);
  longser=longser|data2[i,2:13]';
  i=i+1;
endo;

format /lz 6,4;
output file = ^ofile1 on;
print; print;
print "Table of Empirical Results for Temperature Series, Standard Regressions";
print;
print "                                     tstar                         t-PS"\
"           J";
print "      T     bhat   t-hac   twt    5%    2.5%    1%    btild"\
"      5%    2.5%    1%";
output off;

output file = ^ofile2 on;
print "Table of Empirical Results for Temperature Series, GLS Regressions";
print;
print "          ahat   that   bhat    t-hac  tstar  btild    t-ps";
output off;


/* set plotflag to 1 to have the series plotted */
plotflag=0;  if plotflag==1; library pgraph; endif;
jj=2;
do while jj<=33;
print "jj=" jj;
if jj==7; jj=33; endif;

if jj==1; series="co2"; endif;
if jj==2; series="jwb";  endif;
if jj==3; series="jwbenso";  endif;
if jj==4; series="wh";  endif;
if jj==5; series="vgl";  endif;
if jj==6; series="jp"; endif;
if jj==7; series="long(mon)"; endif;
if jj==8; series="long(year)"; endif;
if jj==9; series="long(Jan)"; endif;
if jj==10; series="long(Feb)"; endif;
if jj==11; series="long(Mar)"; endif;
if jj==12; series="long(Apr)"; endif;
if jj==13; series="long(May)"; endif;
if jj==14; series="long(Jun)"; endif;
if jj==15; series="long(Jul)"; endif;
if jj==16; series="long(Aug)"; endif;
if jj==17; series="long(Sep)"; endif;
if jj==18; series="long(Oct)"; endif;
if jj==19; series="long(Nov)"; endif;
if jj==20; series="long(Dec)"; endif;
if jj==21; series="jobp(Jan)"; endif;
if jj==22; series="jobp(Feb)"; endif;
if jj==23; series="jobp(Mar)"; endif;
if jj==24; series="jobp(Apr)"; endif;
if jj==25; series="jobp(May)"; endif;
if jj==26; series="jobp(Jun)"; endif;
if jj==27; series="jobp(Jul)"; endif;
if jj==28; series="jobp(Aug)"; endif;
if jj==29; series="jobp(Sep)"; endif;
if jj==30; series="jobp(Oct)"; endif;
if jj==31; series="jobp(Nov)"; endif;
if jj==32; series="jobp(Dec)"; endif;
if jj==33; series="jobp(year)"; endif;
if jj==34; series="dutch(Jan)"; endif;
if jj==35; series="dutch(Feb)"; endif;
if jj==36; series="dutch(Mar)"; endif;
if jj==37; series="dutch(Apr)"; endif;
if jj==38; series="dutch(May)"; endif;
if jj==39; series="dutch(Jun)"; endif;
if jj==40; series="dutch(Jul)"; endif;
if jj==41; series="dutch(Aug)"; endif;
if jj==42; series="dutch(Sep)"; endif;
if jj==43; series="dutch(Oct)"; endif;
if jj==44; series="dutch(Nov)"; endif;
if jj==45; series="dutch(Dec)"; endif;

if jj<=5; yy=data[.,jj+1]; xx=data[.,1]; endif;
if jj==6; yy=data1[.,2]; xx=data1[.,1]; endif;
if jj==7; yy=longser; xx=seqa(1,1,rows(yy)); endif;
if jj==8; yy=data2[.,14]; xx=data2[.,1]; endif;
if jj>=9 and jj<=20; yy=data2[.,jj-7]; xx=data2[.,1];  endif;
if jj>=21 and jj<=33; yy=data3[.,jj-19]; xx=data3[.,1]; endif;
if jj>=34 and jj<=45; yy=data4[.,jj-32]; xx=data4[.,1]; endif;

y=selif(yy,yy.>-90); xx=selif(xx,yy.>-90);
if plotflag==1; x5=ones(rows(xx),1)~xx; xy(xx,y~(x5*(y/x5))); endif;

z=ps(y);
T=rows(y);

t1=seqa(1,1,T);
X1=ones(T,1)~t1;

/* Compute the partial sums of X1 */

nr=rows(X1);  nc=cols(X1);
X2=ps(X1);

/* Compute inverse matrices */
X1inv=invpd(X1'X1);
X2inv=invpd(X2'X2);

/* Compute orthonormal trends for the J statistics */
Xj=ones(nr,1)/sqrt(nr);
i=1;
do while i<=9;
  tt=t1^i;
  ehat=tt-Xj*invpd(Xj'Xj)*Xj'tt;
  ehat=ehat/sqrt(ehat'ehat);
  Xj=Xj~ehat;
  i=i+1;
endo;
Xjinv=invpd(Xj'Xj);

/* Compute stats in standard regressions */
bhat=X1inv*X1'y;
uhat=y-X1*bhat;
rss1=uhat'uhat;
vhat=uhat.*X1;
shat=ps(vhat);
mhat=X1inv*shat'shat*X1inv;
s2iid=rss1/(rows(X1)-cols(X1));
s2ols=sig2np(uhat,bigm,kernel,prewhite);
btild=X2inv*X2'z;
jbeta=Xjinv*Xj'y;
rssj=(y-Xj*jbeta)'(y-Xj*jbeta);
s2z=(z-X2*btild)'(z-X2*btild)/(rows(X2)-cols(X2));
J=(rss1-rssj)/rssj;

thac=(bhat[2]/sqrt(s2ols*X1inv[2,2]))*ones(1,3);
tstar=(sqrt(T)*bhat[2]/sqrt(mhat[2,2]))*exp(-btstar*J);
tps=(btild[2]/sqrt(T*s2z*X2inv[2,2]))*exp(-btps*J);
twt=(1.0/sqrt(T))*(bhat[2]/sqrt(s2iid*X1inv[2,2]))*ones(1,3);


/* now do ARMA(1,1)-GLS versions of the tests */

/* fit ARMA(1,1) model to OLS residuals */
ahat=0; that=0.0;
ydtr=uhat;
inc=0.00001;
if glsdum==1;  thattemp=-1+inc; thatmax=1.0-inc; endif;
if glsdum==0;  thattemp=0.0; thatmax=0.0000000001; endif;
minrss=10000000.0;
do while thattemp<=thatmax;
     {ehat,ahattemp}=arma1_1(ydtr,thattemp);
     rss=ehat'ehat;
     if rss<minrss;
        minrss=rss;
        ahat=ahattemp;
        that=thattemp;
     endif;
     thattemp=thattemp+inc;
endo;

/* now transform model using ARMA(1,1) and compute quasi-GLS stats */

y=armatran(ahat,that,y);
z=ps(y);
x1=armatran(ahat,that,x1); x1inv=invpd(x1'x1);
x2=ps(x1); x2inv=invpd(x2'x2);
bhatgls=x1inv*X1'y;
uhat=y-x1*bhatgls;
vhat=uhat.*x1;
shat=ps(vhat);
mhat=x1inv*shat'shat*x1inv;
s2ols=sig2np(uhat,bigm,kernel,prewhite);
btildgls=X2inv*X2'z;
s2z=(z-x2*btildgls)'(z-x2*btildgls)/(rows(x2)-cols(x2));

thacgls=(bhatgls[2]/sqrt(s2ols*x1inv[2,2]))*ones(1,3);
tstargls=(sqrt(T)*bhatgls[2]/sqrt(mhat[2,2]))*ones(1,3);
tpsgls=(btildgls[2]/sqrt(T*s2z*X2inv[2,2]))*ones(1,3);

output file = ^ofile1 on;
print series;
print "  " T~bhat[2]~thac[1]~twt[1]~tstar~btild[2]~tps~J;
output off;

output file = ^ofile2 on;
print series;
ttt=ahat~that~bhatgls[2]~thacgls[1]~tstargls[1]~btildgls[2]~tpsgls[1];
print "        " ttt;
output off;

jj=jj+1;
endo;

output file = ^ofile1 on;
print;
print "Critical values for test statistics.";
print;
print "        t-hac  tstar   t-ps    twt         J";
xxxx=(0.95|0.975|0.99)~cvthac'~cvtstar'~cvtps';
xxxx=xxxx~cvtwt'~(0.01|0.025|0.05)~Jcv';
print xxxx;
output off;

output file = ^ofile2 on;
print;
print "Critical values for test statistics.";
print;
print "        t-hac  tstar   t-ps";
print (0.95|0.975|0.99)~cvthac'~cvtstar'~cvtps';
output off;

end;



#include func.g;


proc (1) = ps(x);
    retp(recserar(x,x[1,.],ones(1,cols(x))));
endp;

proc (2) = arma1_1(y,theta);

  local ahat,yl,C1,C2;

  yl=0|y[1:rows(y)-1];
  C1=recserar(yl,0.0,-theta);
  C2=recserar(y,y[1],-theta);
  ahat=C2/C1;
  retp(recserar(y-yl*ahat,y[1],-theta),ahat);

endp;


/* This procedure computes the inverse var-cov matrix for an
   ARMA(1,1) using formulas in Tiao and Ali, Biometrika 1971.
   The ARMA model is y(t)=alpha*y(t-1)+e(t)+theta*e(t-1)  */

proc (1) = armatran(alpha,theta,z);

   local phi,psi,d,c,T,p,q,a,u,flag;

   phi=alpha;  psi=-theta;
   T=rows(z);
   flag=0;
   if abs(phi)==1.0;
     print "armatran doesn't work with alpha = 1 or -1!";
     flag=1;
     retp(z);
   endif;
   if psi==phi;
     print "roots of MA and AR components cancel, no tranformation done";
     flag=1;
     retp(z);
   endif;
   if flag==0;
     p=(1-phi^2)/(psi-phi)^2;
     q=1.0/(p+(1-(psi^2)^T)/(1-psi^2));
     d=recserar(zeros(T,1),1.0,psi);
     a=q*(1.0+sqrt(p*q))/(1.0-p*q);
     u=z-phi*(zeros(1,cols(z))|z[1:T-1,.]);
     c=recserar(u,u[1,.],psi*ones(1,cols(u)));
     retp(c-a*d*(d'c));
   endif;
endp;


/* This procedure computes the inverse var-cov matrix for an
   ARMA(1,1) using formulas in Tiao and Ali, Biometrika 1971 */

proc (1) = armaslow(alpha,theta,z);

   local pinv,Dainv,Dt,Dtinv,d,Ainv,flag,T;

   T=rows(z);
   theta=-theta;
   flag=0;
   if abs(theta)==1;
     print "alpha=1 does not work with armatran!";
     retp(eye(T));
     flag=1;
   endif;
   if alpha==theta;
     retp(eye(T));
     flag=1;
   endif;
   if flag==0;
     pinv=((theta-alpha)^2)/(1-alpha^2);
     Dt=_D(theta,T);
     Dtinv=inv(Dt);
     Dainv=inv(_D(alpha,T));
     d=Dtinv[.,1];

     Ainv=Dainv*Dt*(eye(T)+pinv*d*d')*Dt'*Dainv';
     retp(inv(chol(Ainv))'z);
   endif;

endp;

/* This procedure computes the D matrix needed to compute the
   ARMA(1,1) transformation matrix as in Tiao and Ali, Biometrika
   1971.  n is the dimension of D which is square and x is the
   argument that defines D  */

proc (1) = _D(x,n);

   retp(eye(n)+((zeros(1,n-1)|(-x*eye(n-1)))~zeros(n,1)));

endp;


