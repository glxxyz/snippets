/* trend.g   This program computes individual tests using the t-PS test of Vogelsang (1998, Econometrica)
and the t-hac (Dan, b=0.02) test as implemented by Bunzel and Vogelsang (2003)

Written by Tim Vogelsang */

#lineson;
new;



/* load your data here */

load y[100,1]=yourdata.prn;  /* yourdata.prn is the name of your ascii */
                             /* data file. Replace the 100 with the */
                             /* number of observations in your series */  

prewhite=0; /* set to 1 to use prewhitening for the t-dan test */

/* critical values for right tailed 5% 2.5% and 1% test and one-sided
   confidence intervals */

cvtps=1.720~2.152~2.647;  btps=0.716~0.995~1.501;
cvtdan=1.710~2.052~2.462; btdan=1.32227~1.79541~2.46582;
Jcv=0.488~0.678~0.908;


format /ld 6,4;
output file = trend.out on;
print; 
print "Table of Empirical Results for Trend Tests";
print "t-PS is the test of Vogelsang (1998) and t-Dan is one of";
print "tests recommended by Bunzel and Vogelsang (2003)";
if prewhite==1; print "AR(1) prewhitening used for t-dan"; endif;
print;
print "                       tdan-J b=0.02"\
"                       t-PS             J";
print "    T      bhat       5%     2.5%     1%    "\
"btild     5%     2.5%     1%";
output off;


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
s2dan=sig2np(uhat,maxc(trunc(0.02*T)|2),5,0);
btild=X2inv*X2'z;
jbeta=Xjinv*Xj'y;
rssj=(y-Xj*jbeta)'(y-Xj*jbeta);
s2z=(z-X2*btild)'(z-X2*btild)/(rows(X2)-cols(X2));
J=(rss1-rssj)/rssj;

tdan=(bhat[2]/sqrt(s2dan*X1inv[2,2]))*exp(-btdan*J);
tps=(btild[2]/sqrt(T*s2z*X2inv[2,2]))*exp(-btps*J);



output file = trend.out on;
print T~bhat[2]~tdan~btild[2]~tps~J;
print;
print;
print "Critical values for test statistics.";
print;
print "          tdan   t-ps              J";
print (0.95|0.975|0.99)~cvtdan'~cvtps'~(0.01|0.025|0.05)~Jcv';
output off;

end;



proc (1) = ps(x);
    retp(recserar(x,x[1,.],ones(1,cols(x))));
endp;


/* This procedure computes the nonparametric estimator
   of the long run variance using Andrews (1991) AR(1)
   plug in optimal truncation lag.  v is the vector of
   residuals and M is the trucation lag.  M<0 uses the
   automatic bandwidth.  Kernel is an integer representing
   the kernel that is used
   1 = bartlett, 2 = parzen, 3 = Tukey-Hanning, 4 = quadratic spectral

   if prewhite == 1 then prewhitening using an AR(1) model is used
   if prewhite == 0 no prewhitening is used.

   8/13/96 */


proc(1) = sig2np(v,M,kernel,prewhite);

local i,R,R0,T,ST,sigma2,rho,rho2,a1,a2;

T=rows(v);

if kernel<0 or kernel>5;
  kernel=4;
  print "using the Quadratic spectral kernel";
endif;

R=zeros(T-1,1);
R0=v[1:T-1]'v[1:T-1];
R[1]=v[2:T]'v[1:T-1];
rho=R[1]/R0;
if prewhite == 1;
  rho2=rho;
  v=0|(v[2:T]-rho*v[1:T-1]);
  R0=v[1:T-1]'v[1:T-1];
  R[1]=v[2:T]'v[1:T-1];
  rho=R[1]/R0;
endif;

if M<0.0;
  a1=4.0*rho*rho/(1.0-rho*rho)^2;
  a2=4.0*rho*rho/(1.0-rho)^4;
  if kernel == 1; ST=1.1447*(T*a1)^(1.0/3.0); endif;
  if kernel == 2; ST=2.6614*(T*a2)^(0.2); endif;
  if kernel == 3; ST=1.7462*(T*a2)^(0.2); endif;
  if kernel == 4; ST=1.3221*(T*a2)^(0.2); endif;
  if kernel == 5; ST=1; endif;
endif;
if M>=0.0;
  ST=M;
endif;
R0=v'v/T;
i=1;
do while i<=T-1;
  R[i]=v[i+1:T]'v[1:T-i]/T;
  i=i+1;
endo;
sigma2=R0+2.0*_K(seqa(1,1,T-1)/ST,kernel)'R;

if prewhite/=1; retp(sigma2); endif;
if prewhite==1; retp(sigma2/(1.0-rho2)^2); endif;

endp;



/* Kernels */

proc(1) = _K(x,kernel);

local y,xx;

x=abs(x);
xx=((x.>0).*x)+(x.<0.0000001);


if kernel==1; /*bartlett*/
  y=(x.<=1.0).*(1.0-x);
endif;
if kernel==2; /*parzen*/
  y=(x.>=0.0).*(x.<=0.5).*(1.0-6.0*x.*x.*(1.0-x));
  y=y+(x.>0.5).*(x.<=1.0)*2.0.*(1.0-x).^3;
endif;
if kernel==3; /*Tukey-hanning*/
  y=(x.<=1.0)*0.5.*(1.0+cos(pi*x));
endif;
if kernel==4; /*quadratic spectral */
y=(x.>0).*(25.0./(12.0*pi^2*x.*x)).*((sin(1.2*pi*x)./(1.2*pi*x))-cos(1.2*pi*x));
  y=y+(x.==0.0);
endif;

if kernel==5; /* Daniells */
  y=((x.>0).*sin(pi*xx)./(pi*xx))+(abs(x).<0.00000001);
endif;

retp(y);

endp;


