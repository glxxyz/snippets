/* This procedure computes the wald test for testing whether the slope
   coefficient of a simple linear trend is zero.  The lag length is
   chosen using the data dependent method suggested by Perron and Vogelsang
   JBES 1992.  The procedure has as inputs:

   y ...  the data
   kmax ... the maximal lag length
   siglevel ... the significance level of the test to determine lag length
                e.g. 0.05

   the procedure returns 3 scalars:

   stat ... the value of the t-stat on the trend slope coefficient
   adf  ... the value of the ADF unit root statistic
   s2   ... the parametric estimate of the spectral density of the
            noise left after taking out the largest AR root
*/

proc(7)=olslags(y,kmax,cvchi2);

local cv,T,k,kflag,x,tt,dy,adf,xxinv,beta,res,s1,i,tstats,stat,s2,se,bkeep;
local sekeep,alpha,kkeep;

  cv=cvchi2;
  T=rows(y); tt=ones(T,1)~seqa(1,1,T);
  k=kmax;  kflag=1;
  do while k>=1 and kflag==1;
     x=tt[k+1:T,.];
     dy=y[k+1:T]-y[k:T-1];
     x=x~y[k:T-1];
     i=2;
     do while i<=k;
       x=x~(y[k+2-i:T-i+1]-y[k-i+1:T-i]);
       i=i+1;
     endo;
     xxinv=invpd(x'x);
     beta=xxinv*x'dy;
     res=dy-x*beta;
     s1=res'res/(rows(x)-cols(x));
     se=sqrt(diag(xxinv)*s1);
     tstats=beta./se;
     if k>1;
       if tstats[cols(x)]^2.0>cv;
         stat=tstats[2]^2.0;
         adf=tstats[3];
         s2=s1/((1.0-sumc(beta[4:rows(beta)]))^2);
     bkeep=beta[2];
         sekeep=se[2];
         alpha=beta[3]+1.0;
         kkeep=k;
         kflag=0;
       endif;
     endif;
     if k==1;
       stat=tstats[2]^2.0;
       adf=tstats[3];
       bkeep=beta[2];
       sekeep=se[2];
       alpha=beta[3]+1.0;
       kkeep=k;
       s2=s1;
     endif;

     k=k-1;
  endo;

  retp(stat,adf,s2,bkeep,sekeep,alpha,kkeep);

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

if kernel<0 or kernel>4;
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

local y;


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

retp(y);

endp;



/* This procedure returns the upper end of the critical value for the
   largest autoregressive root for a two-tailed 95% confidence set
   or a one tailed 97.5% confidence set as suggested by Stock(1991)
   8/13/91 */

proc(1) = stock95(adf);

local y,x,c,x1,x2,y1,y2,ii,s;

y={37.94,36.2,34.42,32.7,31,29.25,27.55,25.86,24.24,22.62,21.02,19.44,
   17.91,16.34,14.87,13.31,11.73,10.25,8.81,7.17,5.57,3.6,-1.42};
y=-y;

x=seqa(-5.9,0.1,23);

if adf<=-5.9; c=-37.94; endif;
if adf>=-3.7283; c=0.0; endif;
if adf < -3.7283 and adf > -5.9;
  ii=(x.<=adf).*(x.>(adf-0.1));
  x1=ii'x;  y1=ii'y;  x2=x1+0.1;  y2=y[ii'seqa(1,1,23)+1];
  s=(y2-y1)/(x2-x1);
  c=s*(adf-x1)+y1;
endif;
retp(c);

endp;


/* This procedure returns the upper end of the critical value for the
   largest autoregressive root for a two-tailed 90% confidence set
   or a one tailed 95% confidence set as suggested by Stock(1991)
   8/13/91 */

proc(1) = stock90(adf);

local y,x,c,x1,x2,y1,y2,ii,s;

y={36.3,34.56,32.8,31.08,29.36,27.72,26.07,24.53,22.95,21.38,19.81,18.29,
   16.75,15.22,13.75,12.26,10.81,9.29,7.79,6.31,4.65,2.85,-1.49};
y=-y;

x=seqa(-5.6,0.1,23);

if adf<=-5.6; c=-36.30; endif;
if adf>=-3.4343; c=0.0; endif;
if adf < -3.4343 and adf > -5.6;
  ii=(x.<=adf).*(x.>(adf-0.1));
  x1=ii'x;  y1=ii'y;  x2=x1+0.1;  y2=y[ii'seqa(1,1,23)+1];
  s=(y2-y1)/(x2-x1);
  c=s*(adf-x1)+y1;
endif;
retp(c);

endp;


/* fracdgp.g  This program simulates a fractionally integrated time
   series.

   Inputs:

    d = the order of integration, must pick -0.5<=d<1.5
    T = length of the time series
 seed = the seed for the random number generators
ireset :  set ireset = 1 for the first call of fracdgp for given
          values of d,T
    g = can be any matrix or number as long as g is defined before
        the first call of fracdgp.

*/



proc(4)=fracdgp(d,T,seed,ireset,g);

local q,i,j,intdum,u,v,y,z,lseed;

  lseed=seed;

  if d<-0.5 or d>=1.5;
    print "d is outside the range -0.5 to 1.499999";
  endif;

  intdum=0;
  if d>0.49999999; d=d-1.0;  intdum=1; endif;

  if ireset==1;
    ireset=0;
    g=zeros(T,1);
q=exp(2.0*ln(1.0-d)+ln(gamma(2.0-2.0*d))-ln(1.0-2.0*d)-2.0*ln(gamma(2.0-d)));
    g[1]=q;
    i=2;
    do while i<=T;
      g[i]=g[i-1]*(d+i-2.0)/(i-1.0-d);
      i=i+1;
    endo;
    j=1; i=T+1;
    g=g|(rev(g[2:T-1]));
 /*   do while i<=2*T-2;
      g[i]=g[i-2*j];
      j=j+1;  i=i+1;
    endo;  */
    g=rows(g)*real(fft(g));
    if g<0.0;  print "You have negative g's in fracdgp!";  endif;
  endif;
  u=rndns(T,1,lseed);
  v=rndns(T,1,lseed);
  u[1]=sqrt(2.0)*u[1];  u[T]=sqrt(2.0)*u[T];
  v[1]=0.0;  v[T]=0.0;
  u=u|rev(u[2:T-1]);  v=v|(-rev(v[2:T-1]));
  z=sqrt(g).*complex(u,v);
  z=rows(real(z))*fft(z);
  y=0.5*real(z[1:T])/sqrt(T-1.0);
  if intdum==1;  y=recserar(y,y[1],1.0);  endif;
  retp(y,lseed,ireset,g);

endp;




