// Stata re-implementation of Vogelsang's trend.g (written in Gauss) by Alan Davies,
// comments below from the original trend.g

/* trend.g   This program computes individual tests using the t-PS test of Vogelsang (1998, Econometrica)
and the t-hac (Dan, b=0.02) test as implemented by Bunzel and Vogelsang (2003)

Written by Tim Vogelsang */

capture end
clear all
set more off

/* load your data here */

/* yourdata.prn is the name of your ascii */
/* data file. Replace the 100 with the */
/* number of observations in your series */
insheet y using data0.txt
mata
y=st_data(., ("y"))

real matrix ps(x)
{
    partialsum = x
    for (i=2; i<= rows(x); i++)
    {
        partialsum[i,] = partialsum[i,] + partialsum[i-1,]
    }
    return(partialsum)
}

real matrix seqa(start,inc,n)
{
    sequence = J(n, 1, 0)
    sequence[1] = start
    for (i=2; i<= n; i++)
    {
        sequence[i,] = inc + sequence[i-1,]
    }
    return(sequence)
}

real matrix ones(r, c) return(J(r, c, 1))
real matrix invpd(x) return(invsym(x))

real matrix raiseall(x, exponent)
{
    raised = x
    n = rows(x)
    for (i=1; i<=n; i++)
    {
        raised[i] = x[i]^exponent
    }
    return(raised)
}

/* critical values for right tailed 5% 2.5% and 1% test and one-sided
   confidence intervals */
cvtps = (1.720, 2.152, 2.647);
btps = (0.716, 0.995, 1.501);
Jcv = (0.488, 0.678, 0.908);
z=ps(y);
T=rows(y);

t1=seqa(1,1,T);
X1=(ones(T,1), t1)

/* Compute the partial sums of X1 */
nr=rows(X1);  nc=cols(X1);
X2=ps(X1);

/* Compute inverse matrices */
X1inv=invpd(X1'X1);
X2inv=invpd(X2'X2);

/* Compute orthonormal trends for the J statistics */
Xj=ones(nr,1)/sqrt(nr);
i=1;
while (i <= 9) {
   tt = raiseall(t1, i)
   ehat=tt-Xj*invpd(Xj'Xj)*Xj'tt;
   ehat=ehat/sqrt(ehat'ehat);
   Xj=(Xj,ehat);
   i=i+1;
}
Xjinv=invpd(Xj'Xj);

/* Compute stats in standard regressions */
bhat=X1inv*X1'y;
uhat=y-X1*bhat;
rss1=uhat'uhat;
btild=X2inv*X2'z;
jbeta=Xjinv*Xj'y;
rssj=(y-Xj*jbeta)'(y-Xj*jbeta);
s2z=(z-X2*btild)'(z-X2*btild)/(rows(X2)-cols(X2));
J=(rss1-rssj)/rssj;
tps=(btild[2]/sqrt(T*s2z*X2inv[2,2]))*exp(-btps*J);
end

display
display "Table of Empirical Results for Trend Test"
display "t-PS is the test of Vogelsang (1998)"
display
display "                                    t-PS             J"
display "    T      bhat     btild    5%     2.5%     1%"
mata
T
bhat[2]
btild[2]
tps
J
end

display
display "Critical values for test statistics."
display
display "         t-ps              J"
display "0.95|0.975|0.99   0.01|0.025|0.05"
mata
cvtps
Jcv
end