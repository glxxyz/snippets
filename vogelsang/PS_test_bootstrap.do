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
use "vwvol1921-2011.dta", clear
sort ym

mata

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

maxval = 1092;
minlength = 36;

results5  = J((maxval-minlength)/12+1,(maxval-minlength)/12+1,0)
results25 = J((maxval-minlength)/12+1,(maxval-minlength)/12+1,0)
results1  = J((maxval-minlength)/12+1,(maxval-minlength)/12+1,0)

for (from=1; from<=(maxval-minlength)/12+1; from++)
{
    for (to=from; to<=(maxval-minlength)/12+1; to++)
    {
        frommonth =(from-1)*12+1
        tomonth= (to-1)*12+minlength
        y=st_data((frommonth, tomonth), ("firm"))
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

        results5[from, to] = tps[1];
        results25[from, to] = tps[2];
        results1[from, to] = tps[3];
    }
}

end

clear
gen from=0
gen to=0
gen ps5=0.0
gen ps25=0.0
gen ps1=0.0
set obs 100000

mata
obs=1
for (from=1; from<=(maxval-minlength)/12+1; from++)
{
    for (to=from; to<=(maxval-minlength)/12+1; to++)
    {
        st_store(obs, 1, from+1920)
        st_store(obs, 2, to+1922)
        st_store(obs, 3, results5[from, to]);
        st_store(obs, 4, results25[from, to]);
        st_store(obs, 5, results1[from, to]);
        obs = obs + 1;
    }
}
end

drop if from == .

gen length = to - from

gen sig=4
replace sig = 1 if ps1 < -2.647 & sig == 4
replace sig = 2 if ps25 < -2.152 & sig == 4
replace sig = 3 if ps5 < -1.720 & sig == 4
replace sig = 7 if ps1 > 2.647 & sig == 4
replace sig = 6 if ps25 > 2.152 & sig == 4
replace sig = 5 if ps5 > 1.720 & sig == 4

save PSTestBootstrap

use PSTestBootstrap, clear
twoway (contour sig to from, ccuts(0 1 2 3 4 5 6 7) interp(none)), ylabel(1921(10)2011) xlabel(1921(10)2011)
