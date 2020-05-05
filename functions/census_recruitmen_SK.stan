data {
  int  <lower=1>            Nobs;            // # of observations
  int  <lower=1>            Nsp;             // # of species

  int  <lower=1>            sp[Nobs];        // species for each observation
  int <lower=0>            obs[Nobs];       // number of recruits per individual
}
parameters {
  real<lower=0>             k[Nsp];
//  real              mu[Nsp];
//  real <lower=0>            sigma[Nsp];
}
model{
  
  for(i in 1:Nobs){
    obs[i] ~ poisson(k[sp[i]]);
//    obs[i] ~ lognormal(mu[sp[i]], sigma[sp[i]]);
  }
  
}
