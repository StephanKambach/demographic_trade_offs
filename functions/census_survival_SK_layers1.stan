data {
  int<lower=0>            n_sp;              // number of species in the CTFS Plot
  int<lower=0>            n_obs;             // total number of observations
  int<lower=1>            sp[n_obs];         // assigns the respective observation to a certain species
  real<lower=0>           tinterval[n_obs];  // time interval in years (t2-t1)/365.25
  int<lower=0,upper=1>    alive[n_obs];      // binary variable alive/dead
}

parameters {
  
    real<lower=0,upper=1> surv[n_sp];
  	
    real<lower=0> alpha;
    real<lower=0> beta;
}	

model {
  real s;
  
  for(i in 1:n_obs){
    s=pow(surv[sp[i]],tinterval[i]);
	  target+= bernoulli_lpmf(alive[i]|s);
  }	
	
  // priors
  surv ~ beta(alpha, beta);
}
