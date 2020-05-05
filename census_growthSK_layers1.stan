data {
  int n_obs;                                // total number of observations
  int<lower=0> n_sp;                        // number of species in the CTFS Plot
  int<lower=1> sp[n_obs];                   // assigns the respective observation to a certain species
  real dinc[n_obs];                         // log growth rate per ba of adult individuals
  real mean_growth_lb;              		// median sd per species per layer
  real mean_growth_ub;              		// median sd per species per layer
  real sigma_of_mean_growth_lb;             // median sd per species per layer
  real sigma_of_mean_growth_ub;             // median sd per species per layer
  real log_mean_sigma_lb;              		// median sd per species per layer
  real log_mean_sigma_ub;              		// median sd per species per layer
  real sigma_of_mean_sigma_lb;              // median sd per species per layer
  real sigma_of_mean_sigma_ub;              // median sd per species per layer
  real zero_border;              			// lowest value for species level growth rates allowed
}

parameters {
  real<lower = zero_border> sp_growth[n_sp];
  real<lower = 0> sp_sigma[n_sp];
  real<lower = mean_growth_lb, upper = mean_growth_ub> mean_growth; // mean of community hyperdistribution
  real<lower = sigma_of_mean_growth_lb, upper = sigma_of_mean_growth_ub> sigma_of_mean_growth; // mean of community hyperdistribution
  real<lower = log_mean_sigma_lb, upper = log_mean_sigma_ub> log_mean_sigma;
  real<lower = sigma_of_mean_sigma_lb, upper = sigma_of_mean_sigma_ub> sigma_of_mean_sigma;
}

model {

// likelihood
  for(i in 1:n_obs){
    target +=	normal_lpdf(dinc[i]|sp_growth[sp[i]], sp_sigma[sp[i]]);
  }

// priors
  target += normal_lpdf(sp_growth | mean_growth, sigma_of_mean_growth);
  target += lognormal_lpdf(sp_sigma | log_mean_sigma, sigma_of_mean_sigma);
}
