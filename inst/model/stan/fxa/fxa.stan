data{
  int<lower = 1> nSubjects;
  int<lower = 1> nObs;
  int<lower = 1> subject[nObs];
  real<lower = 0> cObs[nObs];
  vector[nObs] fxa;

  int<lower = 1> nsim;
  real<lower = 0> cmin;
  real<lower = 0> cmax;
}

parameters{
  real<lower = 0, upper = 100> emax;
  real<lower = 0> ec50Hat;
  real<lower = 0> gamma;
  real<lower = 0> sigma;
  real<lower = 0> omegaEc50;
  vector[nSubjects] eta;
}

transformed parameters{
  vector<lower = 0>[nSubjects] ec50;
  vector<lower = 0>[nObs] fxaHat;

  ec50 = ec50Hat * exp(omegaEc50 * eta);
  for(i in 1:nObs){
    fxaHat[i] = emax * cObs[i]^gamma / (ec50[subject[i]]^gamma + cObs[i]^gamma);
  }

}

model{  
  emax ~ uniform(0, 100);
  ec50Hat ~ normal(0, 250);
  gamma ~ normal(0, 5);
  sigma ~ cauchy(0, 10);
  omegaEc50 ~ cauchy(0, 1);

  eta ~ normal(0, 1);

  fxa ~ normal(fxaHat, sigma); 
}

generated quantities{
  vector[nSubjects] logEc50Pred;
  vector<lower = 0>[nSubjects] ec50Pred;
  real fxaCond[nObs];
  real<lower = 0> fxaHatPred[nObs];
  real fxaPred[nObs];
  vector[nsim] logEc50Pred2;
  vector<lower = 0>[nsim] ec50Pred2;
  real<lower = 0> fxaHatPred2[nsim];
  real fxaPred2[nsim];
  real csim[nsim];
  real log_lik[nObs];

  // Individual predictions
  for(i in 1:nObs){
    fxaCond[i] = normal_rng(fxaHat[i], sigma);
    // Calculate log-probability for model comparison stats
    log_lik[i] = normal_lpdf(fxa[i] | fxaHat[i], sigma); 
  }

  // Population predictions
  for(i in 1:nSubjects){
    logEc50Pred[i] = normal_rng(log(ec50Hat), omegaEc50);
  }
  ec50Pred = exp(logEc50Pred);
  for(i in 1:nObs){
    fxaHatPred[i] = emax * cObs[i]^gamma / (ec50Pred[subject[i]]^gamma + cObs[i]^gamma);
    fxaPred[i] = normal_rng(fxaHatPred[i], sigma);
  }

  // simulation to show posterior predictive distribution as a function of concentration
  for(i in 1:nsim){
    csim[i] = cmin + (i - 1) * (cmax - cmin) / (nsim - 1);
    logEc50Pred2[i]  = normal_rng(log(ec50Hat), omegaEc50);
    ec50Pred2[i] = exp(logEc50Pred2[i]);
    fxaHatPred2[i] = emax * csim[i]^gamma / (ec50Pred2[subject[i]]^gamma + csim[i]^gamma);
    fxaPred2[i] = normal_rng(fxaHatPred2[i], sigma);
  }

}
