// gng_m3.stan - placeholder Stan model header
// See README.md for model description and usage notes
data {
  int<lower=1> N_subj;
  int<lower=1> N_trials;
  int<lower=0,upper=1> choice[N_subj, N_trials];
  // Add data block fields as needed
}
parameters {
  real<lower=0> xi[N_subj]; // inverse temperature per subject
  real<lower=0,upper=1> ep[N_subj]; // learning rate per subject
}
model {
  // Placeholder priors and likelihood
  for (s in 1:N_subj) {
    xi[s] ~ normal(1, 1);
    ep[s] ~ beta(1, 1);
  }
}
