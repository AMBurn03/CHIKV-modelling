functions {
  // Generates sum-to-zero constraint matrix
  matrix sum2zero_generating_matrix(int K) {
    matrix[K, K] A = diag_matrix(rep_vector(1, K));
    for (i in 1:(K - 1)) A[K, i] = -1;
    A[K, K] = 0;
    return qr_Q(A)[, 1:(K - 1)];
  }
  // Computes sum-to-zero effects for a given category
  vector sum2zero_effect(
    int K,                  // Number of levels
    vector raw_effects,      // Unconstrained effects (K-1)
    real sigma              // Standard deviation
    ) {
      matrix[K, K - 1] Q = sum2zero_generating_matrix(K);
      vector[K] effect = Q * raw_effects;  // Constrained effects
      return sigma * effect;               // Scaled by hyperparameter
    }
}
data{
  int<lower=1> N;
  int<lower=1> P_cities;
  int<lower=1> P_dates;
  matrix[N, P_cities] X_cities;
  matrix[N, P_dates] X_dates;
  matrix[N, P_cities*P_dates] X_citydate;
  vector[N] X_prop_male;
  vector[N] X_outbreak_sm;
  vector[N] X_latitude;
  vector[N] X_longitude;
  // sm
  array[N] int<lower=0> N_sm;
  array[N] int<lower=0> y_sm;
  vector[N] sensitivity;
  vector[N] specificity;
  // rp
  array[N] int<lower=0> y_rp;
  vector[N] population;
  
}
transformed data{
  // Precompute scaling factors for sum-to-zero effects (sm)
  real cities_s2z_sd_sm = inv_sqrt(1.0 - inv(P_cities)); 
  real dates_s2z_sd_sm = inv_sqrt(1.0 - inv(P_dates)); // For date effect
  real dpm_s2z_sd_sm = inv_sqrt(1.0 - inv(P_dates)); // For date:prop-male interact
  // Precompute scaling factors for sum-to-zero effects (rp)
  real cities_s2z_sd_rp = inv_sqrt(1.0 - inv(P_cities)); 
  real dates_s2z_sd_rp = inv_sqrt(1.0 - inv(P_dates));
 vector[N] expected_false_pos = to_vector(N_sm) .* (1 - specificity);
  vector[N] FPR = (1-specificity);
}
parameters{
   // sm
  real beta0_sm; 
  real beta_outbreak_sm;
  // s2z sm effects
  vector[P_cities - 1 ] beta_cities_raw_sm; // Unconstrained city effects
  // vector[P_dates ] beta_dates_sm; // Unconstrained date effects
  real<lower=0> beta_cities_sd_sm;       // City effect SD
  // real<lower=0> beta_dates_sd_sm;
   // rp
   vector<lower=0, upper=1>[N] rho_rp;
   // vector[N] logit_rho_rp;
   // real<lower=0> phi_rp;
   vector<lower=0, upper=1>[N] uninformed_specificity;
}

transformed parameters{
  //sm
 vector[P_cities] beta_cities_sm = sum2zero_effect(
   P_cities, beta_cities_raw_sm, beta_cities_sd_sm
 );
  vector[N] pi_sm;
  pi_sm = inv_logit(
    beta0_sm +
  X_cities * beta_cities_sm+
  X_outbreak_sm * beta_outbreak_sm
    );
  vector[N] pi_adj_sm = pi_sm .* sensitivity +
  ( 1 - pi_sm ) .* ( 1 - uninformed_specificity );
  
  // rp
  vector[N] log_mu_rp; /// logarise this!
  log_mu_rp = log(rho_rp) + log(pi_adj_sm) + log(population);
}

model{
  // sm
   // likelihood
  target += binomial_lpmf(y_sm | N_sm, pi_adj_sm);
  beta0_sm ~ normal( 0 , 3.5 );
  // beta_prop_male_sm ~ normal( 0 , 1.5 );
  beta_outbreak_sm ~ normal( 0 , 1.5 );
  // s2z sm specifications
  beta_cities_raw_sm ~ normal( 0 , cities_s2z_sd_sm );
  // beta_dates_sm ~ normal( 0 , beta_dates_sd_sm );
  beta_cities_sd_sm ~ normal(0, 0.04);
  uninformed_specificity ~ uniform(0, 1);
  // beta_dates_sd_sm ~ cauchy( 0 , 1 );
  // rp
  // y_rp ~ neg_binomial_2_log( log_mu_rp , inv(phi_rp) );
  y_rp ~ poisson_log(log_mu_rp);
  // phi_rp ~ exponential( 1 );
  rho_rp ~ beta( 0.5, 0.5 );
}
generated quantities{
  // sm 
  array[N] int<lower=0> y_pred_sm = binomial_rng( N_sm, pi_adj_sm );
  // array[N] int<lower=0> y_pred_naive_sm = binomial_rng( N_sm, pi_naive_sm );
  // array[N] int<lower=0> false_pos_count_sm = binomial_rng( N_sm, 1-specificity);
  // rp 
  // array[N] int<lower=0> y_pred_rp = neg_binomial_2_log_rng( log_mu_rp, inv(phi_rp) );
  array[N] int<lower=0> y_pred_rp = poisson_log_rng( log_mu_rp );
  // vector[N] BetaCity = X_cities*beta_cities_sm;
  // real log_post_SaoCarlos = binomial_lpmf(y_sm[33] | N_sm[33], pi_adj_sm[33])+
  // inv_logit(normal_lpdf(beta0_sm| 0, 3.5 ) + normal_lpdf(BetaCity[33]| 0 , beta_cities_sd_sm));
  // real log_prior_SaoCarlos = inv_logit(normal_lpdf(beta0_sm| 0, 3.5 ) + normal_lpdf(BetaCity[33]| 0 , beta_cities_sd_sm));
  }
