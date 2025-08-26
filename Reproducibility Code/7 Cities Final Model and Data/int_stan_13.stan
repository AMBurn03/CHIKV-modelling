// Prior predictive simulation version of the Stan model
// Samples from priors and generates predicted data without conditioning on observed data
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
  // shared data
  int<lower=1> N;
  int<lower=1> P_cities;
  int<lower=1> P_age;
  matrix[N,P_cities] X_cities;
  matrix[N, 4] X_datesex;
  matrix[N, P_cities * 2] X_citydate;
  matrix[N, P_age] X_age;
  vector[N] X_outbreak;
  vector[N] population;   //population
  vector[N] X_sex;
  vector[N] X_date;
  // serology model specific data (_sm suffix)
  array[N] int<lower=0> N_sm;
  array[N] int<lower=0> y_sm;
  real<lower=0, upper=1> sensitivity_sm;
  real<lower=0, upper=1> specificity_sm;
  // reported cases model specfic data (_rp suffix)
  array[N] int<lower=0> y_rp;
  vector[P_cities] outbreak_city;
}

transformed data{
  int<lower=1> P_citydate = P_cities * 2;
  // Precompute scaling factors for sum-to-zero effects (sm)
  int P_datesex = 4;
  // sm
  real cities_s2z_sd_sm = inv_sqrt(1.0 - inv(P_cities));
  real age_s2z_sd_sm = inv_sqrt(1.0 - inv(P_age));
  real ds_s2z_sd_sm = inv_sqrt(1.0 - inv(P_datesex)); // For date-sex (ds) interaction
  real cd_s2z_sd_sm = inv_sqrt(1.0 - inv(P_citydate)); // For date-city (cd) interaction
}

parameters{
  real beta0_sm;                        // Intercept
  real beta_sex_sm;                     // Sex effect
  real<lower=0> beta_date_sm;           // Date effect
  vector[P_cities - 1] beta_cities_raw_sm; // Unconstrained city effects
  vector[P_citydate - 1] beta_cd_raw_sm;  // Unconstrained city-date interaction effects
  real<lower=0> beta_cities_sd_sm;       // City effect SD
  real<lower=0> beta_cd_sd_sm;          // SD for city:date effects
  
  vector[P_age - 1] rho_age_raw_rp;
  vector[P_cities - 1] rho_city_raw_rp; // Unconstrained r.f. on reporting rate
  real<lower=0> rho_city_sd_rp;       // Random effect SD
  real<lower=0> rho_age_sd_rp;
  real rho_0_rp; // intercept for rho
  real<lower=0> phi_rp;  // Negative Binomial dispersion parameter
}

transformed parameters {
  // sm
  // Compute sum-to-zero effects
  vector[P_cities] beta_cities_sm = sum2zero_effect(
    P_cities, beta_cities_raw_sm, beta_cities_sd_sm
    );
  vector[P_citydate] beta_cd_sm = sum2zero_effect(
    P_citydate, beta_cd_raw_sm, beta_cd_sd_sm);
    
  // Adjusted probabilities
  vector[N] p_logit_sm = beta0_sm +
    X_cities * beta_cities_sm + 
    X_sex * beta_sex_sm+
    X_date * beta_date_sm+
    X_citydate * beta_cd_sm;
  vector[N] p_adj_sm = inv_logit(p_logit_sm) * sensitivity_sm + (1 - inv_logit(p_logit_sm)) * (1 - specificity_sm);
  
  // rp
  vector[P_cities] rho_city_rp = sum2zero_effect(
    P_cities, rho_city_raw_rp, rho_city_sd_rp);
  vector[P_age] rho_age_rp = sum2zero_effect(
    P_age, rho_age_raw_rp, rho_age_sd_rp);
    
  vector[N] log_mu_rp; 
  vector[N] logit_rho_rp;
  logit_rho_rp = rho_0_rp + X_cities * rho_city_rp +
    X_age * rho_age_rp;
  
  // mu parameter
  log_mu_rp = log(inv_logit(logit_rho_rp)) + log(p_adj_sm) + log(population);
}

model {
  y_sm ~ binomial(N_sm, p_adj_sm);
  // sm priors
  beta0_sm ~ normal(0, 3.5);
  beta_sex_sm ~ normal(0, 1.5);
  beta_date_sm ~ normal(0, 1.5);
  beta_cities_raw_sm ~ normal(0, cities_s2z_sd_sm);
  beta_cd_raw_sm ~ normal(0, cd_s2z_sd_sm);
  beta_cities_sd_sm ~ cauchy(0, 1);
  beta_cd_sd_sm ~ cauchy(0, 1);
  
  //rp priors
  y_rp ~ neg_binomial_2_log(log_mu_rp, phi_rp);
  phi_rp ~ inv_gamma(0.04, 0.03);
  rho_0_rp ~ normal(0, 3.5);
  rho_age_raw_rp ~ normal(0 , age_s2z_sd_sm);
  rho_age_sd_rp ~ cauchy( 0 , 1 );
  rho_city_raw_rp ~ normal(0, cities_s2z_sd_sm);
  rho_city_sd_rp ~ cauchy(0, 1);
}

generated quantities{
  // sm: simulated serology data
  array[N] int<lower=0> y_pred_adj_sm = binomial_rng(N_sm, p_adj_sm);
    array[N] int<lower=0> y_pred_naive_sm = binomial_rng(N_sm, inv_logit(p_logit_sm));

  // rp: simulated reported cases
  array[N] int<lower=0> y_pred_rp = neg_binomial_2_log_rng(log_mu_rp, phi_rp);
  
  vector<lower=0, upper=1>[N] naive_seroprevalence = inv_logit(p_logit_sm);
  vector<lower=0, upper=1>[N] rho_rp = inv_logit(logit_rho_rp);
}
