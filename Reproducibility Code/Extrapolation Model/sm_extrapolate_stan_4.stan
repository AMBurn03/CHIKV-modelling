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
  // matrix[N, P_cities*P_dates] X_citydate;
  vector[N] X_prop_male;
  vector[N] X_outbreak_sm;
  vector[N] X_latitude;
  vector[N] X_longitude;
  // sm
  array[N] int<lower=0> N_sm;
  array[N] int<lower=0> y_sm;
  vector[N] sensitivity;
  vector[N] specificity;
  // extrapolation
  matrix[N, 2] X_coords; // coordinates of places we have sero data for
  int<lower = 1> N_ext; // number of points we want to extrapolate
  matrix[N_ext, 2] Z_coords; // coordinates of places which we want to predict prevalence, average (x, y) of each municipality we want to investigate
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
}
parameters{
   // sm
  real beta0_sm; 
  real beta_outbreak_sm;
  // s2z sm effects
  vector[P_cities - 1 ] beta_cities_raw_sm; // Unconstrained city effects
  real<lower=0> beta_cities_sd_sm;       // City effect SD
  vector<lower=0, upper=1>[N] uninformed_specificity;
  // extrapolation
  real<lower=0> loc_x; // location scaling factor for x distance
  real<lower=0> loc_y; // location scaling factor for y distance
  real<lower=0> sigma_error;
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
  ( 1 - pi_sm ) .* ( 1 - specificity );
  
 for(i in 1:N){
   if( y_sm[i]< expected_false_pos[i] ){
     pi_adj_sm[i] = pi_sm[i]* sensitivity[i] +
  ( 1 - pi_sm[i] ) .* ( 1 - uninformed_specificity[i] );
     };
 }
   // extrapolation
  // train the location parameters from known data
  
  vector[N] pi_train = rep_vector(0,N);
  vector[N] error_bound;
  
  for(i in 1:N){
    vector[N] scaled_distances;
    vector[N] weights_train;
    
    // Compute scaled_distances to all other training points
    for(j in 1:N){
      if(i == j){
        scaled_distances[j] = positive_infinity(); // 100000000;
      } else {
        scaled_distances[j] = sqrt(
          square(X_coords[j,1] - X_coords[i,1]) * inv(loc_x) +
          square(X_coords[j,2] - X_coords[i,2]) * inv(loc_y)
        );
      }
    }
    
    // Compute unnormalized weights
    weights_train = exp(-scaled_distances);
    
    // Set self-weight to zero explicitly
    weights_train[i] = 0;
    
    // Normalize weights (only non-zero weights will matter)
    weights_train = weights_train / sum(weights_train);
    
    // Compute weighted average, vectorised so its faster
    pi_train[i] = dot_product(weights_train, pi_sm);
  }
  
  error_bound = pi_train - pi_sm; // keep a log of how far off we are !
}

model{
  // sm
   // likelihood
  target += binomial_lpmf(y_sm | N_sm, pi_adj_sm);
  beta0_sm ~ normal( 0 , 3.5 );
  beta_outbreak_sm ~ normal( 0 , 1.5 );
  // s2z sm specifications
  beta_cities_raw_sm ~ normal( 0 , cities_s2z_sd_sm );
  beta_cities_sd_sm ~ cauchy( 0 , 1);
  uninformed_specificity ~ uniform(0, 1);
  // extrapolation
  target+=normal_lpdf(error_bound| 0 , sigma_error);
  sigma_error ~ cauchy( 0 , 1 );
  loc_x ~ exponential(1);
  loc_y ~ exponential(1);

}

generated quantities{
  // sm 
  array[N] int<lower=0> y_pred_sm = binomial_rng( N_sm, pi_adj_sm );
  
  // extrapolation to all the other municipalities
  vector[N_ext] pi_ext = rep_vector(0, N_ext); // extrapolated prevalence values
  
  for(i in 1:N_ext){
    vector[N] scaled_distances;
    vector[N] weights;
    
    // Compute scaled_distances from extrapolation point to all training points
    for(j in 1:N){
      scaled_distances[j] = sqrt(
        square(X_coords[j,1] - Z_coords[i,1]) * inv(loc_x) +
        square(X_coords[j,2] - Z_coords[i,2]) * inv(loc_y)
      );
    }
    
    // Compute unnormalized weights
    weights = exp(-scaled_distances);
    
    // Normalize weights to sum to 1
    weights = weights / sum(weights);
    
    // Compute weighted average
    pi_ext[i] = dot_product(weights, pi_sm);
  }
}
