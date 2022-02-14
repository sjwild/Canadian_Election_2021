 data {

  // Ns
  int<lower=1> N_days;                     // number of days
  int<lower=1> N_parties;                  // number of parties
  int<lower=1> N_polls;                    // number of polls
  int<lower=1> N_modes;                    // number of survey modes
  int<lower=1> N_pollsters;                // number of pollsters
  
  // election results
  real xi_start[N_parties];                // value at starting election
  real xi_2019[N_parties];                 // value at final election
  int election_day_2019;                   // date of 2019 election
  
  // poll results
  matrix[N_polls, N_parties] y;            // actual values in polls for each party
  matrix[N_polls, N_parties] y_moe;        // margins of errors for each party based on srs
  
  // poll date + polster id
  int<lower=1> poll_date[N_polls];         // the number of days since starting election each poll was taken
  int pollster_id[N_polls];                // id for each pollster for each party
  
  // mode id
  int<lower=1> mode_id[N_polls];           // mode id for variance
  
}


parameters {
  
  matrix[N_days-2, N_parties] z_omega;                          // Matrix to hold std_normal errors for non-centered parameterization 
  vector<lower=0>[N_parties] omega;                             // innovation sd for each party
  cholesky_factor_corr[N_parties] L_Rho;                        // L_corr to account for correlated errors
  
  vector<lower=0>[N_parties] sigma_party;                       // party specific error
  matrix<lower=0>[N_pollsters, N_parties] sigma_pollster;       // to account for error per pollster other than sampling
  matrix<lower=0>[N_modes, N_parties] sigma_mode;               // to account for mode
  
  matrix[N_pollsters, N_parties] delta;                         // house effect for each pollster for each party
  
  //matrix[N_modes, N_parties] gamma;                           // uncomment for mode effects
  
}

transformed parameters {
  matrix[N_days-2, N_parties] Omega;                            // matrix to hold innovation sd per party
  matrix[N_days, N_parties] xi;                                 // underlying state of vote intention
  matrix<lower=0>[N_polls, N_parties] sigma;                    // total sd
  matrix[N_polls, N_parties] mu;                                // xi + delta


  Omega = z_omega * diag_pre_multiply(omega, L_Rho);            // Done as pre-multiplication because indexing was easier for me
  

  // set up non-centered paramterization for innovation sd
  for(j in 1:N_parties) {
    
    xi[1, j] = xi_start[j];
    xi[election_day_2019,j] = xi_2019[j];
      
    // from 2015 until 2019 election
    for (t in 2:(election_day_2019 - 1)) {
      
      xi[t, j] = xi[(t - 1), j] + Omega[(t - 1), j];
      
      }
      
    // post 2019 election
    for(tt in (election_day_2019 + 1):N_days) {
      
      xi[tt, j] = xi[(tt - 1), j] + Omega[(tt - 2), j];
      
    }
      
  }
    
  // build mu and sigma for likelihood  
  for(i in 1:N_polls){
    for(j in 1:N_parties){
      
      sigma[i, j] = sqrt(square(sigma_party[j]) + square(sigma_pollster[pollster_id[i], j]) + 
                         square(sigma_mode[mode_id[i], j]) + square(y_moe[i, j]));
      //mu[i, j] = xi[poll_date[i], j] + delta[pollster_id[i], j] + gamma[mode_id[i], j];    // uncomment for mode effects
      mu[i, j] = xi[poll_date[i], j] + delta[pollster_id[i], j];

      }
      
    }
    
    
}


model {
  
  // priors for innovation sd (omega and corr matrix)
  to_vector(z_omega) ~ normal(0, 1);
  omega ~ exponential(5);
  L_Rho ~ lkj_corr_cholesky(2);

  // priors for various sigmas
  sigma_party ~ exponential(5);
  to_vector(sigma_pollster) ~ exponential(5);
  to_vector(sigma_mode) ~ exponential(5);
 
  // prior for house effects and survey mode effects (if applicable) 
  to_vector(delta) ~ normal(0, 0.05);
  //to_vector(gamma) ~ normal(0, 0.05);                         // uncomment for mode effects


  // force small sd of xi on day before election day, to force 
  for(j in 1:N_parties){

    xi_2019[j] ~ normal(xi[election_day_2019-1,j] , 0.001); 
    
  }

  
  
   // Likelihood
  for(j in 1:N_parties){
    y[, j] ~ normal(mu[, j], sigma[, j]);
  }
  
}


generated quantities {
  matrix[N_parties, N_parties] Rho;

  Rho = multiply_lower_tri_self_transpose(L_Rho);

}


