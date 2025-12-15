// Hierarchical linear likelihood model of information aggregation
// without sequential effect

data {
  int<lower=1> N; // number of subjects
  int<lower=1> T_max; // maximum number of trials among all subjects
  int<lower=1> I; // number of info by trials
  int Tsubj[N]; // number of trials for each subject
  int<lower=-1, upper=2> color[N,T_max, I]; // sequences of info colors: blue=1, red=2
  real proba[N,T_max, I]; // sequences of probabilities
  int<lower=1, upper=2> choice[N,T_max]; // choices for each subject
}

parameters {
  //hypergroup param
  vector[2] mu_pr; // alpha, beta means
  vector<lower=0>[2] sigma; // alpha, beta

  //  subject level param
  vector[N] alpha_raw;
  vector[N] beta_raw;
}

transformed parameters {
  vector<lower = 0>[N] alpha;
  vector[N] beta;

  for (n in 1:N){
    alpha[n] = Phi_approx(mu_pr[1]+ sigma[1]*alpha_raw[n])*20; 
    beta[n] = mu_pr[2]+sigma[2]*beta_raw[n]; 
  }
}

model {
  // Hypermarameters
  mu_pr   ~ std_normal();
  sigma   ~ normal(0,.2);

  // individual parameters
  alpha_raw      ~ std_normal();
  beta_raw       ~ std_normal();

  for (n in 1:N) {
    for (t in 1:Tsubj[n]) {
      vector[2] evidence;
      vector[2] val;
      evidence = rep_vector(0, 2);

      for (i in 1:I) {
        if (color[n, t, i] == 1) { // color = blue
          evidence[1] += (alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
        }
        if (color[n, t, i] == 2) { // color = red
          evidence[2] += (alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
        }
        if (color[n, t, i] == -1) {  
          break;
          }
      }
    
      // softmax choice
      val = softmax(evidence);
      choice[n,t] ~ categorical(val);
    }
  }
}

generated quantities {
  // For group level parameters
  real mu_alpha;
  real mu_beta;

  // For log likelihood calculation
  real log_lik[N,T_max];
  
  // For posterior predictive check
  real y_pred[N, T_max];
  
  // Set all posterior predictions to -1 (avoids NULL values)
  for (n in 1:N) {
    for (t in 1:T_max) {
      y_pred[n, t] = -1;
      log_lik[n , t] = 0;
	}
  }
  
  mu_alpha   = Phi_approx(mu_pr[1])*20;
  mu_beta    = mu_pr[2]; 

  { // local section, this saves time and space
    for (n in 1:N) {
      // define values
      vector[2] evidence;
      vector[2] val;
      evidence = rep_vector(0, 2);
      for (t in 1:Tsubj[n]) {
      	evidence = rep_vector(0, 2);
        
        for (i in 1:I) {
      	  if (color[n, t, i] == 1) { // color = blue
      	    evidence[1] += (alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
      	  }
      	  if (color[n, t, i] == 2) {  // color = red
      	    evidence[2] += (alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
      	  }
          if (color[n, t, i] == -1) {  
          break;
          }      	  
      	}
      	  // Calculate log likelihood and store it in the array
      	val = softmax(evidence);
      	log_lik[n,t] = categorical_lpmf(choice[n, t] | val);
      	y_pred[n,t] = categorical_rng(val);
      }
    }
  }
}
