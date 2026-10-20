// Hierarchical linear likelihood model of information aggregation
// with sequential effect

data {
  int<lower=1> N; // number of subjects
  int<lower=1> T_max; // maximum number of trials among all subjects
  int<lower=1> I; // number of info by trials
  int Tsubj[N]; // number of trials for each subject
  int<lower=1, upper=2> color[N,T_max, I]; // sequences of info colors: blue=1, red=2
  real proba[N,T_max, I]; // sequences of probabilities
  // real l_proba[N,T_max, I]; // sequences of logit probabilities
  int<lower=1, upper=2> choice[N,T_max]; // choices for each subject
}

parameters {
  //hypergroup param
  vector[I-1] mu_w; // sequential weights mean (for  i:1-5; w6 normaliszed to 1)
  vector[4] mu_pr; // distorted_50, distorted_55, distorted_65, theta means
  vector<lower=0>[I-1] sigma_w; // sequential weights var 
  vector<lower=0>[4] sigma; // distorted_50, distorted_55, distorted_65, theta


  //  subject level param
  vector[N] distorted_50_raw;
  vector[N] distorted_55_raw;
  vector[N] distorted_65_raw;
  vector[N] theta_raw;
  matrix[N, I-1] w_raw;
}

transformed parameters {
  vector<lower = -2, upper = 5>[N] distorted_50;
  vector<lower = -2, upper = 5>[N] distorted_55;
  vector<lower = -2, upper = 5>[N] distorted_65;
  vector<lower = 1, upper = 3>[N] theta;
  matrix<lower=0, upper=2>[N, I-1] w;
  
  for (n in 1:N){
    distorted_50[n] = Phi_approx(mu_pr[1]+ sigma[1]*distorted_50_raw[n])*7 - 2; 
    distorted_55[n] = Phi_approx(mu_pr[2]+ sigma[2]*distorted_55_raw[n])*7 - 2; 
    distorted_65[n] = Phi_approx(mu_pr[3]+ sigma[3]*distorted_65_raw[n])*7 - 2; 
    theta[n] = 1 + Phi_approx(mu_pr[4] + sigma[4]*theta_raw[n]) * 2;
    for (i in 1:5){
    	w[n,i] = Phi_approx(mu_w[i]+ sigma_w[i]*w_raw[n,i])*2;
    }
  }
}

model {
  // Hypermarameters
  mu_w    ~ std_normal();
  sigma_w ~ normal(0,.2);
  mu_pr   ~ std_normal();
  sigma   ~ normal(0,.2);

  // individual parameters
  distorted_50_raw      ~ std_normal();
  distorted_55_raw       ~ std_normal();
  distorted_65_raw       ~ std_normal();
  theta_raw       ~ std_normal();
  to_vector(w_raw) ~ std_normal();
  

  for (n in 1:N) {
    for (t in 1:Tsubj[n]) {
      vector[2] evidence;
      real weight;
      vector[2] val;
      evidence = rep_vector(0, 2);
      for (i in 1:I) {
      	if (i == 6){
      	  weight = 1;
      	}
      	else{
      	  weight = w[n,i];
      	}
        if (color[n, t, i] == 1) { // color = blue
          // evidence[1] += weight*(alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
          if (proba[n, t, i]==0.5){
            evidence[1] += weight*distorted_50[n];
          }
          if (proba[n, t, i]==0.55){
            evidence[1] += weight*distorted_55[n];
          }
          if (proba[n, t, i]==0.65){
            evidence[1] += weight*distorted_65[n];
          }
        }
        if (color[n, t, i] == 2) { // color = red
          // evidence[2] += weight*(alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
          if (proba[n, t, i]==0.5){
            evidence[2] += weight*distorted_50[n];
          }
          if (proba[n, t, i]==0.55){
            evidence[2] += weight*distorted_55[n];
          }
          if (proba[n, t, i]==0.65){
            evidence[2] += weight*distorted_65[n];
          }
        }
      }
      // softmax choice
      val = softmax(theta[n]*evidence);
      choice[n,t] ~ categorical(val);
    }
  }
}

generated quantities {
  // For group level parameters
  real mu_distorted_50;
  real mu_distorted_55;
  real mu_distorted_65;
  real mu_theta;
  real  mu_w1;
  real  mu_w2;
  real  mu_w3;
  real  mu_w4;
  real  mu_w5;  

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
  
  mu_distorted_50   = Phi_approx(mu_pr[1])*7-2;
  mu_distorted_55   = Phi_approx(mu_pr[2])*7-2;
  mu_distorted_65   = Phi_approx(mu_pr[3])*7-2;
  mu_theta   = 1+Phi_approx(mu_pr[4])*2;
  mu_w1      = Phi_approx(mu_w[1])*2;
  mu_w2      = Phi_approx(mu_w[2])*2;
  mu_w3      = Phi_approx(mu_w[3])*2;
  mu_w4      = Phi_approx(mu_w[4])*2;
  mu_w5      = Phi_approx(mu_w[5])*2;
  
  { // local section, this saves time and space
    for (n in 1:N) {
      // define values
      vector[2] evidence;
      real weight;
      vector[2] val;
      evidence = rep_vector(0, 2);
      for (t in 1:Tsubj[n]) {
	evidence = rep_vector(0, 2);
	for (i in 1:I) {
	  if (i == 6){
	    weight = 1;
	  }
	  else{
	    weight = w[n,i];
	  }
	  if (color[n, t, i] == 1) { // color = blue
	    // evidence[1] += weight*( alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
      if (proba[n, t, i]==0.5){
        evidence[1] += weight*distorted_50[n];
      }
      if (proba[n, t, i]==0.55){
        evidence[1] += weight*distorted_55[n];
      }
      if (proba[n, t, i]==0.65){
        evidence[1] += weight*distorted_65[n];
      }
	  }
	  if (color[n, t, i] == 2) {  // color = red
	    // evidence[2] += weight*(alpha[n] * log(proba[n, t, i] / (1 - proba[n, t, i])) + beta[n]);
      if (proba[n, t, i]==0.5){
        evidence[2] += weight*distorted_50[n];
      }
      if (proba[n, t, i]==0.55){
        evidence[2] += weight*distorted_55[n];
      }
      if (proba[n, t, i]==0.65){
        evidence[2] += weight*distorted_65[n];
      }
	  }
	}
	  // Calculate log likelihood and store it in the array
	val = softmax(theta[n]*evidence);
	log_lik[n,t] = categorical_lpmf(choice[n, t] | val);
	y_pred[n,t] = categorical_rng(val);
      }
    }
  }
}
