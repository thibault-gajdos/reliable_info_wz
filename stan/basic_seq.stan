// Hierarchical linear likelihood model of information aggregation
// with sequential effect
// alpha = 1, beta = 0 fixed

data {
  int<lower=1> N;        // number of subjects
  int<lower=1> T_max;    // maximum number of trials among all subjects
  int<lower=1> I;        // number of info per trial
  int Tsubj[N];          // number of trials per subject
  int<lower=1, upper=2> color[N,T_max, I]; // blue=1, red=2
  real proba[N,T_max, I];                  // probabilities
  int<lower=1, upper=2> choice[N,T_max];  // choices for each subject
}

parameters {
  // Sequential weights
  vector[I-1] mu_w;                 // group mean for weights
  vector<lower=0>[I-1] sigma_w;     // weight variance
  matrix[N, I-1] w_raw;             // subject-level raw weights
}

transformed parameters {
  matrix<lower=0, upper=2>[N, I-1] w; // subject-level weights

  for (n in 1:N) {
    for (i in 1:(I-1)) {
      w[n,i] = Phi_approx(mu_w[i] + sigma_w[i] * w_raw[n,i]) * 2;
    }
  }
}

model {
  // Priors for sequential weights
  mu_w    ~ std_normal();
  sigma_w ~ normal(0, 0.2);
  to_vector(w_raw) ~ std_normal();

  for (n in 1:N) {
    for (t in 1:Tsubj[n]) {
      vector[2] evidence = rep_vector(0, 2);
      real weight;

      for (i in 1:I) {
        if (i == I) weight = 1;   // last info weight fixed to 1
        else        weight = w[n,i];

        if (color[n,t,i] == 1) evidence[1] += weight * logit(proba[n,t,i]);
        if (color[n,t,i] == 2) evidence[2] += weight * logit(proba[n,t,i]);
      }

      choice[n,t] ~ categorical(softmax(evidence));
    }
  }
}

generated quantities {
  // Group-level weight means
  real mu_w1 = Phi_approx(mu_w[1]) * 2;
  real mu_w2 = Phi_approx(mu_w[2]) * 2;
  real mu_w3 = Phi_approx(mu_w[3]) * 2;
  real mu_w4 = Phi_approx(mu_w[4]) * 2;
  real mu_w5 = Phi_approx(mu_w[5]) * 2;

  // Log-likelihood and posterior predictive
  real log_lik[N,T_max];
  real y_pred[N,T_max];

  // Initialize
  for (n in 1:N)
    for (t in 1:T_max) {
      log_lik[n,t] = 0;
      y_pred[n,t] = -1;
    }

  for (n in 1:N) {
    for (t in 1:Tsubj[n]) {
      vector[2] evidence = rep_vector(0, 2);
      real weight;
      vector[2] val;

      for (i in 1:I) {
        if (i == I) weight = 1;
        else        weight = w[n,i];

        if (color[n,t,i] == 1) evidence[1] += weight * logit(proba[n,t,i]);
        if (color[n,t,i] == 2) evidence[2] += weight * logit(proba[n,t,i]);
      }

      val = softmax(evidence);
      log_lik[n,t] = categorical_lpmf(choice[n,t] | val);
      y_pred[n,t] = categorical_rng(val);
    }
  }
}
