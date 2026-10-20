data {

  int<lower=1> N;                         // number of participants
  int<lower=1> T_max;                     // maximum trials
  int<lower=1> I;                         // number of information sources

  array[N] int<lower=1> Tsubj;
  array[N, T_max, I] int<lower=-1, upper=2> color;
  array[N, T_max, I] int<lower=50, upper=65> level;
  array[N, T_max] int<lower=-1, upper=2> choice;
  array[N, T_max] int<lower=-1, upper=2> optimal;


}


parameters {

  // group-level means (unconstrained)
  vector[3] mu_pr;                        // alpha, theta, lambda

  // group-level standard deviations
  vector<lower=0>[3] sigma;              // alpha, theta, lambda

  // individual-level parameters (non-centered, unconstrained)
  vector[N] alpha_raw;
  vector[N] theta_raw;
  vector[N] lambda_raw;

  // individual-level initial values
  vector[N] V50_init;
  vector[N] V55_init;
  vector[N] V65_init;

}


transformed parameters {

  vector<lower=0,upper=1>[N] alpha;       // learning rate:     (0, 1)
  vector<lower=0,upper=5>[N] theta;       // decision scaling:  (0, 5)
  vector[N]                  lambda;      // temporal discount: unconstrained

  for (n in 1:N) {
    alpha[n]  = Phi_approx(mu_pr[1] + sigma[1] * alpha_raw[n]);
    theta[n]  = 1 + 4 * Phi_approx(mu_pr[2] + sigma[2] * theta_raw[n]);
    lambda[n] = mu_pr[3] + sigma[3] * lambda_raw[n];
  }

}


model {

  // -------------------------------------------------------
  // Priors: group-level
  // -------------------------------------------------------

  mu_pr ~ normal(0, 1);
  // tighter prior on alpha mean to push toward smaller learning rates
  // mu_pr[1] ~ normal(0, 0.5);
  mu_pr[1] ~ normal(-1, 0.5);
  sigma[1] ~ normal(0, 0.2);             // alpha SD
  sigma[2] ~ normal(0, 0.2);             // theta SD
  sigma[3] ~ normal(0, 0.2);             // lambda SD

  // -------------------------------------------------------
  // Priors: individual-level (non-centered)
  // -------------------------------------------------------

  alpha_raw  ~ std_normal();
  theta_raw  ~ std_normal();
  lambda_raw ~ std_normal();

  // -------------------------------------------------------
  // Priors: initial values
  // -------------------------------------------------------

  V50_init ~ normal(0, 1);
  V55_init ~ normal(0, 1);
  V65_init ~ normal(0, 1);


  // -------------------------------------------------------
  // Likelihood
  // -------------------------------------------------------

  for (n in 1:N) {

    real V50 = V50_init[n];
    real V55 = V55_init[n];
    real V65 = V65_init[n];

    for (t in 1:Tsubj[n]) {

      // -------------------------------------------------------
      // Evidence accumulation
      // -------------------------------------------------------

      vector[2] evidence = rep_vector(0, 2);

      for (i in 1:I) {

        real value;
        real discount;

        if (level[n,t,i] == 50) {
          value = V50;
        } else if (level[n,t,i] == 55) {
          value = V55;
        } else {
          value = V65;
        }

        discount = exp(lambda[n] * (i - I));

        // accumulate evidence for the cue's color
        evidence[color[n,t,i]] += discount * value;

      }

      // -------------------------------------------------------
      // Choice probability and likelihood
      // -------------------------------------------------------

      // softmax over [blue, red] evidence scaled by theta
      vector[2] scaled_evidence = theta[n] * evidence;
      // prevent softmax overflow
      scaled_evidence = scaled_evidence - max(scaled_evidence);
      vector[2] val = softmax(scaled_evidence);
      // vector[2] val = softmax(theta[n] * evidence);

      choice[n,t] ~ categorical(val);


      // -------------------------------------------------------
      // Reward and prediction error
      // -------------------------------------------------------

      real reward    = (choice[n,t] == optimal[n,t]) ? 1.0 : 0.0;
      real pCorrect  = val[optimal[n,t]];             // p(choosing optimally)
      real PE        = reward - pCorrect;


      // -------------------------------------------------------
      // Credit assignment
      // -------------------------------------------------------

      real credit50 = 0;
      real credit55 = 0;
      real credit65 = 0;

      for (i in 1:I) {

        // +1 if cue color matches choice, -1 otherwise
        real contribution = (color[n,t,i] == choice[n,t]) ? 1.0 : -1.0;

        if (level[n,t,i] == 50) {
          credit50 += contribution;
        } else if (level[n,t,i] == 55) {
          credit55 += contribution;
        } else {
          credit65 += contribution;
        }

      }
      
      // change 6
      // if (abs(V50) > 1e-5 && abs(V55) > 1e-5 && abs(V65) > 1e-5){
      //   credit50 = credit50*abs(V50);
      //   credit55 = credit55*abs(V55);
      //   credit65 = credit65*abs(V65);
      // }

      // normalise credits by total absolute credit
      real totalCredit = abs(credit50) + abs(credit55) + abs(credit65);

      if (totalCredit > 0) {
        credit50 /= totalCredit;
        credit55 /= totalCredit;
        credit65 /= totalCredit;
      }


      // -------------------------------------------------------
      // Value update
      // -------------------------------------------------------

      // V50 += alpha[n] * credit50 * PE;
      // V55 += alpha[n] * credit55 * PE;
      // V65 += alpha[n] * credit65 * PE;
      
      V50 = fmin(fmax(V50 + alpha[n] * credit50 * PE, -10), 10);
      V55 = fmin(fmax(V55 + alpha[n] * credit55 * PE, -10), 10);
      V65 = fmin(fmax(V65 + alpha[n] * credit65 * PE, -10), 10);
      
      
      // after value update, before next trial
      if (is_nan(V50) || is_nan(V55) || is_nan(V65)) {
        reject("nan in values at participant ", n, " trial ", t,
               " V50=", V50, " V55=", V55, " V65=", V65,
               " PE=", PE, " credit50=", credit50,
               " credit55=", credit55, " credit65=", credit65);
      }

    } // end trial loop

  } // end participant loop

}


generated quantities {

  // recovered group-level means in constrained space
  real mu_alpha  = Phi_approx(mu_pr[1]);
  real mu_theta  = 1 +4 * Phi_approx(mu_pr[2]);
  real mu_lambda = mu_pr[3];

  // log likelihood (for LOO / WAIC)
  array[N, T_max] real log_lik;

  // posterior predictive choices
  array[N, T_max] int y_pred;

  // initialise arrays
  for (n in 1:N) {
    for (t in 1:T_max) {
      log_lik[n,t] = 0;
      y_pred[n,t]  = -1;
    }
  }

  { // local scope: saves memory, keeps intermediate vars out of global space

    for (n in 1:N) {

      real V50 = V50_init[n];
      real V55 = V55_init[n];
      real V65 = V65_init[n];

      for (t in 1:Tsubj[n]) {

        vector[2] evidence = rep_vector(0, 2);

        for (i in 1:I) {

          real value;
          real discount;

          if (level[n,t,i] == 50) {
            value = V50;
          } else if (level[n,t,i] == 55) {
            value = V55;
          } else {
            value = V65;
          }

          discount = exp(lambda[n] * (i - I));
          evidence[color[n,t,i]] += discount * value;

        }

        vector[2] scaled_evidence = theta[n] * evidence;
        // prevent softmax overflow
        scaled_evidence = scaled_evidence - max(scaled_evidence);
        vector[2] val = softmax(scaled_evidence);
        // vector[2] val = softmax(theta[n] * evidence);

        log_lik[n,t] = categorical_lpmf(choice[n,t] | val);
        y_pred[n,t]  = categorical_rng(val);

        // value update — must mirror model block exactly
        real reward   = (choice[n,t] == optimal[n,t]) ? 1.0 : 0.0;
        real pCorrect = val[optimal[n,t]];
        real PE       = reward - pCorrect;

        real credit50 = 0;
        real credit55 = 0;
        real credit65 = 0;

        for (i in 1:I) {

          real contribution = (color[n,t,i] == choice[n,t]) ? 1.0 : -1.0;

          if (level[n,t,i] == 50) {
            credit50 += contribution;
          } else if (level[n,t,i] == 55) {
            credit55 += contribution;
          } else {
            credit65 += contribution;
          }

        }
        
        // change 6
        // if (abs(V50) > 1e-5 && abs(V55) > 1e-5 && abs(V65) > 1e-5){
        //   credit50 = credit50*abs(V50);
        //   credit55 = credit55*abs(V55);
        //   credit65 = credit65*abs(V65);
        // }

        real totalCredit = abs(credit50) + abs(credit55) + abs(credit65);

        if (totalCredit > 0) {
          credit50 /= totalCredit;
          credit55 /= totalCredit;
          credit65 /= totalCredit;
        }

        // V50 += alpha[n] * credit50 * PE;
        // V55 += alpha[n] * credit55 * PE;
        // V65 += alpha[n] * credit65 * PE;
        
        V50 = fmin(fmax(V50 + alpha[n] * credit50 * PE, -10), 10);
        V55 = fmin(fmax(V55 + alpha[n] * credit55 * PE, -10), 10);
        V65 = fmin(fmax(V65 + alpha[n] * credit65 * PE, -10), 10);
        
        // after value update, before next trial
        if (is_nan(V50) || is_nan(V55) || is_nan(V65)) {
          reject("nan in values at participant ", n, " trial ", t,
                 " V50=", V50, " V55=", V55, " V65=", V65,
                 " PE=", PE, " credit50=", credit50,
                 " credit55=", credit55, " credit65=", credit65);
        }

      } // end trial loop

    } // end participant loop

  } // end local scope

}
