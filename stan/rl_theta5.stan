data {

  int<lower=1> N;                         // number of participants
  int<lower=1> T_max;                     // maximum trials
  int<lower=1> I;                         // number of information sources

  array[N] int<lower=1> Tsubj;
  array[N, T_max, I] int<lower=1, upper=2> color;
  array[N, T_max, I] int<lower=50, upper=65> level;
  array[N, T_max] int<lower=1, upper=2> choice;
  array[N, T_max] int<lower=1, upper=2> feedback;


}


parameters {

  // group-level means (unconstrained)
  vector[1] mu_pr;                        // delta, theta, lambda

  // group-level standard deviations
  vector<lower=0>[1] sigma;              // delta, theta, lambda

  // individual-level parameters (non-centered, unconstrained)
  vector[N] delta_raw;
  // vector[N] theta_raw;
  // vector[N] lambda_raw;

  // group-level initial values
  real V50_init;
  real V55_init;
  real V65_init;

}


transformed parameters {

  vector<lower=0,upper=1>[N] delta;       // learning rate:     (0, 1)
  // vector<lower=0,upper=5>[N] theta;       // decision scaling:  (0, 5)
  // vector[N]                  lambda;      // temporal discount: unconstrained


  // // group-level initial values
  // V50_init ~ normal(0, 1);
  // V55_init ~ normal(0, 1);
  // V65_init ~ normal(0, 1);

  for (n in 1:N) {
    delta[n]  = Phi_approx(mu_pr[1] + sigma[1] * delta_raw[n]);
    // theta[n]  = 1 + 4 * Phi_approx(mu_pr[2] + sigma[2] * theta_raw[n]);
    // lambda[n] = mu_pr[3] + sigma[3] * lambda_raw[n];
    // lambda[n] = 0;
  }

}


model {

  // -------------------------------------------------------
  // Priors: group-level
  // -------------------------------------------------------

  mu_pr ~ normal(0, 1);
  // tighter prior on delta mean to push toward smaller learning rates
  // mu_pr[1] ~ normal(0, 0.5);
  mu_pr[1] ~ normal(-1, 0.5);
  sigma[1] ~ normal(0, 0.2);             // delta SD
  // sigma[2] ~ normal(0, 0.2);             // theta SD
  // sigma[3] ~ normal(0, 0.2);             // lambda SD

  // -------------------------------------------------------
  // Priors: individual-level (non-centered)
  // -------------------------------------------------------

  delta_raw  ~ std_normal();
  // theta_raw  ~ std_normal();
  // lambda_raw ~ std_normal();

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

    real V50 = V50_init;
    real V55 = V55_init;
    real V65 = V65_init;

    for (t in 1:Tsubj[n]) {

      // -------------------------------------------------------
      // Evidence accumulation
      // -------------------------------------------------------

      vector[2] evidence = rep_vector(0, 2);

      for (i in 1:I) {

        real value;

        if (level[n,t,i] == 50) {
          value = V50;
        } else if (level[n,t,i] == 55) {
          value = V55;
        } else {
          value = V65;
        }

        // discount = exp(lambda[n] * (i - I));

        // accumulate evidence for the cue's color
        evidence[color[n,t,i]] += value;

      }

      // -------------------------------------------------------
      // Choice probability and likelihood
      // -------------------------------------------------------

      // softmax over [blue, red] evidence scaled by theta
      
      // vector[2] scaled_evidence = theta[n] * evidence;
      // scaled_evidence = scaled_evidence - max(scaled_evidence);
      // vector[2] val = softmax(scaled_evidence);
      
      vector[2] val_decision = softmax(5*evidence);
      vector[2] val_PE = softmax(evidence);
      
      choice[n,t] ~ categorical(val_decision);


      // -------------------------------------------------------
      // Reward and prediction error
      // -------------------------------------------------------

      real reward    = (choice[n,t] == feedback[n,t]) ? 1.0 : 0.0;
      real pCorrect  = val_PE[feedback[n,t]];             // this should not be modulated by theta
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
      // if (abs(V50) * abs(V55) * abs(V65) > 1e-5){
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

      // V50 += delta[n] * credit50 * PE;
      // V55 += delta[n] * credit55 * PE;
      // V65 += delta[n] * credit65 * PE;
      
      V50 = fmin(fmax(V50 + delta[n] * credit50 * PE, -10), 10);
      V55 = fmin(fmax(V55 + delta[n] * credit55 * PE, -10), 10);
      V65 = fmin(fmax(V65 + delta[n] * credit65 * PE, -10), 10);
      


    } // end trial loop

  } // end participant loop

}


generated quantities {

  // recovered group-level means in constrained space
  real mu_delta  = Phi_approx(mu_pr[1]);
  // real mu_theta  = 1 +4 * Phi_approx(mu_pr[2]);
  // real mu_lambda = mu_pr[3];
  // real mu_V50_init = V50_init;
  // real mu_V55_init = V55_init;
  // real mu_V65_init = V65_init;

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

      real V50 = V50_init;
      real V55 = V55_init;
      real V65 = V65_init;

      for (t in 1:Tsubj[n]) {

        vector[2] evidence = rep_vector(0, 2);

        for (i in 1:I) {

          real value;

          if (level[n,t,i] == 50) {
            value = V50;
          } else if (level[n,t,i] == 55) {
            value = V55;
          } else {
            value = V65;
          }

          // discount = exp(lambda[n] * (i - I));
          evidence[color[n,t,i]] += value;

        }

        // vector[2] scaled_evidence = theta[n] * evidence;
        // scaled_evidence = scaled_evidence - max(scaled_evidence);
        // vector[2] val = softmax(scaled_evidence);
        
        vector[2] val_decision = softmax(5*evidence);
        vector[2] val_PE = softmax(evidence);

        log_lik[n,t] = categorical_lpmf(choice[n,t] | val_decision);
        y_pred[n,t]  = categorical_rng(val_decision);

        // value update — must mirror model block exactly
        real reward   = (choice[n,t] == feedback[n,t]) ? 1.0 : 0.0;
        real pCorrect = val_PE[feedback[n,t]];
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
        // if (abs(V50) * abs(V55) * abs(V65) > 1e-5){
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

        // V50 += delta[n] * credit50 * PE;
        // V55 += delta[n] * credit55 * PE;
        // V65 += delta[n] * credit65 * PE;
        
        V50 = fmin(fmax(V50 + delta[n] * credit50 * PE, -10), 10);
        V55 = fmin(fmax(V55 + delta[n] * credit55 * PE, -10), 10);
        V65 = fmin(fmax(V65 + delta[n] * credit65 * PE, -10), 10);
        


      } // end trial loop

    } // end participant loop

  } // end local scope

}
