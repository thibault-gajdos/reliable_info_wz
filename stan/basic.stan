// Hierarchical linear likelihood model of information aggregation
// without sequential effect
// (alpha = 1, beta = 0 fixed)

data {
  int<lower=1> N; // number of subjects
  int<lower=1> T_max; // maximum number of trials
  int<lower=1> I; // number of information pieces per trial
  int Tsubj[N]; // number of trials for each subject
  int<lower=-1, upper=2> color[N,T_max, I]; // blue=1, red=2, -1 = no more info
  real proba[N,T_max, I]; // probabilities
  int<lower=1, upper=2> choice[N,T_max]; // participant choice
}

parameters {
  // No free parameters since alpha and beta are fixed
}

model {
  for (n in 1:N) {
    for (t in 1:Tsubj[n]) {
      vector[2] evidence;
      vector[2] val;
      evidence = rep_vector(0, 2);

      for (i in 1:I) {

        if (color[n, t, i] == -1)
          break;

        // alpha = 1, beta = 0
        real update = logit(proba[n, t, i]);

        if (color[n, t, i] == 1)      evidence[1] += update;
        else if (color[n, t, i] == 2) evidence[2] += update;
      }

      val = softmax(evidence);
      choice[n,t] ~ categorical(val);
    }
  }
}

generated quantities {
  real log_lik[N,T_max];
  real y_pred[N, T_max];

  // init
  for (n in 1:N)
    for (t in 1:T_max) {
      log_lik[n,t] = 0;
      y_pred[n,t] = -1;
    }

  // posterior predictive + log likelihood
  for (n in 1:N) {
    for (t in 1:Tsubj[n]) {
      vector[2] evidence = rep_vector(0, 2);
      vector[2] val;

      for (i in 1:I) {

        if (color[n, t, i] == -1)
          break;

        real update = logit(proba[n, t, i]);

        if (color[n, t, i] == 1)      evidence[1] += update;
        else if (color[n, t, i] == 2) evidence[2] += update;
      }

      val = softmax(evidence);
      log_lik[n,t] = categorical_lpmf(choice[n,t] | val);
      y_pred[n,t] = categorical_rng(val);
    }
  }
}
