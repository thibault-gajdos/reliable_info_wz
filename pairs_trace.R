## Correlation and trace plots



####### non-parallel

library(readr)
library(ggplot2)
library(bayesplot)

# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'



# Extract posterior samples (basic + seq + theta)
# fit = read_rds(paste('./results/',chosen_exp,'/log_seq_theta.rds', sep = ''), refhook = NULL)
# fit = read_rds(paste('./results/',chosen_exp,'/3values_seq_theta.rds', sep = ''), refhook = NULL)
fit = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)

posterior <- as.array(fit)

# dim(posterior)
# all_names <- dimnames(posterior)[[3]]
# group_pars <- grep("mu_", all_names, value = TRUE)

# target_pars = c("mu_alpha", "mu_beta", "mu_theta")
# target_pars = c("mu_distorted_50", "mu_distorted_55", "mu_distorted_65", "mu_theta")
target_pars = c("mu_alpha", "mu_beta")

mcmc_pairs(posterior, 
           pars = target_pars, 
           off_diag_fun = "scatter",
           off_diag_args = list(alpha = 0.2, size = 1))

mcmc_trace(posterior, 
           pars = target_pars,
           facet_args = list(ncol = 1, strip.position = "left")) +
  theme_minimal()


####### parallel

library(readr)
library(ggplot2)
library(bayesplot)
library(posterior)

# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'

# fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice.rds',sep = ''))
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice.rds',sep = ''))

posterior_samples <- fit$draws()
posterior_df <- as_draws_df(posterior_samples)
selected_params <- posterior_df[, grepl("^mu", colnames(posterior_df)) & !grepl("^mu_pr", colnames(posterior_df))]
mcmc_pairs(selected_params, off_diag_args = list(alpha = 0.2, size = 1))

selected_params <- posterior_df[, grepl("^log_lik", colnames(posterior_df))]
total_log_lik_draws <- rowSums(selected_params)
mean_log_lik <- mean(total_log_lik_draws)


keep <- grepl('^mu_', colnames(posterior_df)) | grepl('^mu_pr\\[', colnames(posterior_df))
selected_params <- posterior_df[, keep, drop = FALSE]
mcmc_trace(selected_params)



