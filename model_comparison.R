library(loo)

# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'



load(paste0('results/',chosen_exp,'/loo/loo_log_seq.rdata')) 
loo_log_seq <- loo

load(paste0('results/',chosen_exp,'/loo/loo_log_seq_theta.rdata')) 
loo_log_seq_theta <- loo

load(paste0('results/',chosen_exp,'/loo/loo_log_seq_theta(narrow).rdata')) 
loo_log_seq_theta_narrow <- loo

load(paste0('results/',chosen_exp,'/loo/loo_3values_seq_theta.rdata')) 
loo_log_seq_3values <- loo

load(paste0('results/',chosen_exp,'/loo/loo_BLO_choice.rds')) 
loo_BLO_choice <- loo

load(paste0('results/',chosen_exp,'/loo/loo_BLO_noise_choice.rds')) 
loo_BLO_noise_choice <- loo



comp = loo_compare(loo_log_seq, 
            loo_log_seq_theta, 
            # loo_log_seq_theta_narrow,  
            loo_log_seq_3values,
            loo_BLO_choice,
            loo_BLO_noise_choice
            )


df = data.frame(comp[,c(1,2)])

name_map <- c(
  model1 = "basic_seq",
  model2 = "basic_seq_theta",
  # model3 = "basic_seq_theta_narrow",
  model3 = "3values",
  model4 = "BLO_choice",
  model5 = "BLO_noise_choice"
)

model.name = name_map[row.names(comp)]

df = cbind(df, model.name)
df$model.name <- factor(
  df$model.name,
  levels = rev(unique(df$model.name))
)

df$ci_low  <- df$elpd_diff - 1.96 * df$se_diff
df$ci_high <- df$elpd_diff + 1.96 * df$se_diff


df
ggplot(df, aes(x = model.name, y = elpd_diff)) +
  geom_col(fill = 'gray50') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.1) +
  theme_minimal() +
  ylab('expected log predictive density difference') +
  labs(title = chosen_exp)



#### fit




# additional_suffix = '_original_edit'
# additional_suffix = '_redo'
additional_suffix = ''

fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice',additional_suffix,'.rds',sep = ''))
# fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice',additional_suffix,'.rds',sep = '')

library(shinystan)
# launch_shinystan(fit)

library(cmdstanr)
library(rstan)
# draws = fit$draws(variables = c(''))
library(tidybayes)
library(dplyr)


indiv_draws <- fit %>%
  spread_draws(params[n, v])

indiv_draws <- indiv_draws %>%
  mutate(param_name = case_when(
    v == 1 ~ "lambda",
    v == 2 ~ "delta+",
    v == 3 ~ "delta-",
    v == 4 ~ "psi",
    v == 5 ~ "kappa",
    v == 6 ~ "l_anchor",
    v == 7 ~ "theta_choice"
  ))

# hist(indiv_draws$params[which(indiv_draws$param_name=='theta_choice')], breaks=seq(0,1000,0.5),xlim = c(0,20))

hist(indiv_draws$params[which(indiv_draws$param_name=='lambda')], breaks=100)
hist(indiv_draws$params[which(indiv_draws$param_name=='delta+')], breaks=100)
hist(indiv_draws$params[which(indiv_draws$param_name=='delta-')], breaks=100)
hist(indiv_draws$params[which(indiv_draws$param_name=='psi')], breaks=100)
hist(indiv_draws$params[which(indiv_draws$param_name=='kappa')], breaks=100)
hist(indiv_draws$params[which(indiv_draws$param_name=='kappa')], breaks=seq(0,8000,0.5),xlim = c(0,100))
hist(indiv_draws$params[which(indiv_draws$param_name=='l_anchor')], breaks=100)
hist(indiv_draws$params[which(indiv_draws$param_name=='theta_choice')], breaks=100)

mean(indiv_draws$params[which(indiv_draws$param_name=='kappa')])

for (n_ in 1:max(indiv_draws$n)) {
  print(mean(indiv_draws$params[which(indiv_draws$param_name=='kappa' & indiv_draws$n==n_)])
)
}



###### check BIC

# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'

if(chosen_exp == 'Exp6'){
  K = 32
  N = 9216
}else if(chosen_exp == 'Alex'){
  K = 30
  N = 9000
  
}else if(chosen_exp == 'Divyaj'){
  K = 30
  N = 9000
}

## log seq
fit = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
k = (K+1)*7
total_log_lik_draws <- rowSums(as.matrix(fit, pars = "log_lik"))
mean_log_lik <- mean(total_log_lik_draws)
BIC_approx_log_seq <- -2 * mean_log_lik + k * log(N)

## log seq theta
fit = read_rds(paste('./results/',chosen_exp,'/log_seq_theta.rds', sep = ''), refhook = NULL)
k = (K+1)*8
total_log_lik_draws <- rowSums(as.matrix(fit, pars = "log_lik"))
mean_log_lik <- mean(total_log_lik_draws)
BIC_approx_log_seq_theta <- -2 * mean_log_lik + k * log(N)


## 3values
fit = read_rds(paste('./results/',chosen_exp,'/3values_seq_theta.rds', sep = ''), refhook = NULL)
k = (K+1)*9
total_log_lik_draws <- rowSums(as.matrix(fit, pars = "log_lik"))
mean_log_lik <- mean(total_log_lik_draws)
BIC_approx_log_3_values <- -2 * mean_log_lik + k * log(N)


## BLO choice

fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice.rds',sep = ''))
k = (K+1)*7

posterior_samples <- fit$draws()
posterior_df <- as_draws_df(posterior_samples)
selected_params <- posterior_df[, grepl("^log_lik", colnames(posterior_df))]
total_log_lik_draws <- rowSums(selected_params)
mean_log_lik <- mean(total_log_lik_draws)

BIC_approx_log_BLO_choice <- -2 * mean_log_lik + k * log(N)

# BLO noise choice

fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice.rds',sep = ''))
k = (K+1)*7

posterior_samples <- fit$draws()
posterior_df <- as_draws_df(posterior_samples)
selected_params <- posterior_df[, grepl("^log_lik", colnames(posterior_df))]
total_log_lik_draws <- rowSums(selected_params)
mean_log_lik <- mean(total_log_lik_draws)

BIC_approx_log_BLO_noise_choice <- -2 * mean_log_lik + k * log(N)

BIC_ = c(BIC_approx_log_seq, BIC_approx_log_seq_theta, BIC_approx_log_3_values, BIC_approx_log_BLO_choice, BIC_approx_log_BLO_noise_choice)

name_map <- c( "basic_seq","basic_seq_theta","3values","BLO_choice","BLO_noise_choice")

BIC_compare = data.frame(BIC_, name_map)
colnames(BIC_compare) = c('BIC', 'model')
BIC_compare_sorted <- BIC_compare[order(-BIC_compare$BIC), ]

BIC_df = mutate(BIC_compare_sorted, model = reorder(model, -BIC))

upper_limit = max(BIC_compare$BIC)
lower_limit = min(BIC_compare$BIC)


ggplot(BIC_df, aes(x = model, y = BIC)) +
  geom_col(fill = 'gray50') +
  theme_minimal() +
  ylab('BIC') +
  labs(title = chosen_exp)+
  coord_cartesian(ylim = c(upper_limit, lower_limit))



##### posterior distribution
library(readr)
library(ggplot2)



### log seq

chosen_exp = 'Exp6'
fit = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_alpha_control = post$mu_alpha
mu_beta_control = post$mu_beta

chosen_exp = 'Alex'
fit = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_alpha_mitigation_1 = post$mu_alpha
mu_beta_mitigation_1 = post$mu_beta

chosen_exp = 'Divyaj'
fit = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_alpha_mitigation_2 = post$mu_alpha
mu_beta_mitigation_2 = post$mu_beta


library(tidyr)
library(dplyr)
library(forcats)

posterior_long <- data.frame(
  Experiment_1_alpha = mu_alpha_control,
  Experiment_2_alpha = mu_alpha_mitigation_1,
  Experiment_3_alpha = mu_alpha_mitigation_2,
  Experiment_1_beta  = mu_beta_control,
  Experiment_2_beta  = mu_beta_mitigation_1,
  Experiment_3_beta  = mu_beta_mitigation_2
) %>%
  # pivot both parameters into their own columns simultaneously
  pivot_longer(
    cols = everything(),
    names_to = c("Experiment", ".value"),
    names_pattern = "(Experiment_\\d+)_(.*)"
  ) %>%
  # clean up the data types
  mutate(
    # Convert to factor and explicitly recode the levels
    Experiment = factor(Experiment) %>% 
      fct_recode(
        "Control"      = "Experiment_1",
        "Mitigation 1" = "Experiment_2",
        "Mitigation 2" = "Experiment_3"
      )
  )

bayes_boxplot_stats <- function(x) {
  # Calculate empirical quantiles from the posterior draws
  quants <- quantile(x, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
  
  return(c(
    ymin  = as.numeric(quants[1]), # Lower whisker = 2.5%
    lower = as.numeric(quants[1]), # Lower box edge = 2.5% (overriding 25%)
    middle= as.numeric(quants[3]), # Median line = 50%
    upper = as.numeric(quants[5]), # Upper box edge = 97.5% (overriding 75%)
    ymax  = as.numeric(quants[5])  # Upper whisker = 97.5%
  ))
}


ggplot(posterior_long, aes(x = Experiment, y = alpha, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Alpha"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggplot(posterior_long, aes(x = Experiment, y = beta, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Beta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )






### log seq theta

chosen_exp = 'Exp6'
fit = read_rds(paste('./results/',chosen_exp,'/log_seq_theta.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_alpha_control = post$mu_alpha
mu_beta_control = post$mu_beta
mu_theta_control = post$mu_theta

chosen_exp = 'Alex'
fit = read_rds(paste('./results/',chosen_exp,'/log_seq_theta.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_alpha_mitigation_1 = post$mu_alpha
mu_beta_mitigation_1 = post$mu_beta
mu_theta_mitigation_1 = post$mu_theta

chosen_exp = 'Divyaj'
fit = read_rds(paste('./results/',chosen_exp,'/log_seq_theta.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_alpha_mitigation_2 = post$mu_alpha
mu_beta_mitigation_2 = post$mu_beta
mu_theta_mitigation_2 = post$mu_theta



posterior_long <- data.frame(
  Experiment_1_alpha = mu_alpha_control,
  Experiment_2_alpha = mu_alpha_mitigation_1,
  Experiment_3_alpha = mu_alpha_mitigation_2,
  Experiment_1_beta  = mu_beta_control,
  Experiment_2_beta  = mu_beta_mitigation_1,
  Experiment_3_beta  = mu_beta_mitigation_2,
  Experiment_1_theta  = mu_theta_control,
  Experiment_2_theta  = mu_theta_mitigation_1,
  Experiment_3_theta  = mu_theta_mitigation_2
) %>%
  # pivot both parameters into their own columns simultaneously
  pivot_longer(
    cols = everything(),
    names_to = c("Experiment", ".value"),
    names_pattern = "(Experiment_\\d+)_(.*)"
  ) %>%
  # clean up the data types
  mutate(
    # Convert to factor and explicitly recode the levels
    Experiment = factor(Experiment) %>% 
      fct_recode(
        "Control"      = "Experiment_1",
        "Mitigation 1" = "Experiment_2",
        "Mitigation 2" = "Experiment_3"
      )
  )


ggplot(posterior_long, aes(x = Experiment, y = alpha, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Alpha"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggplot(posterior_long, aes(x = Experiment, y = beta, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Beta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = theta, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Theta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


### 3 internal representations theta


chosen_exp = 'Exp6'
fit = read_rds(paste('./results/',chosen_exp,'/3values_seq_theta.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_distorted_50_control = post$mu_distorted_50
mu_distorted_55_control = post$mu_distorted_55
mu_distorted_65_control = post$mu_distorted_65
mu_theta_control = post$mu_theta

chosen_exp = 'Alex'
fit = read_rds(paste('./results/',chosen_exp,'/3values_seq_theta.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_distorted_50_mitigation_1 = post$mu_distorted_50
mu_distorted_55_mitigation_1 = post$mu_distorted_55
mu_distorted_65_mitigation_1 = post$mu_distorted_65
mu_theta_mitigation_1 = post$mu_theta

chosen_exp = 'Divyaj'
fit = read_rds(paste('./results/',chosen_exp,'/3values_seq_theta.rds', sep = ''), refhook = NULL)
post <- rstan::extract(fit)
mu_distorted_50_mitigation_2 = post$mu_distorted_50
mu_distorted_55_mitigation_2 = post$mu_distorted_55
mu_distorted_65_mitigation_2 = post$mu_distorted_65
mu_theta_mitigation_2 = post$mu_theta


mu_distorted_50_control = post$mu_distorted_50
mu_distorted_55_control = post$mu_distorted_55
mu_distorted_65_control = post$mu_distorted_65
mu_theta_control = post$mu_theta


posterior_long <- data.frame(
  Experiment_1_distorted_50 = mu_distorted_50_control,
  Experiment_2_distorted_50 = mu_distorted_50_mitigation_1,
  Experiment_3_distorted_50 = mu_distorted_50_mitigation_2,
  Experiment_1_distorted_55  = mu_distorted_55_control,
  Experiment_2_distorted_55  = mu_distorted_55_mitigation_1,
  Experiment_3_distorted_55  = mu_distorted_55_mitigation_2,
  Experiment_1_distorted_65  = mu_distorted_65_control,
  Experiment_2_distorted_65  = mu_distorted_65_mitigation_1,
  Experiment_3_distorted_65  = mu_distorted_65_mitigation_2,
  Experiment_1_theta  = mu_theta_control,
  Experiment_2_theta  = mu_theta_mitigation_1,
  Experiment_3_theta  = mu_theta_mitigation_2
) %>%
  # pivot both parameters into their own columns simultaneously
  pivot_longer(
    cols = everything(),
    names_to = c("Experiment", ".value"),
    names_pattern = "(Experiment_\\d+)_(.*)"
  ) %>%
  # clean up the data types
  mutate(
    # Convert to factor and explicitly recode the levels
    Experiment = factor(Experiment) %>% 
      fct_recode(
        "Control"      = "Experiment_1",
        "Mitigation 1" = "Experiment_2",
        "Mitigation 2" = "Experiment_3"
      )
  )


ggplot(posterior_long, aes(x = Experiment, y = distorted_50, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Distorted 50%"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )




ggplot(posterior_long, aes(x = Experiment, y = distorted_55, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Distorted 55%"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = distorted_65, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Distorted 65%"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = theta, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40",
    fill = "gray60",
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Theta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


### BLO choice


chosen_exp = 'Exp6'
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice.rds',sep = ''))
draws = fit$draws(variables = c('mu_pr'))

mu_lambda_control = as.vector(draws[,,1])
mu_delta_plus_control = as.vector(draws[,,2])
mu_psi_control = as.vector(draws[,,4])
mu_kappa_control = as.vector(draws[,,5])
mu_l_anchor_control = as.vector(draws[,,6])
mu_theta_control = as.vector(draws[,,7])

chosen_exp = 'Alex'
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice.rds',sep = ''))
draws = fit$draws(variables = c('mu_pr'))

mu_lambda_mitigation_1 = as.vector(draws[,,1])
mu_delta_plus_mitigation_1 = as.vector(draws[,,2])
mu_psi_mitigation_1 = as.vector(draws[,,4])
mu_kappa_mitigation_1 = as.vector(draws[,,5])
mu_l_anchor_mitigation_1 = as.vector(draws[,,6])
mu_theta_mitigation_1 = as.vector(draws[,,7])

chosen_exp = 'Divyaj'
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice.rds',sep = ''))
draws = fit$draws(variables = c('mu_pr'))

mu_lambda_mitigation_2 = as.vector(draws[,,1])
mu_delta_plus_mitigation_2 = as.vector(draws[,,2])
mu_psi_mitigation_2 = as.vector(draws[,,4])
mu_kappa_mitigation_2 = as.vector(draws[,,5])
mu_l_anchor_mitigation_2 = as.vector(draws[,,6])
mu_theta_mitigation_2 = as.vector(draws[,,7])




posterior_long <- data.frame(
  Experiment_1_lambda = mu_lambda_control,
  Experiment_2_lambda = mu_lambda_mitigation_1,
  Experiment_3_lambda = mu_lambda_mitigation_2,
  Experiment_1_mu_delta_plus = mu_delta_plus_control,
  Experiment_2_mu_delta_plus = mu_delta_plus_mitigation_1,
  Experiment_3_mu_delta_plus = mu_delta_plus_mitigation_2,
  Experiment_1_mu_psi = mu_psi_control,
  Experiment_2_mu_psi = mu_psi_mitigation_1,
  Experiment_3_mu_psi = mu_psi_mitigation_2,
  Experiment_1_mu_kappa = mu_kappa_control,
  Experiment_2_mu_kappa = mu_kappa_mitigation_1,
  Experiment_3_mu_kappa = mu_kappa_mitigation_2,
  Experiment_1_mu_l_anchor = mu_l_anchor_control,
  Experiment_2_mu_l_anchor = mu_l_anchor_mitigation_1,
  Experiment_3_mu_l_anchor = mu_l_anchor_mitigation_2,
  Experiment_1_theta  = mu_theta_control,
  Experiment_2_theta  = mu_theta_mitigation_1,
  Experiment_3_theta  = mu_theta_mitigation_2
) %>%
  # pivot both parameters into their own columns simultaneously
  pivot_longer(
    cols = everything(),
    names_to = c("Experiment", ".value"),
    names_pattern = "(Experiment_\\d+)_(.*)"
  ) %>%
  # clean up the data types
  mutate(
    # Convert to factor and explicitly recode the levels
    Experiment = factor(Experiment) %>% 
      fct_recode(
        "Control"      = "Experiment_1",
        "Mitigation 1" = "Experiment_2",
        "Mitigation 2" = "Experiment_3"
      )
  )







ggplot(posterior_long, aes(x = Experiment, y = lambda, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Lambda"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )




ggplot(posterior_long, aes(x = Experiment, y = mu_delta_plus, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Delta Plus"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = mu_psi, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Psi"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = mu_kappa, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Kappa"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


ggplot(posterior_long, aes(x = Experiment, y = mu_l_anchor, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior L Anchor"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


ggplot(posterior_long, aes(x = Experiment, y = theta, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Theta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


### BLO noise choice

chosen_exp = 'Exp6'
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice.rds',sep = ''))
draws = fit$draws(variables = c('mu_pr'))

mu_lambda_control = as.vector(draws[,,1])
mu_delta_plus_control = as.vector(draws[,,2])
mu_psi_control = as.vector(draws[,,4])
mu_kappa_control = as.vector(draws[,,5])
mu_l_anchor_control = as.vector(draws[,,6])
mu_theta_control = as.vector(draws[,,7])

chosen_exp = 'Alex'
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice.rds',sep = ''))
draws = fit$draws(variables = c('mu_pr'))

mu_lambda_mitigation_1 = as.vector(draws[,,1])
mu_delta_plus_mitigation_1 = as.vector(draws[,,2])
mu_psi_mitigation_1 = as.vector(draws[,,4])
mu_kappa_mitigation_1 = as.vector(draws[,,5])
mu_l_anchor_mitigation_1 = as.vector(draws[,,6])
mu_theta_mitigation_1 = as.vector(draws[,,7])

chosen_exp = 'Divyaj'
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice.rds',sep = ''))
draws = fit$draws(variables = c('mu_pr'))

mu_lambda_mitigation_2 = as.vector(draws[,,1])
mu_delta_plus_mitigation_2 = as.vector(draws[,,2])
mu_psi_mitigation_2 = as.vector(draws[,,4])
mu_kappa_mitigation_2 = as.vector(draws[,,5])
mu_l_anchor_mitigation_2 = as.vector(draws[,,6])
mu_theta_mitigation_2 = as.vector(draws[,,7])




posterior_long <- data.frame(
  Experiment_1_lambda = mu_lambda_control,
  Experiment_2_lambda = mu_lambda_mitigation_1,
  Experiment_3_lambda = mu_lambda_mitigation_2,
  Experiment_1_mu_delta_plus = mu_delta_plus_control,
  Experiment_2_mu_delta_plus = mu_delta_plus_mitigation_1,
  Experiment_3_mu_delta_plus = mu_delta_plus_mitigation_2,
  Experiment_1_mu_psi = mu_psi_control,
  Experiment_2_mu_psi = mu_psi_mitigation_1,
  Experiment_3_mu_psi = mu_psi_mitigation_2,
  Experiment_1_mu_kappa = mu_kappa_control,
  Experiment_2_mu_kappa = mu_kappa_mitigation_1,
  Experiment_3_mu_kappa = mu_kappa_mitigation_2,
  Experiment_1_mu_l_anchor = mu_l_anchor_control,
  Experiment_2_mu_l_anchor = mu_l_anchor_mitigation_1,
  Experiment_3_mu_l_anchor = mu_l_anchor_mitigation_2,
  Experiment_1_theta  = mu_theta_control,
  Experiment_2_theta  = mu_theta_mitigation_1,
  Experiment_3_theta  = mu_theta_mitigation_2
) %>%
  # pivot both parameters into their own columns simultaneously
  pivot_longer(
    cols = everything(),
    names_to = c("Experiment", ".value"),
    names_pattern = "(Experiment_\\d+)_(.*)"
  ) %>%
  # clean up the data types
  mutate(
    # Convert to factor and explicitly recode the levels
    Experiment = factor(Experiment) %>% 
      fct_recode(
        "Control"      = "Experiment_1",
        "Mitigation 1" = "Experiment_2",
        "Mitigation 2" = "Experiment_3"
      )
  )







ggplot(posterior_long, aes(x = Experiment, y = lambda, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Lambda"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )




ggplot(posterior_long, aes(x = Experiment, y = mu_delta_plus, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Delta Plus"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = mu_psi, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Psi"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggplot(posterior_long, aes(x = Experiment, y = mu_kappa, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Kappa"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


ggplot(posterior_long, aes(x = Experiment, y = mu_l_anchor, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40", 
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior L Anchor"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


ggplot(posterior_long, aes(x = Experiment, y = theta, fill = Experiment)) +
  # Base Violin Layer
  geom_violin(
    alpha = 0.3, 
    color = "gray40",
    fill = "gray60",
    scale = "width", 
    adjust = 1.2
  ) +
  # Custom Boxplot Layer via stat_summary
  stat_summary(
    fun.data = bayes_boxplot_stats, 
    geom = "boxplot", 
    width = 0.1,          # Keeps the box elegantly narrow inside the violin
    color = "black",       # High contrast crisp lines
    fill = "white",        # Distinct from the violin fill
    alpha = 0.9            # Forces box to sit cleanly on top
  ) +
  # Styling and Formatting
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = expression(paste("Posterior Theta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


