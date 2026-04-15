library(posterior)
library(ggplot2)
library(dplyr)
rm(list=ls())

# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
chosen_exp = 'Exp6'

# additional_suffix = '_original_edit'
# additional_suffix = '_redo'
additional_suffix = ''

fit <- readRDS(paste('./results/',chosen_exp,'/log_full',additional_suffix,'.rds',sep = ''))

fit$variable_skeleton()
draws = fit$draws(variables = c('mu_alpha','mu_beta','mu_lambda','mu_theta','mu_psi'))
df_draws = as_draws_df(draws)

# fit$summary()$variable


raw_draws = fit$draws(variables = c('params'))
df_raw = as_draws_df(raw_draws)
df_raw = df_raw[,1:150]





###### start of analysis

alpha_mean = mean(df_draws$mu_alpha)
beta_mean = mean(df_draws$mu_beta)
lambda_mean = mean(df_draws$mu_lambda)
theta_mean = mean(df_draws$mu_theta)
psi_mean = mean(df_draws$mu_psi)


par(mfrow = c(1,5))
hist(df_draws$mu_alpha)
abline(b = alpha_mean)
hist(df_draws$mu_beta)
abline(b = mu_beta)
hist(df_draws$mu_lambda)
abline(b = mu_lambda)
hist(df_draws$mu_theta)
abline(b = mu_theta)
hist(df_draws$mu_psi)
abline(b = mu_psi)

##### plot distortion


# Objective reliability grid
p_grid <- seq(0.001, 0.999, length.out = 200)

logit <- function(p){
  l = log(p/(1-p))
  return(l)
}

post_means = c(alpha_mean, beta_mean, lambda_mean, theta_mean, psi_mean)

# distortion function
calc_distorted_p <- function(p_actual, params) {
  log_odds <- params[1] * params[5] * logit(p_actual) + (1 - params[1]) * params[2]
  distorted_p <- 1/(1+exp(-log_odds))

  return(distorted_p)
}

plot_df <- data.frame(p = seq(0, 1, length.out = 200)) %>%
  mutate(distorted = sapply(p, calc_distorted_p, post_means))

plot_df$p = plot_df$p*100
plot_df$distorted = plot_df$distorted*100


highlight <- data.frame(
  p = c(0.5, 0.55, 0.65)
)
highlight$distorted <- calc_distorted_p(highlight$p, post_means)
highlight$p = highlight$p*100
highlight$distorted = highlight$distorted*100


ggplot(plot_df, aes(x = p, y = distorted)) +
  # main sigmoid curve
  geom_line(linewidth = 1.6, colour = "black") +
  # identity line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # vertical guide lines
  geom_segment(
    data = highlight,
    aes(x = p, xend = p, y = 0, yend = distorted, col = '#F2F2F2'),
    linewidth = 0.6
  ) +
  # horizontal guide lines
  geom_segment(
    data = highlight,
    aes(x = 0, xend = p, y = distorted, yend = distorted, col = '#F2F2F2'),
    linewidth = 0.6
  ) +
  # highlighted points
  geom_point(
    data = highlight,
    aes(colour = factor(p)),
    size = 5
  ) +
  scale_colour_manual(
    values = c("50" = "#0099FF",
               "55" = "#00CC33",
               "65" = "#009933")
  ) +
  scale_x_continuous(
    limits = c(0, 100), breaks = c(0, 50, 55, 65, 100),
    expand = c(0, 1)
  ) +
  scale_y_continuous(
    limits = c(0, 100), breaks = c(0, 50, 65, 80, 100),
    expand = c(0, 1)
  )+
  labs(
    x = "Reliability",
    y = "Distorted reliability"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 1),
    axis.ticks.length = unit(6, "pt")
  )
##### plot matrix
library(bayesplot)
summary(draws)
mcmc_pairs(draws, 
           pars = c("mu_alpha", "mu_beta", "mu_lambda","mu_theta","mu_psi"), # Choose specific params
           off_diag_fun = "hex",               # "hex" or "scatter"
           diag_fun = "hist")                  # "hist" or "dens"

##### plot seq.weights
df.seq <- data.frame(
  no.seq = c(1:6),
  weights = exp(lambda_mean * (c(1:6) - 6))
)

df.seq

ggplot(df.seq, aes(x = no.seq, y = weights)) +
  # 1. Add vertical lines from the x-axis to the points
  geom_segment(aes(xend = no.seq, yend = 0.5), color = "grey70") +
  # 2. Add the main line
  geom_line(linewidth = 1.3) +
  # 3. Add the large circular points
  geom_point(size = 5, color = "grey50") +
  # 4. Add the horizontal dashed reference line at y = 1.0
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "grey30") +
  labs(
    x = NULL, # The second image removes the x-axis title
    y = NULL  # The second image removes the y-axis title
  ) +
  # 5. Define discrete-like labels for the continuous x-axis
  scale_x_continuous(
    breaks = 1:6, 
    labels = c("1st", "2nd", "3rd", "4th", "5th", "6th"),
    limits = c(0.8, 6.2), # Add a little padding on the sides
    expand = c(0, 0)
  ) +
  # 6. Set y-axis limits and breaks
  scale_y_continuous(
    breaks = seq(0.6, 1.1, by = 0.1),
    limits = c(0.55, 1.1),
    expand = c(0, 0)
  ) +
  # 7. Apply a clean theme and remove the grid
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.8),
    axis.ticks = element_line(size = 0.8),
    axis.text = element_text(color = "black", size = 12),
    plot.title = element_blank() # Removing title to match the goal image
  )

##### introspective
library(tidybayes)
library(dplyr)
indiv_draws <- fit %>%
  spread_draws(params[n, v])

indiv_draws <- indiv_draws %>%
  mutate(param_name = case_when(
    v == 1 ~ "alpha",
    v == 2 ~ "beta",
    v == 3 ~ "lambda",
    v == 4 ~ "theta",
    v == 5 ~ "psi"
  ))

indiv_summary <- indiv_draws %>%
  group_by(n, param_name) %>%
  summarise(
    mean_val = mean(params),
    lower_95 = quantile(params, 0.025),
    upper_95 = quantile(params, 0.975)
  )

print(indiv_summary)
