library(posterior)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(bayesplot)

rm(list=ls())

# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
chosen_exp = 'Exp6'

# additional_suffix = '_original_edit'
# additional_suffix = '_redo'
additional_suffix = ''

# fit <- readRDS(paste('./results/',chosen_exp,'/BLO_choice',additional_suffix,'.rds',sep = ''))
fit <- readRDS(paste('./results/',chosen_exp,'/BLO_noise_choice',additional_suffix,'.rds',sep = ''))

fit$variable_skeleton()
draws = fit$draws(variables = c('params'))
df_draws = as_draws_df(draws)

draws = fit$draws()
summary(draws)

## extract means
df_summary <- summarise_draws(df_draws) 

df_param_means <- df_summary %>%
  filter(str_detect(variable, "params\\[")) %>%
  mutate(
    param_idx = as.numeric(str_match(variable, "params\\[\\d+,(\\d+)\\]")[,2])
  ) %>%
  group_by(param_idx) %>%
  summarize(
    overall_mean = mean(mean),
    sd_between_subjects = sd(mean),
    n_subjects = n()
  ) %>%
  mutate(param_name = case_when(
    param_idx == 1 ~ "lambda",
    param_idx == 2 ~ "delta+",
    param_idx == 3 ~ "delta-",
    param_idx == 4 ~ "psi",
    param_idx == 5 ~ "kappa",
    param_idx == 6 ~ "l_anchor",
    param_idx == 7 ~ "theta_choice"
  ))

print(df_param_means)

lambda_mean = df_param_means$overall_mean[1]
delta_plus_mean = df_param_means$overall_mean[2]
delta_minus_mean = df_param_means$overall_mean[3]
psi_mean = df_param_means$overall_mean[4]
kappa_mean = df_param_means$overall_mean[5]
l_anchor_mean = df_param_means$overall_mean[6]
theta_mean = df_param_means$overall_mean[7]



######## calculate subjective probability

logit <- function(p) {
  log(p / (1 - p))
}

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


# BLO Core
calc_distorted_p <- function(p, Delta_plus, Delta_minus, Psi, kappa, l_anchor) {
  # reliability weight w(p)
  wp <- 1 / (1 + kappa * p * (1 - p))
  
  # Gamma truncation
  l <- logit(p)
  G <- pmax(pmin(l, Delta_plus), Delta_minus)
  
  # Lambda transformation
  denom <- sqrt((Delta_plus - Delta_minus)^2 + 1e-12)
  L <- (2 * Psi / denom) * (G - 0.5 * (Delta_plus + Delta_minus))
  
  m <- wp * L + (1 - wp) * l_anchor
  
  distorted_p <- inv_logit(m)
  return(distorted_p)
}

p_test <- c(0.5, 0.55, 0.65)

subj_p <- calc_distorted_p(
  p = p_test,
  Delta_plus  = delta_minus_mean,
  Delta_minus = delta_minus_mean,
  Psi         = psi_mean,
  kappa       = kappa_mean,
  l_anchor    = l_anchor_mean
)

print(paste("Original P:", p_test))
print(paste("Distorted P:", round(subj_p, 4)))


# Objective reliability grid
p_grid <- seq(0.001, 0.999, length.out = 200)

# Distortion function (Equation 6)

distorted <- calc_distorted_p(
  p = p_grid,
  Delta_plus  = delta_plus_mean,
  Delta_minus = delta_minus_mean,
  Psi         = psi_mean,
  kappa       = kappa_mean,
  l_anchor    = l_anchor_mean
)



df <- data.frame(
  p = p_grid * 100,
  distorted = distorted * 100
)


highlight <- data.frame(
  p = c(0.5, 0.55, 0.65)
)

highlight$distorted <- calc_distorted_p(
  p = highlight$p,
  Delta_plus  = delta_plus_mean,
  Delta_minus = delta_minus_mean,
  Psi         = psi_mean,
  kappa       = kappa_mean,
  l_anchor    = l_anchor_mean
)


highlight$p = highlight$p*100
highlight$distorted = highlight$distorted*100



ggplot(df, aes(x = p, y = distorted)) +
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


#### Slope of introspective against distorted reliability
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

indiv_summary <- indiv_draws %>%
  group_by(n, param_name) %>%
  summarise(
    mean_val = mean(params),
    lower_95 = quantile(params, 0.025),
    upper_95 = quantile(params, 0.975)
  )

print(indiv_summary)

indiv_delta_plus = unlist(indiv_summary[which(indiv_summary$param_name=='delta+'),3], use.names=FALSE)
indiv_delta_minus = unlist(indiv_summary[which(indiv_summary$param_name=='delta-'),3], use.names=FALSE)
indiv_kappa = unlist(indiv_summary[which(indiv_summary$param_name=='kappa'),3], use.names=FALSE)
indiv_l_anchor = unlist(indiv_summary[which(indiv_summary$param_name=='l_anchor'),3], use.names=FALSE)
indiv_lambda = unlist(indiv_summary[which(indiv_summary$param_name=='lambda'),3], use.names=FALSE)
indiv_psi = unlist(indiv_summary[which(indiv_summary$param_name=='psi'),3], use.names=FALSE)
indiv_theta = unlist(indiv_summary[which(indiv_summary$param_name=='theta_choice'),3], use.names=FALSE) # this is currently not used, but maybe correlated with performance in some ways



distorted_50 = calc_distorted_p(
  p = 0.5,
  Delta_plus  = indiv_delta_plus,
  Delta_minus = indiv_delta_minus,
  Psi         = indiv_psi,
  kappa       = indiv_kappa,
  l_anchor    = indiv_l_anchor
)

distorted_55 = calc_distorted_p(
  p = 0.55,
  Delta_plus  = indiv_delta_plus,
  Delta_minus = indiv_delta_minus,
  Psi         = indiv_psi,
  kappa       = indiv_kappa,
  l_anchor    = indiv_l_anchor
)

distorted_65 = calc_distorted_p(
  p = 0.65,
  Delta_plus  = indiv_delta_plus,
  Delta_minus = indiv_delta_minus,
  Psi         = indiv_psi,
  kappa       = indiv_kappa,
  l_anchor    = indiv_l_anchor
)


load('./results/data_preprocessed.rdata')

data_processed$slider_reliability = as.factor(data_processed$slider_reliability)

data_chosen = data_processed[which(data_processed$Done.by==chosen_exp),]

data_chosen$Net.Cong.50 = data_chosen$Net.Blue.50
data_chosen$Net.Cong.55 = data_chosen$Net.Blue.55
data_chosen$Net.Cong.65 = data_chosen$Net.Blue.65

data_chosen$Net.Cong.50[which(data_chosen$Response==0)] = 0 - data_chosen$Net.Cong.50[which(data_chosen$Response==0)]
data_chosen$Net.Cong.55[which(data_chosen$Response==0)] = 0 - data_chosen$Net.Cong.55[which(data_chosen$Response==0)]
data_chosen$Net.Cong.65[which(data_chosen$Response==0)] = 0 - data_chosen$Net.Cong.65[which(data_chosen$Response==0)]

data_chosen$slider_response = as.numeric(data_chosen$slider_response)-50


slope_50 = c()
slope_55 = c()
slope_65 = c()


for (id_ in unique(data_chosen$Participant.Private.ID)){
  data_participant = data_chosen[which(data_chosen$Participant.Private.ID == id_),]
  lm.slider = lm(slider_response ~  slider_reliability * Net.Cong.50 + slider_reliability * Net.Cong.55 + slider_reliability * Net.Cong.65, data = data_participant)
  slope_50 = c(slope_50, lm.slider$coefficients[4])
  slope_55 = c(slope_55, lm.slider$coefficients[5]+lm.slider$coefficients[9])
  slope_65 = c(slope_65, lm.slider$coefficients[6]+lm.slider$coefficients[12])
}

df50 <- data.frame(
  distorted_reliability = distorted_50*100,
  slope_ = slope_50,
  reliability = '50'
)

df55 <- data.frame(
  distorted_reliability = distorted_55*100,
  slope_ = slope_55,
  reliability = '55'
)

df65 <- data.frame(
  distorted_reliability = distorted_65*100,
  slope_ = slope_65,
  reliability = '65'
)

df_all = rbind(df50, df55, df65)

my_colors = c('#0099FF', '#00CC33', '#009933')

ggplot(df_all, aes(distorted_reliability, slope_, col = reliability))+
  geom_point() +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(limits = c(-10, 20)) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title = chosen_exp,
       x = "Distorted Reliability",
       y = "Slope of introspective reports",
       color = "Reliability") +
  theme_bw()


library(patchwork)
#################  50

fit <- lm(slope_ ~ I(distorted_reliability^2), data = df50)
summary(fit)
grid <- data.frame(distorted_reliability = seq(min(df50$distorted_reliability), max(df50$distorted_reliability), length.out = 20))
preds <- predict(fit, newdata = grid, interval = "confidence")
plot_data <- cbind(grid, preds)


plot.50 = ggplot(df50, aes(distorted_reliability, slope_)) +
  geom_point(shape = 21, fill = "#0099FF", size = 1) +
  # The mean prediction line
  geom_line(data = plot_data, aes(y = fit), color = "red", size = 1) +
  # The dashed confidence interval lines
  geom_line(data = plot_data, aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = plot_data, aes(y = upr), color = "red", linetype = "dashed") +
  # Reference lines and styling
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(
    limits = c(-10, 20),
    expand = c(0.005, 0.005)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = chosen_exp,
       x = "Distorted reliability of 50% reliable sources",
       y = "Slope of introspective reports")
labs(title = chosen_exp, 
     x = " ", 
     y = " ")

###############  55

fit <- lm(slope_ ~ I(distorted_reliability^2), data = df55)
summary(fit)

grid <- data.frame(distorted_reliability = seq(min(df55$distorted_reliability), max(df55$distorted_reliability), length.out = 20))
preds <- predict(fit, newdata = grid, interval = "confidence")
plot_data <- cbind(grid, preds)

plot.55 = ggplot(df55, aes(distorted_reliability, slope_)) +
  geom_point(shape = 21, fill = "#00CC33", size = 1) +
  # The mean prediction line
  geom_line(data = plot_data, aes(y = fit), color = "red", size = 1) +
  # The dashed confidence interval lines
  geom_line(data = plot_data, aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = plot_data, aes(y = upr), color = "red", linetype = "dashed") +
  # Reference lines and styling
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(
    limits = c(-10, 20),
    expand = c(0.005, 0.005)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = chosen_exp,
       x = "Distorted reliability of 55% reliable sources",
       y = "Slope of introspective reports")
labs(title = chosen_exp, 
     x = " ", 
     y = " ")


###############  65

fit <- lm(slope_ ~ I(distorted_reliability^2), data = df65)
summary(fit)
grid <- data.frame(distorted_reliability = seq(min(df65$distorted_reliability), max(df65$distorted_reliability), length.out = 20))
preds <- predict(fit, newdata = grid, interval = "confidence")
plot_data <- cbind(grid, preds)

plot.65 = ggplot(df65, aes(distorted_reliability, slope_)) +
  geom_point(shape = 21, fill = "#009933", size = 1) +
  # The mean prediction line
  geom_line(data = plot_data, aes(y = fit), color = "red", size = 1) +
  # The dashed confidence interval lines
  geom_line(data = plot_data, aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = plot_data, aes(y = upr), color = "red", linetype = "dashed") +
  # Reference lines and styling
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(
    limits = c(-10, 20),
    expand = c(0.005, 0.005)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = chosen_exp,
       x = "Distorted reliability of 55% reliable sources",
       y = "Slope of introspective reports")
labs(title = chosen_exp, 
     x = " ", 
     y = " ")

plot.50 + plot.55 + plot.65

#### theta distribution

hist(indiv_draws$params[which(indiv_draws$param_name=='theta_choice')], breaks=seq(0,1000,0.5),xlim = c(0,20))

## do this in boxplot


lm()

lambda_mean = df_param_means$overall_mean[1]
delta_plus_mean = df_param_means$overall_mean[2]
delta_minus_mean = df_param_means$overall_mean[3]
psi_mean = df_param_means$overall_mean[4]
kappa_mean = df_param_means$overall_mean[5]
l_anchor_mean = df_param_means$overall_mean[6]
theta_mean = df_param_means$overall_mean[7]