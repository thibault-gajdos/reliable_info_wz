library(readr)

# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
chosen_exp = 'Exp6'



# fit_log_basic = read_rds(paste('./results/',chosen_exp,'/log_basic.rds', sep = ''), refhook = NULL)
fit_log_seq = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)

load(paste("./results/", chosen_exp, "/loo/loo_log_basic.rdata", sep = '')) 
loo_log_basic = loo

# load(paste("./results/", chosen_exp, "/loo/loo_basic.rdata", sep = '')) 
# loo_basic = loo

load(paste("./results/", chosen_exp, "/loo/loo_basic_seq.rdata", sep = '')) 
loo_basic_seq = loo

load(paste("./results/", chosen_exp, "/loo/loo_log_seq.rdata", sep = '')) 
loo_log_seq = loo


# loo_basic$estimates
loo_basic_seq$estimates
loo_log_basic$estimates
loo_log_seq$estimates

#### extract the fit

########

library(ggplot2)

# Extract posterior samples
post <- rstan::extract(fit_log_seq)

# Participant-level posterior means
a_mean <- apply(post$alpha, 2, mean)
b_mean <- apply(post$beta, 2, mean)
w_mean <- apply(post$w, c(2,3), mean)


# Group means
a_g <- mean(a_mean)
b_g <- mean(b_mean)

a_g
b_g

w_g <- apply(w_mean, 2, mean)

w_g <- c(w_g, 1) # add 1 at the end


gamma_g <- exp(b_g)

# Objective reliability grid
p_grid <- seq(0.001, 0.999, length.out = 200)

# Distortion function (Equation 6)
phi <- function(p, a, gamma) {
  (gamma * p^a) / (gamma * p^a + (1 - p)^a)
}

distorted <- phi(p_grid, a_g, gamma_g)

df <- data.frame(
  p = p_grid * 100,
  distorted = distorted * 100
)

ggplot(df, aes(x = p, y = distorted)) +
  geom_line(size = 1.3, color = "#0072B2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = chosen_exp,
    x = "Objective reliability (%)",
    y = "Subjective reliability (%)"
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 15)

##### plot w
df <- data.frame(
  no.seq = c(1:6),
  weights = w_g
)

ggplot(df, aes(x = no.seq, y = weights)) + 
  geom_line(size = 1.3) +
  labs(
    title = chosen_exp,
    x = "Order in the sequence",
    y = "Sequential weights"
  ) +
  scale_x_continuous(limits = c(1, 6)) +
  scale_y_continuous(limits = c(0.5, 1.1))

  
