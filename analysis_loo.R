# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'

fit_log_basic = read_rds(paste('./results/',chosen_exp,'/log_basic.rds', sep = ''), refhook = NULL)
fit_log_seq = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
fit_basic = read_rds(paste('./results/',chosen_exp,'/basic.rds', sep = ''), refhook = NULL)
fit_basic_seq = read_rds(paste('./results/',chosen_exp,'/basic_seq.rds', sep = ''), refhook = NULL)

# log_lik <- extract(fit_basic)$log_lik
# loo_basic_result <- loo(log_lik, r_eff = NA)
loo_basic_result <- loo(fit_basic)

loo_basic_seq_result <- loo(fit_basic_seq)

loo_log_basic_result <- loo(fit_log_basic)

loo_log_seq_result <- loo(fit_log_seq)


loo_basic_result$estimates[1,1]
loo_basic_seq_result$estimates[1,1]
loo_log_basic_result$estimates[1,1]
loo_log_seq_result$estimates[1,1]

df <- data.frame(
  model.name = c('BDT', 'Seq.weights','Rel.distortion','Full model'),
  elpd_ = c(loo_basic_result$estimates[1,1],
              loo_basic_seq_result$estimates[1,1],
              loo_log_basic_result$estimates[1,1],
              loo_log_seq_result$estimates[1,1]),
  SE = c(loo_basic_result$estimates[1,2],
         loo_basic_seq_result$estimates[1,2],
         loo_log_basic_result$estimates[1,2],
         loo_log_seq_result$estimates[1,2])
)

df$model.name = factor(df$model.name, levels = c(df$model.name))
df$ci_low  <- df$elpd_ - 1.96 * df$SE
df$ci_high <- df$elpd_ + 1.96 * df$SE

library(ggplot2)


ggplot(df, aes(x = model.name, y = elpd_)) +
  geom_col(fill = 'gray') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.1) +
  theme_minimal() +
  labs(
    title = chosen_exp)

