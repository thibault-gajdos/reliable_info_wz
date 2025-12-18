library(loo)
library(ggplot2)

chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'

# fit_log_basic = read_rds(paste('./results/',chosen_exp,'/log_basic.rds', sep = ''), refhook = NULL)
# fit_log_seq = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
fit_basic = read_rds(paste('./results/',chosen_exp,'/basic.rds', sep = ''), refhook = NULL)
# fit_basic_seq = read_rds(paste('./results/',chosen_exp,'/basic_seq.rds', sep = ''), refhook = NULL)

# log_lik <- extract(fit_basic)$log_lik
# loo_basic_result <- loo(log_lik, r_eff = NA)
loo_basic_result <- loo(fit_basic)

load(paste0('results/',chosen_exp,'/loo/loo_basic_seq.rdata')) 
loo_basic_seq_result <- loo

# 
# loo = loo(fit_log_basic)
# save(loo, file = paste('./results/',chosen_exp,'/loo/loo_log_basic.rdata',sep = ''))

load(paste0('results/',chosen_exp,'/loo/loo_log_basic.rdata')) 
loo_log_basic_result <- loo

load(paste0('results/',chosen_exp,'/loo/loo_log_seq.rdata')) 
loo_log_seq_result <- loo


loo_basic_result$estimates
loo_basic_seq_result$estimates
loo_log_basic_result$estimates
loo_log_seq_result$estimates

comp = loo_compare(loo_basic_result, 
                   loo_basic_seq_result, 
                   loo_log_basic_result,  
                   loo_log_seq_result)




df = data.frame(comp[,c(1,2)])

name_map <- c(
  model1 = "BDT",
  model2 = "Seq.weights",
  model3 = "Rel.distortion",
  model4 = "Full model"
)

model.name = name_map[row.names(comp)]

df = cbind(df, model.name)
df$model.name <- factor(
  df$model.name,
  levels = rev(unique(df$model.name))
)
df$ci_low  <- df$elpd_diff - 1.96 * df$se_diff
df$ci_high <- df$elpd_diff + 1.96 * df$se_diff


ggplot(df, aes(x = model.name, y = elpd_diff)) +
  geom_col(fill = 'gray50') +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.1) +
  theme_minimal() +
  ylab('expected log predictive density difference') +
  labs(title = chosen_exp)


#### sanity check
# 
# elpd_diff_manual <-
#   loo_log_seq_result$pointwise[, "elpd_loo"] -
#   loo_log_basic_result$pointwise[, "elpd_loo"]
# 
# sum(elpd_diff_manual)



