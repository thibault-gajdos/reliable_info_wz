# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'
chosen_exp = 'mitigation3'

#### extract the fit

########

# Extract posterior samples
# fit = read_rds(paste('./results/',chosen_exp,'/rl.rds', sep = ''), refhook = NULL)
# fit = read_rds(paste('./results/',chosen_exp,'/rl_theta1.rds', sep = ''), refhook = NULL)
# fit = read_rds(paste('./results/',chosen_exp,'/rl_theta5.rds', sep = ''), refhook = NULL)
# fit = read_rds(paste('./results/',chosen_exp,'/rl_theta1_seq.rds', sep = ''), refhook = NULL)
fit = read_rds(paste('./results/',chosen_exp,'/rl_theta5_seq.rds', sep = ''), refhook = NULL)

# group_draws = fit$draws(variables = c('mu_delta', 'mu_theta','V50_init', 'V55_init', 'V65_init'))
# group_draws = fit$draws(variables = c('mu_delta','V50_init', 'V55_init', 'V65_init'))
group_draws = fit$draws(variables = c('mu_delta','mu_lambda','V50_init', 'V55_init', 'V65_init'))
group_draws = as_draws_df(group_draws)

# inidiv_draws = fit$draws(variables = c('delta','theta'))
# inidiv_draws = fit$draws(variables = c('delta'))
inidiv_draws = fit$draws(variables = c('delta', 'lambda'))
inidiv_draws = as_draws_df(inidiv_draws)

group_summary <- summarise_draws(group_draws) 



if(chosen_exp == 'Alex'){
  exp_name = 'Mitigation 1'
}else if(chosen_exp == 'Exp6'){
  exp_name = 'Control'
}else if(chosen_exp == 'Divyaj'){
  exp_name = 'Mitigation 2'
}else if(chosen_exp == 'mitigation3'){
  exp_name = 'Mitigation 3'
}

group_draws$exp_name = exp_name


# df_params = data.frame()

df_params = rbind(df_params, group_draws)




#### plotting
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



#### new function

bayes_boxplot_stats <- function(x) {
  ci <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  data.frame(
    ymin   = ci[[1]],
    lower  = ci[[2]],
    middle = ci[[3]],
    upper  = ci[[4]],
    ymax   = ci[[5]]
  )
}
ggplot(df_params, aes(x = exp_name, y = mu_delta)) +
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
    y = expression(paste("Posterior Delta"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


ggplot(df_params, aes(x = exp_name, y = mu_theta)) +
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

ggplot(df_params, aes(x = exp_name, y = mu_lambda)) +
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



library(tidyr)

df_long <- df_params %>%
  pivot_longer(
    cols = c(V50_init, V55_init, V65_init),
    names_to = "V_type",
    values_to = "value"
  )
ggplot(df_long, aes(x = exp_name, y = value, fill = V_type, 
                    group = interaction(exp_name, V_type))) +
  geom_violin(
    alpha = 0.6,
    scale = "width",
    adjust = 1.2,
    position = position_dodge(width = 0.9)
  ) +scale_fill_manual(values = c(
    "V50_init" = "#0099FF",   # orange
    "V55_init" = "#00FF00",   # sky blue
    "V65_init" = "#009933"    # green
  ))+ # my_colors = c('#0099FF', '#00CC33', '#009933')

  geom_boxplot(
    width = 0.2,
    color = "black",
    fill = "white",
    outlier.shape = NA,
    position = position_dodge(width = 0.9)
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Posterior Distribution Across Experiments",
    subtitle = "Violin plots with 95% Credible Intervals",
    x = "Experimental Dataset",
    y = "Posterior V_init",
    fill = "Type"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10.5, hjust = 0.5, color = "gray30"),
    panel.grid.minor = element_blank()
  )





####### loo comparison
library(loo)

# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'
chosen_exp = 'mitigation3'




load(paste0('results/',chosen_exp,'/loo/loo_rl.rds')) 
loo_base <- loo
pareto_k_table(loo_base)

load(paste0('results/',chosen_exp,'/loo/loo_rl_theta1.rds')) 
loo_t1<- loo
pareto_k_table(loo_t1)


load(paste0('results/',chosen_exp,'/loo/loo_rl_theta5.rds')) 
loo_t5 <- loo
pareto_k_table(loo_t5)


load(paste0('results/',chosen_exp,'/loo/loo_rl_theta1_seq.rds')) 
loo_t1s <- loo
pareto_k_table(loo_t1s)


load(paste0('results/',chosen_exp,'/loo/loo_rl_theta5_seq.rds')) 
loo_t5s <- loo
pareto_k_table(loo_t5s)







comp = loo_compare(loo_base, 
                   loo_t1, 
                   loo_t5,
                   loo_t1s,
                   loo_t5s
)


df = data.frame(comp[,c(1,2)])

name_map <- c(
  model1 = "base",
  model2 = "theta1",
  model3 = "theta5",
  model4 = "theta1_seq.weights",
  model5 = "theta5_seq.weights"
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




