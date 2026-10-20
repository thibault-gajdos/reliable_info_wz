rm(list=ls())
load('./results/data_preprocessed.rdata')

### Prepare the data for cmdstan
library(jsonlite)
library(dplyr)

results = lapply(data_processed$Stimulus.Info, function(txt) {
  d = fromJSON(txt)
  # colors = ifelse(d$color == "blue", 1, 0)
  colors = d$color
  blue_rel = d$reliability*100
  list(
    color_1 = colors[1],
    color_2 = colors[2],
    color_3 = colors[3],
    color_4 = colors[4],
    color_5 = colors[5],
    color_6 = colors[6],
    proba_1 = blue_rel[1],
    proba_2 = blue_rel[2],
    proba_3 = blue_rel[3],
    proba_4 = blue_rel[4],
    proba_5 = blue_rel[5],
    proba_6 = blue_rel[6]
  )
})

results = bind_rows(results)

data_processed = cbind(data_processed, results)

data_processed = data_processed[,c(1:37)]

# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'
chosen_exp = 'mitigation3'





data = data_processed[which(data_processed$Done.by==chosen_exp),]


data <- data %>%
  mutate(choice = case_when(
    (Response == 0) ~ 2, # red 0 -> 2
    (Response == 1) ~ 1  # blue 1 -> 1
  )) %>% mutate_at(vars(starts_with("color")), ~ ifelse(. == 'blue', 1, 2))


data <- data %>%
  mutate(optimal_ = case_when(
    chosen_exp %in% c('Alex', 'Exp6') & Correct.Response == 0 ~ 2,
    chosen_exp %in% c('Alex', 'Exp6') & Correct.Response == 1 ~ 1,
    !chosen_exp %in% c('Alex', 'Exp6') & Optimalchoice == 0 ~ 2,
    !chosen_exp %in% c('Alex', 'Exp6') & Optimalchoice == 1 ~ 1
  ))



N = length(unique(data$Participant.Private.ID))
T_max = max(table(data[[1]]))
I =  length(unlist(data$blue_rel[1])) ## number of samples/trial

## compute trials by subject
d <- data %>%
  group_by(Participant.Private.ID) %>%
  summarise(t_subjs = n())
t_subjs <- d$t_subjs
subjs <- unique(data$Participant.Private.ID)


## Initialize data arrays
choice  <- array(-1, c(N, T_max))
optimal <- array(-1, c(N, T_max))
color <- array( -1, c(N, T_max, I))
level <- array(-1, c(N, T_max, I))
sample <- array(6, c(N, T_max))
## fill the  arrays
for (n in 1:N) {
  t <- t_subjs[n] ## number of trials for subj i
  data_subj <- data %>% filter(Participant.Private.ID == subjs[n])
  choice[n, 1:t] <- data_subj$choice
  optimal[n, 1:t] <- data_subj$optimal_

  for (k in 1:t) {
    for (i in 1:I) {
      color_var <- paste0("color_", i)
      proba_var <- paste0("proba_", i)
      color[n, k, i] <- data_subj[[color_var]][k]
      level[n, k, i] <- data_subj[[proba_var]][k]
    }
  }
}


data_list <- list(
  N = N,
  T_max = T_max,
  I = I,
  Tsubj = t_subjs,
  # sample = sample,
  color = color,
  level = level,
  choice = choice,
  feedback = optimal)

data_list$color  <- apply(data_list$color, 1:3, as.integer)
data_list$level  <- apply(data_list$level, 1:3, as.integer)
data_list$choice <- apply(data_list$choice, 1:2, as.integer)
data_list$feedback <- apply(data_list$feedback, 1:2, as.integer)

# init_fn <- function() {
#   list(
#     mu_pr      = c(0, 0, 0),
#     sigma      = c(0.1, 0.1, 0.1),
#     alpha_raw  = rep(0, N),
#     theta_raw  = rep(0, N),
#     lambda_raw = rep(0, N),
#     V50_init   = rep(0.1, N),
#     V55_init   = rep(0.1, N),
#     V65_init   = rep(0.1, N)
#   )
# }



#####################################################
##  FIT THE MODEL
####################################################

# model_name = 'simplified'
# model_name = 'theta1'
# model_name = 'theta5'
model_name = 'theta1_seq_weights'
# model_name = 'theta5_seq_weights'

if(model_name == 'simplified'){
  stan_file_location = './stan/rl_credit_assignment_simplified.stan'
  file_name = 'rl'
}else if(model_name == 'theta1'){
  stan_file_location = './stan/rl_theta1.stan'
  file_name = 'rl_theta1'
}else if(model_name == 'theta5'){
  stan_file_location = './stan/rl_theta5.stan'
  file_name = 'rl_theta5'
}else if(model_name == 'theta5_seq_weights'){
  stan_file_location = './stan/rl_theta5_seqweights.stan'
  file_name = 'rl_theta5_seq'
}else if(model_name == 'theta1_seq_weights'){
  stan_file_location = './stan/rl_theta1_seqweights.stan'
  file_name = 'rl_theta1_seq'
}


library(cmdstanr)
library(loo)
library(readr)

data_list$grainsize = 5 ## specify grainsize for within chain parallelization

## Compile the model
model <- cmdstan_model(
  stan_file = stan_file_location,
  force_recompile = TRUE, ## necessary if you change the mode
  # cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE), ## within chain parallel
  cpp_options = list(stan_threads = TRUE), ## within chain parallel
  stanc_options = list("O1"), ## fastest sampling
  compile_model_methods = TRUE ## necessary for loo moment matching
)

## Sampling
fit <- model$sample(
  data = data_list,
  ##seed = 1234,
  seed = 4321,
  ##init = list(inits_chain,inits_chain, inits_chain,inits_chain),
  chains = 4,
  parallel_chains = 4,
  threads_per_chain = 5,
  iter_warmup = 2000,
  iter_sampling = 2000,
  max_treedepth = 12,
  adapt_delta = .9,
  save_warmup = FALSE
)


fit$save_object(file = paste('./results/',chosen_exp,'/',file_name,'.rds',sep = ''))

loo <- fit$loo(cores = 10, moment_match = TRUE)
save(loo, file = paste('./results/',chosen_exp,'/loo/loo_',file_name,'.rds',sep = ''))

loo$estimates

plot(loo)  # visual overview of pareto-k values
pareto_k_table(loo)


#diagnostic

model <- cmdstan_model(
  stan_file = './stan/rl_credit_assignment_simplified.stan',
  force_recompile = TRUE, ## necessary if you change the mode
  # cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE), ## within chain parallel
  cpp_options = list(stan_threads = TRUE), ## within chain parallel
  stanc_options = list("O1"), ## fastest sampling
  compile_model_methods = TRUE ## necessary for loo moment matching
)

init_fn <- function() {
  list(
    mu_pr      = c(0, 0),        # was c(0, 0, 0) — now only alpha, theta
    sigma      = c(0.1, 0.1),    # was c(0.1, 0.1, 0.1)
    alpha_raw  = rep(0, N),
    theta_raw  = rep(0, N)
    # removed lambda_raw, V50_init, V55_init, V65_init
  )
}

fit_test <- model$sample(
  data = data_list,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 5,
  iter_warmup = 100,
  iter_sampling = 100,
  max_treedepth = 12,
  adapt_delta = 0.8,
  save_warmup = FALSE
)

fit_test$diagnostic_summary()

fit_test$sampler_diagnostics() |>
  as.data.frame() |>
  subset(select = "treedepth__") |>
  table()

diag_df <- as.data.frame(fit_test$sampler_diagnostics())
table(diag_df$treedepth__)
