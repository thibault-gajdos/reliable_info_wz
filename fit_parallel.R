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
chosen_exp = 'Exp6'


additional_suffix = '_original_edit'
# additional_suffix = '_redo'
# additional_suffix = ''



data = data_processed[which(data_processed$Done.by==chosen_exp),]


data <- data %>%
  mutate(choice = case_when(
    (Response == 0) ~ 2, # red 0 -> 2
    (Response == 1) ~ 1  # blue 1 -> 1
  )) %>% mutate_at(vars(starts_with("color")), ~ ifelse(. == 'blue', 1, 2))




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
color <- array( -1, c(N, T_max, I))
proba <- array(-1, c(N, T_max, I))
sample <- array(6, c(N, T_max))
## fill the  arrays
for (n in 1:N) {
  t <- t_subjs[n] ## number of trials for subj i
  data_subj <- data %>% filter(Participant.Private.ID == subjs[n])
  choice[n, 1:t] <- data_subj$choice
  for (k in 1:t) {
    for (i in 1:I) {
      color_var <- paste0("color_", i)
      proba_var <- paste0("proba_", i)
      color[n, k, i] <- data_subj[[color_var]][k]
      proba[n, k, i] <- data_subj[[proba_var]][k]/100
    }
  }
}
# 
# l_obs = 1/(1+exp(-max(proba)))
# 
p = max(proba)
l_obs = log(p/(1-p))

data_list <- list(
  N = N,
  T_max = T_max,
  I_max = I,
  Tsubj = t_subjs,
  color = color,
  proba = proba,
  choice = choice,
  sample = sample,
  l_obs = l_obs)


#####################################################
##  FIT THE MODEL
####################################################

library(cmdstanr)
library(loo)
library(readr)

data_list$grainsize = 5 ## specify grainsize for within chain parallelization

## Compile the model
model <- cmdstan_model(
  stan_file = './stan/log_trunc_simplified_original_edit.stan',
  force_recompile = TRUE, ## necessary if you change the mode
  cpp_options = list(stan_opencl = TRUE, stan_threads = TRUE), ## within chain parallel
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

# write_rds(fit, paste('./results/',chosen_exp,'/log_full.rds',sep = ''))

fit$save_object(file = paste('./results/',chosen_exp,'/log_full',additional_suffix,'.rds',sep = ''))

loo <- fit$loo(cores = 10, moment_match = TRUE)
save(loo, file = paste('./results/',chosen_exp,'/loo/loo_log_full',additional_suffix,'.rds',sep = ''))

loo$estimates

