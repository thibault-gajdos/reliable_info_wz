### Prepare the data for stan


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




data_list <- list(
  N = N,
  T_max = T_max,
  I = I,
  Tsubj = t_subjs,
  color = color,
  proba = proba,
  choice = choice)



########### Fit models
library(rstan)
library(loo)
library(readr)


## log basic model

# fit <- stan('./stan/log_basic.stan',
#             data = data_list,
#             iter = 4000,
#             warmup = 2000,
#             chains = 4,
#             cores = 4,
#             init =  "random",
#             seed = 12345,
#             control = list(adapt_delta = .8,  max_treedepth = 12)
# )

model.basic <- stan_model(file = './stan/log_basic.stan')



fit <- sampling(
  model.basic,
  data = data_list,
  iter = 2000,
  chains = 1,
  algorithm = "Fixed_param"
)



write_rds(fit, paste('./results/',chosen_exp,'/log_basic.rds',sep = ''))
loo <- loo(fit)
save(loo, file = paste('./results/',chosen_exp,'/loo/loo_log_basic.rdata',sep = ''))


log_lik <- extract(fit)$log_lik
loo_result <- loo(log_lik, r_eff = NA)
loo_result$estimates


## log seq model
fit <- stan('./stan/log_seq_basic.stan',
            data = data_list,
            iter = 4000,
            warmup = 2000,
            chains = 4,
            cores = 4,
            init =  "random",
            seed = 12345,
            control = list(adapt_delta = .95,  max_treedepth = 12)
)

write_rds(fit, paste('./results/',chosen_exp,'/log_seq.rds',sep = ''))

loo <- loo(fit) # leave one method
save(loo, file = paste('./results/',chosen_exp,'/loo/loo_log_seq.rdata',sep = ''))



## basic model 

model.basic <- stan_model(file = './stan/basic.stan')

fit <- sampling(
  model.basic,
  data = data_list,
  iter = 2000,
  chains = 1,
  algorithm = "Fixed_param"
)

write_rds(fit, paste('./results/',chosen_exp,'/basic.rds',sep = ''))
loo <- loo(fit)
save(loo, file = paste('./results/',chosen_exp,'/loo/loo_basic.rdata',sep = ''))


## basic seq model
fit <- stan('./stan/basic_seq.stan',
            data = data_list,
            iter = 4000,
            warmup = 2000,
            chains = 4,
            cores = 4,
            init =  "random",
            seed = 12345,
            control = list(adapt_delta = .95,  max_treedepth = 12)
)

write_rds(fit, paste('./results/',chosen_exp,'/basic_seq.rds',sep = ''))

loo <- loo(fit) # leave one method
save(loo, file = paste('./results/',chosen_exp,'/loo/loo_basic_seq.rdata',sep = ''))



