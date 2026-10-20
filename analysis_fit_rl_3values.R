library(readr)
library(ggplot2)
library(posterior)
library(bayesplot)


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

# fit$variable_skeleton()


##### correlation plot

posterior <- fit$draws()
# target_pars = c('mu_delta', 'mu_theta','V50_init', 'V55_init', 'V65_init')
target_pars = c('mu_delta', 'mu_lambda','V50_init', 'V55_init', 'V65_init')



bayesplot_theme_update(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
mcmc_pairs(posterior, 
                pars = target_pars, 
                off_diag_fun = "scatter",
                off_diag_args = list(alpha = 0.2, size = 1))


# Reset after if you don't want it applying globally
bayesplot_theme_update(axis.text.x = element_text())  # reset




fit$summary()

group_draws = fit$draws(variables = c('mu_delta', 'mu_theta','V50_init', 'V55_init', 'V65_init'))
group_draws = as_draws_df(group_draws)

inidiv_draws = fit$draws(variables = c('delta','theta'))
inidiv_draws = as_draws_df(inidiv_draws)

group_summary <- summarise_draws(group_draws) 


mu_delta_mean = as.numeric(group_summary[which(group_summary$variable=='mu_delta'), 2])
mu_theta_mean = as.numeric(group_summary[which(group_summary$variable=='mu_theta'), 2])
mu_V50_init_mean = as.numeric(group_summary[which(group_summary$variable=='V50_init'), 2])
mu_V55_init_mean = as.numeric(group_summary[which(group_summary$variable=='V55_init'), 2])
mu_V65_init_mean = as.numeric(group_summary[which(group_summary$variable=='V65_init'), 2])


## simulation preparation
# trials generation
library(dplyr)

logodds <- function(a){
  return(log(a/(1-a)))
}


generate_trial <- function(
    n_info = 6,
    levels = c(50,55,65),
    probs = c(0.5,0.55,0.65)
){
  stimulus <- sample(c('Red','Blue'),1)
  false.color <- setdiff(c('Red','Blue'), stimulus)
  level <- sample(levels,n_info,replace=TRUE)
  p <- level/100
  
  color <- ifelse(
    runif(length(p)) < p,
    stimulus,
    false.color
  )
  
  blue_evidence <- sum(logodds(p[color=="Blue"]))
  red_evidence  <- sum(logodds(p[color=="Red"]))
  
  optimal <-
    ifelse(
      blue_evidence > red_evidence,
      "Blue",
      ifelse(
        red_evidence > blue_evidence,
        "Red",
        sample(c("Blue","Red"),1)
      )
    )
  
  out <- c(
    Stimulus=stimulus,
    setNames(color,paste0("Color",1:n_info)),
    setNames(level,paste0("Prob",1:n_info)),
    Optimal=optimal
  )
  return(out)
}

simulate_mean <- function(delta, theta, V50_init, V55_init, V65_init, exp_){
  if(exp_ == 'Alex'){
    exp_name = 'Mitigation 1'
    feedback = 'stimulus'
  }else if(exp_ == 'Exp6'){
    exp_name = 'Control'
    feedback = 'stimulus'
  }else if(exp_ == 'Divyaj'){
    exp_name = 'Mitigation 2'
    feedback = 'optimal'
  }else if(exp_ == 'mitigation3'){
    exp_name = 'Mitigation 3'
    feedback = 'optimal'
  }else{
    stop("invalid experiment name")
  }
  n_trial = 300
  
  trial_df <-
    as.data.frame(
      t(replicate(
          n_trial,
          generate_trial(),
          simplify=TRUE
        )
      ),
      stringsAsFactors = FALSE
    )
  
  trial_df$Trial <- 1:n_trial
  
  trial_df$Value50[1] <- V50_init
  trial_df$Value55[1] <- V55_init
  trial_df$Value65[1] <- V65_init
  
  alpha = delta
  lambda = 0
  
  for(t in 1:nrow(trial_df)){
    
    ## --------------------------
    ## Current latent values
    ## --------------------------
    
    V50 <- trial_df$Value50[t]
    V55 <- trial_df$Value55[t]
    V65 <- trial_df$Value65[t]
    
    blueEvidence <- 0
    redEvidence  <- 0
    
    
    ## --------------------------
    ## Evidence accumulation
    ## --------------------------
    
    for(i in 1:6){
      
      level <- trial_df[t,paste0("Prob",i)]
      color <- trial_df[t,paste0("Color",i)]
      
      value <- switch(as.character(level),
                      "50" = V50,
                      "55" = V55,
                      "65" = V65)
      
      discount <- exp(lambda*(i-6))
      
      if(color=="Blue"){
        blueEvidence <- blueEvidence +
          discount*value
      }else{
        redEvidence <- redEvidence +
          discount*value
      }
    }
    trial_df$BlueEvidence[t] <- blueEvidence
    trial_df$RedEvidence[t]  <- redEvidence
    
    ## --------------------------
    ## Choice
    ## --------------------------
    
    # pBlue <-
    #   plogis(blueEvidence-redEvidence)
    
    pBlue <-
      plogis(blueEvidence - redEvidence) # change 7
    
    trial_df$PBlue[t] <- pBlue
    
    pBlue_decision = plogis((blueEvidence - redEvidence)*theta)
    
    
    choice <- sample(c("Blue", "Red"), 1, prob = c(pBlue_decision, 1 - pBlue_decision))
    # plogis(theta*(blueEvidence-redEvidence))
    
    # change 3
    
    # choice <- ifelse(
    #   pBlue>=0.5,
    #   "Blue",
    #   "Red"
    # )
    
    trial_df$Choice[t] <- choice
    
    
    ## --------------------------
    ## Reward
    ## --------------------------
    if(feedback=='stimulus'){
      reward <- ifelse(
        choice==
        trial_df$Stimulus[t],
        1,
        0
      )
    }else{
      reward <- ifelse(
        choice==
          trial_df$Optimal[t],
        1,
        0
      )
    }
    
    trial_df$Reward[t] <- reward
    
    ## --------------------------
    ## Prediction Error
    ## --------------------------
    if(feedback=='stimulus'){
      pCorrect <-
        ifelse(
          trial_df$Stimulus[t]=="Blue",
          pBlue,
          1-pBlue
        )
    }else{
      pCorrect <-
        ifelse(
          trial_df$Optimal[t]=="Blue",
          pBlue,
          1-pBlue
        )
    }
    
    PE <- reward-pCorrect
    trial_df$PE[t] <- PE
    
    
    ## --------------------------
    ## Credit Assignment
    ## --------------------------
    
    credit50 <- 0
    credit55 <- 0
    credit65 <- 0
    
    
    for(i in 1:6){
      
      level <- trial_df[t,paste0("Prob",i)]
      color <- trial_df[t,paste0("Color",i)]
      
      color==choice
      contribution <-
        ifelse(
          color==
            choice,
          1,
          -1
        )
      
      if(level==50){
        credit50 <- credit50+contribution
      }else if(level==55){
        credit55 <- credit55+contribution
      }else{
        credit65 <- credit65+contribution
      }
    }
    # change 6
    # credit50 = credit50 * abs(trial_df$Value50[t])
    # credit55 = credit55 * abs(trial_df$Value55[t])
    # credit65 = credit65 * abs(trial_df$Value65[t])
    
    totalCredit <-
      abs(credit50)+
      abs(credit55)+
      abs(credit65)
    
    
    if(totalCredit>0){
      credit50 <- credit50/totalCredit
      credit55 <- credit55/totalCredit
      credit65 <- credit65/totalCredit
    }else{
      credit50 <- 0
      credit55 <- 0
      credit65 <- 0
    }
    
    trial_df$Credit50[t] <- credit50
    trial_df$Credit55[t] <- credit55
    trial_df$Credit65[t] <- credit65
    
    ## --------------------------
    ## Value Update
    ## --------------------------
    
    V50_new <-
      V50+
      alpha*
      credit50*
      PE
    
    V55_new <-
      V55+
      alpha*
      credit55*
      PE
    
    V65_new <-
      V65+
      alpha*
      credit65*
      PE
    
    
    ## --------------------------
    ## Next trial
    ## --------------------------
    
    if(t<nrow(trial_df)){
      
      trial_df$Value50[t+1] <- V50_new
      trial_df$Value55[t+1] <- V55_new
      trial_df$Value65[t+1] <- V65_new
      
    }
    
  }
  
  
  #### plotting
  par(mfrow = c(1, 3), oma = c(0, 0, 3, 0))
  y_range <- range(trial_df$Value50, trial_df$Value55, trial_df$Value65, c(logodds(0.65),logodds(0.55),logodds(0.5)))
  
  # First plot: all three lines together
  plot(trial_df$Trial, trial_df$Value65, type = 'l', col = '#009933',
       ylim = y_range, ylab = 'Value', xlab = 'Trial')
  lines(trial_df$Trial, trial_df$Value55, col = '#00CC33')
  lines(trial_df$Trial, trial_df$Value50, col = '#0099FF')
  
  abline(h=logodds(0.65), col = '#009933', lty = 3)
  abline(h=logodds(0.55), col = '#00CC33', lty = 3)
  abline(h=logodds(0.5), col = '#0099FF', lty = 3)
  
  
  legend('topleft', legend = c('Value50', 'Value55', 'Value65'),
         col = c('#0099FF', '#00CC33', '#009933'), lty = 1)
  
  # Second plot: ratio
  
  y_range <- range(trial_df$Value65/trial_df$Value55, c(logodds(0.65)/logodds(0.55)))
  
  plot(trial_df$Trial, trial_df$Value65 / trial_df$Value55, ylim = y_range, type = 'l',
       ylab = 'Value65 / Value55', xlab = 'Trial')
  abline(h = logodds(0.65)/logodds(0.55), lty = 3)
  
  
  
  trial_df$OptimalAccuracy = ifelse(
    trial_df$Optimal==trial_df$Choice, 1, 0 
  ) 
  
  trial_df$ChoiceAccuracy = ifelse(
    trial_df$Stimulus==trial_df$Choice, 1, 0 
  ) 
  
  
  window_length <- 30
  
  accuracy_window_df <- trial_df %>%
    filter(Trial >= 1) %>%
    group_modify(function(df, key) {
      
      max_trial <- max(df$Trial, na.rm = TRUE)
      
      window_start <- 1:(max_trial - window_length + 1)
      
      data.frame(
        Window_start = window_start,
        Window_end = window_start + window_length - 1,
        OptimalAccuracy = sapply(
          window_start,
          function(start) {
            mean(
              df$OptimalAccuracy[
                df$Trial >= start &
                  df$Trial <= start + window_length - 1
              ],
              na.rm = TRUE
            )
          }
        ),
        ChoiceAccuracy = sapply(
          window_start,
          function(start) {
            mean(
              df$ChoiceAccuracy[
                df$Trial >= start &
                  df$Trial <= start + window_length - 1
              ],
              na.rm = TRUE
            )
          }
        )
      )
    })
  
  
  plot(accuracy_window_df$Window_end, accuracy_window_df$ChoiceAccuracy, type = 'l',
       ylab = 'Choice Accuracy', xlab = 'Window End', ylim = c(0,1), col = 'red')
  
  
  lines(accuracy_window_df$Window_end, accuracy_window_df$OptimalAccuracy, type = 'l', col = 'blue')
  legend('bottomright', legend = c('Optimality', 'Choice'),
         col = c('blue', 'red'), lty = 1)
  
  
  mtext(exp_name, outer = TRUE, cex = 1.5, font = 2)
}

simulate_mean(mu_delta_mean, mu_theta_mean, mu_V50_init_mean, mu_V55_init_mean, mu_V65_init_mean, chosen_exp)



