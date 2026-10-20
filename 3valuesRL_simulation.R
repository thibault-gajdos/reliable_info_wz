## Simulation to verify the learning model

library(dplyr)

# parameter setup

delta  = 0.02
theta  = 5
lambda = 0
kappa = 0 # decay

n_trial <- 60000


# trials generation
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
  # level <- sample(c(65, 65),n_info,replace=TRUE)
  # change 1
  p <- level/100
  
  color <- ifelse(
    runif(length(p)) < p,
    stimulus,
    false.color
  )
  
  # color <- sample(c("Blue","Red"),n_info,replace=TRUE)
  
  
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

trial_df <-
  
  as.data.frame(
    
    t(
      
      replicate(
        n_trial,
        generate_trial(),
        simplify=TRUE
      )
      
    ),
    
    stringsAsFactors = FALSE
    
  )


trial_df$Trial <- 1:n_trial

trial_df <- trial_df |>
  dplyr::relocate(Trial)

trial_df$Value50 <- NA
trial_df$Value55 <- NA
trial_df$Value65 <- NA

trial_df$Choice <- NA
trial_df$Reward <- NA
trial_df$PE <- NA

trial_df$BlueEvidence <- NA
trial_df$RedEvidence <- NA
trial_df$PBlue <- NA

trial_df$Credit50 <- NA
trial_df$Credit55 <- NA
trial_df$Credit65 <- NA

trial_df$Value50[1] <- 0.1
trial_df$Value55[1] <- 0.1
trial_df$Value65[1] <- 0.1

## feedback ~ "optimal", "correct", "optimal_prob"
feedback = "optimal"



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
  
  # pBlue = pBlue_decision ##### change_evidence
  
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
  
  
  ######################## change_reward
  if(feedback == "optimal"){
    reward <- ifelse(
      choice==
        trial_df$Optimal[t], # change 5
        # trial_df$Stimulus[t],
      1,
      0
    )
  }else if(feedback == "correct"){
    reward <- ifelse(
      choice==
        trial_df$Stimulus[t],
      1,
      0
    )
  }
  
  

  trial_df$Reward[t] <- reward
  
  
  ## --------------------------
  ## Prediction Error
  ## --------------------------
  
  if(feedback == "optimal"){
    pCorrect <-
      ifelse(
        choice=="Blue",
        # trial_df$Optimal[t]=="Blue",
        pBlue,
        1-pBlue
      )
  }else if(feedback == "correct"){
    pCorrect <-
      ifelse(
        choice=="Blue",
        # trial_df$Stimulus[t]=="Blue",
        pBlue,
        1-pBlue
      )
  }


  ##### in one trial, pBlue = 0.6
  ##### I choose blue
  ##### but the feedback says the correct colour is red
  ##### reward = 0
  ##### pCorrect = 0.4
  ##### PE = -0.4


  
  PE <- reward-pCorrect
  
  # PE <- ifelse(
  #   choice==trial_df$Optimal[t],
  #   0.1,
  #   -0.1
  # )
  
  # change 4

  
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
          # trial_df$Optimal[t],
          choice,    # change 2
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
    V50 * (1-kappa)+
    delta*
    credit50*
    PE
  
  V55_new <-
    V55 * (1-kappa)+
    delta*
    credit55*
    PE
  
  V65_new <-
    V65 * (1-kappa)+
    delta*
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


trial_df$Value50[n_trial]
trial_df$Value55[n_trial]
trial_df$Value65[n_trial]

logodds(0.65)/logodds(0.55)
trial_df$Value65[n_trial]/trial_df$Value55[n_trial]




# par(mfrow = c(2,2))
# plot(trial_df$Trial, trial_df$Value50, type = 'l', ylim = c(-0.3,1))
# plot(trial_df$Trial, trial_df$Value55, type = 'l', ylim = c(-0.3,1))
# plot(trial_df$Trial, trial_df$Value65, type = 'l', ylim = c(-0.3,1))
# plot(trial_df$Trial, trial_df$Value65/trial_df$Value55, type = 'l', ylim = c(0,6))


# par(mfrow = c(2,2))
# plot(trial_df$Trial, trial_df$Value50, type = 'l')
# plot(trial_df$Trial, trial_df$Value55, type = 'l')
# plot(trial_df$Trial, trial_df$Value65, type = 'l')
# plot(trial_df$Trial, trial_df$Value65/trial_df$Value55, type = 'l')


par(mfrow = c(1, 3))

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

# plot(trial_df$Trial, trial_df$PE, type = 'l',
#      ylab = 'Prediction Error', xlab = 'Trial', ylim = c(0,1))
par(mfrow = c(1,3))

hist(trial_df$PE, breaks = 30)
plot(trial_df$Trial, trial_df$PE)


# plot(trial_df$Trial, trial_df$PBlue)

plot(trial_df$Trial[which(trial_df$Optimal=='Blue')], trial_df$PBlue[which(trial_df$Optimal=='Blue')], col = 'blue', ylim = c(0,1))
points(trial_df$Trial[which(trial_df$Optimal=='Red')], trial_df$PBlue[which(trial_df$Optimal=='Red')], col = 'red')



plot(trial_df$Trial, trial_df$Credit65, type = 'lo', col = '#009933', ylab = 'Value', xlab = 'Trial', xlim = c(0,20))
lines(trial_df$Trial, trial_df$Credit55, col = '#00CC33')
lines(trial_df$Trial, trial_df$Credit50, col = '#0099FF')


plot(trial_df$Trial, trial_df$Credit65, pch = 1, col = '#009933', ylab = 'Value', xlab = 'Trial', xlim = c(0,20))
points(trial_df$Trial, trial_df$Credit55, col = '#00CC33')
points(trial_df$Trial, trial_df$Credit50, col = '#0099FF')
lines(trial_df$Trial, trial_df$Credit65, col = '#009933')
lines(trial_df$Trial, trial_df$Credit55, col = '#00CC33')
lines(trial_df$Trial, trial_df$Credit50, col = '#0099FF')



plot(trial_df$Trial, trial_df$Credit65 * trial_df$PE, pch = 1, col = '#009933', ylab = 'Value', xlab = 'Trial', xlim = c(0,20), ylim = c(-1,1))
points(trial_df$Trial, trial_df$Credit55 * trial_df$PE, col = '#00CC33')
points(trial_df$Trial, trial_df$Credit50 * trial_df$PE, col = '#0099FF')
lines(trial_df$Trial, trial_df$Credit65 * trial_df$PE, col = '#009933')
lines(trial_df$Trial, trial_df$Credit55 * trial_df$PE, col = '#00CC33')
lines(trial_df$Trial, trial_df$Credit50 * trial_df$PE, col = '#0099FF')


plot(trial_df$Trial, trial_df$Credit55 * trial_df$PE, pch = 1, col = '#00CC33', ylab = 'Value', xlab = 'Trial', xlim = c(0,2000), ylim = c(-1,1))
lines(trial_df$Trial, trial_df$Credit55 * trial_df$PE, col = '#00CC33')

