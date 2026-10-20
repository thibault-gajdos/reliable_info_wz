library(jsonlite)
library(dplyr)
library(ggplot2)

mitigation3_a5xi = read.csv('data/mitigation3/data_exp_273055-v7_task-a5xi.csv')
mitigation3_sxrr = read.csv('data/mitigation3/data_exp_273055-v7_task-sxrr.csv')


df_mitigation3 = rbind(cbind(na.exclude(unique(mitigation3_a5xi$Participant.Private.ID)),'a5xi'),
                cbind(na.exclude(unique(mitigation3_sxrr$Participant.Private.ID)),'sxrr'))
colnames(df_mitigation3)=c('Participant.Private.ID','Experiment.Name')
df_mitigation3 = data.frame(df_mitigation3)
df_mitigation3$Done.by = 'mitigation3'


######## Get only response and slider, remove irrelevant columns
Button0Blue_flip = function(df){
  button0blue_indx = which(df$randomiser.475f=='Button0Blue')
  button0blue_response_indx = which(df$randomiser.475f=='Button0Blue' & df$Task=='response')
  df$Response[button0blue_response_indx] = 1 - as.numeric(df$Response[button0blue_response_indx])
  df$Correct.Response[button0blue_indx] = 1 - as.numeric(df$Correct.Response[button0blue_indx])
  if ('Optimalchoice' %in% colnames(df)){
    df$Optimalchoice[button0blue_indx] = 1 - as.numeric(df$Optimalchoice[button0blue_indx])
  }
  df$randomiser.475f[button0blue_indx] = 'Button0Red'
  return(df)
}


mitigation3_a5xi$Experiment.Name = 'a5xi'
mitigation3_sxrr$Experiment.Name = 'sxrr'
mitigation3_a5xi$PosteriorBlue = mitigation3_a5xi$PosteriorBlue - mitigation3_a5xi$PosteriorRed
mitigation3_sxrr$PosteriorBlue = mitigation3_sxrr$PosteriorBlue - mitigation3_sxrr$PosteriorRed

mitigation3_clean = rbind(mitigation3_a5xi[mitigation3_a5xi$Task=='response'|mitigation3_a5xi$Task=='slider'|mitigation3_a5xi$Task=='feedback',c(13,29,39,40,42,45,46,51,56,57,59,60)],
                          mitigation3_sxrr[mitigation3_sxrr$Task=='response'|mitigation3_sxrr$Task=='slider'|mitigation3_a5xi$Task=='feedback',c(13,29,39,40,42,45,46,51,56,57,59,60)])
mitigation3_clean$Done.by = 'mitigation3'
mitigation3_clean = Button0Blue_flip(mitigation3_clean)

data_clean = rbind(data_clean,mitigation3_clean)
df_participant_experiment = rbind(df_participant_experiment, df_mitigation3)
# response_idx = which(mitigation3_clean$Task=='response')

data_to_fit = data_processed

data_to_fit = data_to_fit[data_to_fit$Done.by=='mitigation3',]

library(lme4)
model <- glmer(
  Response ~ (1 | Participant.Private.ID) +
    Net.Blue.50 +
    Net.Blue.55 +
    Net.Blue.65,
  data = data_to_fit,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)


p.50 <- plot_model(model, type = "pred", terms = c("Net.Blue.50"), axis.lim = c(0, 1))
p.55 <- plot_model(model, type = "pred", terms = c("Net.Blue.55"), axis.lim = c(0, 1))
p.65 <- plot_model(model, type = "pred", terms = c("Net.Blue.65"), axis.lim = c(0, 1))





ggplot(df_all, aes(x = x, y = prob, color = series, fill = series)) +
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.3, color = NA) +
  geom_line(size = 0.5) +
  # facet_wrap(~ group) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Net Number of Blue",
       y = "Predicted Probability of Choosing Blue",
       color = "Reliability",
       fill = "Reliability") +
  theme_bw()



##### reliability

response_idx = which(mitigation3_clean$Task=='response')
feedback_idx = response_idx+2

library(stringr)
library(dplyr)

optimal_prob = mitigation3_clean[feedback_idx,]

optimal_prob <- optimal_prob %>%
  mutate(optimal_probability = str_extract(Stimulus, "[0-9.]+(?=%)"))

optimal_prob = as.numeric(optimal_prob$optimal_probability)

mitigation3_clean$OptimalProbability = NA
mitigation3_clean[response_idx,14] = optimal_prob

data_optimal_prob = mitigation3_clean

data_optimal_prob <- data_optimal_prob %>% 
  drop_na(OptimalProbability)

data_optimal_prob <- data_optimal_prob %>% 
  mutate(OptimalAccuracy = as.numeric(Response == Optimalchoice))

ggplot(data_optimal_prob, mapping = aes(OptimalProbability, OptimalAccuracy))+
  geom_smooth(aes(colour = Stimulus))

ggplot(data_optimal_prob, mapping = aes(OptimalProbability, OptimalAccuracy))+
  geom_smooth()

model.optimalprob = glm(OptimalAccuracy~OptimalProbability, data = data_optimal_prob, family = binomial)
model.optimalprob.1 = glm(OptimalAccuracy~OptimalProbability*Stimulus, data = data_optimal_prob, family = binomial)

plot.optimalprob = plot_model(model.optimalprob, type = "pred", terms = c("OptimalProbability"), axis.lim = c(0, 1))

plot.optimalprob


plot.optimalprob = plot_model(model.optimalprob.1, type = "pred", terms = c("OptimalProbability", "Stimulus"), axis.lim = c(0, 1))




##### learning by time

time_seq_df = data_processed
time_seq_df <- time_seq_df %>%
  group_by(Participant.Private.ID) %>%
  mutate(Trial = row_number()) %>%
  ungroup() # Good practice to ungroup after calculations

time_seq_df <- time_seq_df %>% 
  mutate(OptimalAccuracy = as.numeric(Response == Optimalchoice))

ggplot(data = time_seq_df, aes(Trial, OptimalAccuracy))+
  geom_smooth(aes(color = Done.by), 
              method = "glm", 
              method.args = list(family = "binomial"),
              se = TRUE, 
              linewidth = 1.5) +
  theme_minimal()






ggplot(data = time_seq_df, aes(x = Trial, y = OptimalAccuracy)) +
  # 1. Plot individual participant lines (thin, transparent, and grouped)
  geom_smooth(aes(group = Participant.Private.ID, color = Done.by), 
            alpha = 0.1, linewidth = 0.1) +
  # 2. Plot the aggregate smooth trend per group
  geom_smooth(aes(color = Done.by), 
              method = "glm", 
              method.args = list(family = "binomial"),
              se = TRUE, 
              linewidth = 1.5) +
  labs(title = "Trial Accuracy by Group",
       x = "Trial Number", 
       y = "Accuracy (0/1)") +
  theme_minimal()+ 
  facet_wrap(~ Done.by)+
  coord_cartesian(ylim = c(0.25, 1))


###### sliding window
## length 50 per participant

window_length <- 50

accuracy_window_df <- time_seq_df %>%
  filter(Trial >= 1) %>%
  group_by(
    Done.by,
    Participant.Private.ID
  ) %>%
  group_modify(function(df, key) {
    
    max_trial <- max(df$Trial, na.rm = TRUE)
    
    window_start <- 1:(max_trial - window_length + 1)
    
    data.frame(
      Window_start = window_start,
      Window_end = window_start + window_length - 1,
      Accuracy = sapply(
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
      N_trials = sapply(
        window_start,
        function(start) {
          sum(
            df$Trial >= start &
              df$Trial <= start + window_length - 1
          )
        }
      )
    )
  }) %>%
  ungroup()


# id_row = which(time_seq_df$Participant.Private.ID==id)
# 
# 
# for(id in unique(time_seq_df$Participant.Private.ID)){
#   id_row = which(time_seq_df$Participant.Private.ID==id)
#   print(max(time_seq_df[id_row,]$Trial))
# }



ggplot(
  accuracy_window_df,
  aes(
    x = Window_end,
    y = Accuracy,
    group = Participant.Private.ID,
    colour = Done.by
  )
) +
  geom_line(alpha = 0.1) +
  geom_smooth(
    aes(
      group = Done.by,
      colour = Done.by
    ),
    method = "loess",
    se = TRUE,
    linewidth = 1.2
  ) +
  theme_classic() +
  labs(
    x = "Trial",
    y = "Accuracy",
    colour = "Experiment"
  )




###### sliding window
## length 50 per experiment

window_length <- 50

accuracy_window_exp_df <- time_seq_df %>%
  filter(Trial >= 1) %>%
  group_by(Done.by) %>%
  group_modify(function(df, key) {
    
    max_trial <- max(df$Trial, na.rm = TRUE)
    
    window_start <- 1:(max_trial - window_length + 1)
    
    data.frame(
      Window_start = window_start,
      Window_end = window_start + window_length - 1,
      Accuracy = sapply(
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
      N_trials = sapply(
        window_start,
        function(start) {
          sum(
            df$Trial >= start &
              df$Trial <= start + window_length - 1
          )
        }
      ),
      N_participants = n_distinct(df$Participant.Private.ID)
    )
  }) %>%
  ungroup()


ggplot(accuracy_window_exp_df, aes(Window_end, Accuracy))+
  geom_line(aes(colour = Done.by))+
  theme_classic() +
  labs(
    x = "Trial",
    y = "Accuracy",
    colour = "Experiment"
  )

