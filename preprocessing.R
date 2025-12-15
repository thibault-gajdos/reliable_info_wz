## Replicate analysis on Alex & Divyaj data
library(jsonlite)
library(dplyr)


alex_czkz = read.csv('data/Alex/data_exp_178975-v10_task-czkz.csv')
alex_r9tq = read.csv('data/Alex/data_exp_178975-v10_task-r9tq.csv')

divyaj_czkz = read.csv('data/Divyaj/data_exp_179659-v6_task-czkz.csv')
divyaj_r9tq = read.csv('data/Divyaj/data_exp_179659-v6_task-r9tq.csv')

# exp1_dr3e = read.csv('data/Exp1/data_exp_52545-v4_task-dr3e.csv', sep = '\t')
# exp1_t5fq = read.csv('data/Exp1/data_exp_52545-v4_task-t5fq.csv', sep = '\t')

exp6_dr3e_v10 = read.csv('data/Exp6/data_exp_52555-v10_task-dr3e.csv')
exp6_t5fq_v10 = read.csv('data/Exp6/data_exp_52555-v10_task-t5fq.csv')

exp6_dr3e_v11 = read.csv('data/Exp6/data_exp_52555-v11_task-dr3e.csv')
exp6_t5fq_v11 = read.csv('data/Exp6/data_exp_52555-v11_task-t5fq.csv')

exp6_dr3e = rbind(exp6_dr3e_v11, exp6_dr3e_v10)
exp6_t5fq = rbind(exp6_t5fq_v10, exp6_t5fq_v11)


# sum(exp6_dr3e$Task=='response') + sum(exp6_t5fq$Task=='response')

# length(na.exclude(unique(alex_czkz$Participant.Private.ID))) # 17
# length(na.exclude(unique(alex_r9tq$Participant.Private.ID))) # 13
# length(na.exclude(unique(divyaj_czkz$Participant.Private.ID))) # 14
# length(na.exclude(unique(divyaj_r9tq$Participant.Private.ID))) # 16
# length(na.exclude(unique(exp6_dr3e$Participant.Private.ID)))
# length(na.exclude(unique(exp6_t5fq$Participant.Private.ID)))


df_Alex = rbind(cbind(na.exclude(unique(alex_czkz$Participant.Private.ID)),'czkz'),
cbind(na.exclude(unique(alex_r9tq$Participant.Private.ID)),'r9tq'))
colnames(df_Alex)=c('Participant.Private.ID','Experiment.Name')
df_Alex = data.frame(df_Alex)
df_Alex$Done.by = 'Alex'

df_Djvyaj = rbind(cbind(na.exclude(unique(divyaj_czkz$Participant.Private.ID)),'czkz'),
                cbind(na.exclude(unique(divyaj_r9tq$Participant.Private.ID)),'r9tq'))
colnames(df_Djvyaj)=c('Participant.Private.ID','Experiment.Name')
df_Djvyaj = data.frame(df_Djvyaj)
df_Djvyaj$Done.by = 'Divyaj'


# df_exp1 = rbind(cbind(na.exclude(unique(exp1_dr3e$Participant.Private.ID)),'dr3e'),
#                   cbind(na.exclude(unique(exp1_t5fq$Participant.Private.ID)),'t5fq'))
# colnames(df_exp1)=c('Participant.Private.ID','Experiment.Name')
# df_exp1 = data.frame(df_exp1)
# df_exp1$Done.by = 'Exp1'


df_exp6 = rbind(cbind(na.exclude(unique(exp6_dr3e$Participant.Private.ID)),'dr3e'),
                cbind(na.exclude(unique(exp6_t5fq$Participant.Private.ID)),'t5fq'))


colnames(df_exp6)=c('Participant.Private.ID','Experiment.Name')
df_exp6 = data.frame(df_exp6)
df_exp6$Done.by = 'Exp6'



df_participant_experiment = rbind(df_Alex, 
                                  df_Djvyaj,
                                  # df_exp1,
                                  df_exp6)


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


alex_czkz$Experiment.Name = 'czkz'
alex_r9tq$Experiment.Name = 'r9tq'
alex_czkz$PosteriorBlue = alex_czkz$PosteriorBlue - alex_czkz$PosteriorRed
alex_r9tq$PosteriorBlue = alex_r9tq$PosteriorBlue - alex_r9tq$PosteriorRed

alex_clean = rbind(alex_czkz[alex_czkz$Task=='response'|alex_czkz$Task=='slider',c(13,29,40,41,43,46,47,52,58,59,61,62)],
                   alex_r9tq[alex_r9tq$Task=='response'|alex_r9tq$Task=='slider',c(13,29,40,41,43,46,47,52,58,59,61,62)])
alex_clean$Done.by = 'Alex'
alex_clean = Button0Blue_flip(alex_clean)


divyaj_czkz$Experiment.Name = 'czkz'
divyaj_r9tq$Experiment.Name = 'r9tq'
divyaj_czkz$PosteriorBlue = divyaj_czkz$PosteriorBlue - divyaj_czkz$PosteriorRed
divyaj_r9tq$PosteriorBlue = divyaj_r9tq$PosteriorBlue - divyaj_r9tq$PosteriorRed

divyaj_clean = rbind(divyaj_czkz[divyaj_czkz$Task=='response'|divyaj_czkz$Task=='slider',c(13,29,40,41,43,46,47,52,57,58,60,61)],
                     divyaj_r9tq[divyaj_r9tq$Task=='response'|divyaj_r9tq$Task=='slider',c(13,29,40,41,43,46,47,52,57,58,60,61)])
divyaj_clean$Done.by = 'Divyaj'
divyaj_clean = Button0Blue_flip(divyaj_clean)





# exp1_dr3e$Experiment.Name = 'dr3e'
# exp1_t5fq$Experiment.Name = 't5fq'
# 
# exp1_clean = rbind(exp1_dr3e[exp1_dr3e$Task=='response'|exp1_dr3e$Task=='slider',c(13,27,36,37,39,42,43,48,53)],
#                    exp1_t5fq[exp1_t5fq$Task=='response'|exp1_t5fq$Task=='slider',c(13,27,36,37,39,42,43,48,53)])
# 
# exp1_clean = Button0Blue_flip(exp1_clean)

exp6_dr3e$Experiment.Name = 'dr3e'
exp6_t5fq$Experiment.Name = 't5fq'

# exp6_clean = rbind(exp6_dr3e[exp6_dr3e$Task=='response'|exp6_dr3e$Task=='slider',c(13,27,37,38,40,43,44,49,54)],
#                    exp6_t5fq[exp6_t5fq$Task=='response'|exp6_t5fq$Task=='slider',c(13,27,37,38,40,43,44,49,54)])

exp6_clean = rbind(exp6_t5fq[exp6_t5fq$Task=='response'|exp6_t5fq$Task=='slider',c(13,27,37,38,40,43,44,49,54)],
                   exp6_dr3e[exp6_dr3e$Task=='response'|exp6_dr3e$Task=='slider',c(13,27,37,38,40,43,44,49,54)])


exp6_clean = Button0Blue_flip(exp6_clean)

# button0blue_indx = which(exp1_clean$randomiser.475f=='Button0Blue')
# exp1_clean$Response[button0blue_indx] = 1 - as.numeric(exp1_clean$Response[button0blue_indx])
# exp1_clean$Correct.Response[button0blue_indx] = 1 - as.numeric(exp1_clean$Correct.Response[button0blue_indx])
# # exp1_clean$Optimalchoice[button0blue_indx] = 1 - as.numeric(exp1_clean$Optimalchoice[button0blue_indx])
# exp1_clean$randomiser.475f[button0blue_indx] = 'Button0Red'


### Get BDT for exp1 or exp6

# results = lapply(exp1_clean$Stimulus.Info, function(txt) {
#   d = fromJSON(txt)
#   blue_rel = ifelse(d$color == "blue", d$reliability, 1 - d$reliability)
#   list(
#     blue_rel = list(blue_rel)
#   )
# })


response_idx = which(exp6_clean$Task=='response')


results = lapply(exp6_clean$Stimulus.Info[response_idx], function(txt) {
  d = fromJSON(txt)
  blue_rel = ifelse(d$color == "blue", d$reliability, 1 - d$reliability)
  list(
    blue_rel = list(blue_rel)
  )
})

res_df = bind_rows(results)
# unlist(results[4,1])


results = lapply(res_df$blue_rel, function(blue_rel) {
  blue_rel = unlist(blue_rel)
  log_odds = round(sum(log(blue_rel/(1-blue_rel))), digits = 6)
  list(
    Optimalchoice = case_when(
      log_odds > 0  ~ 1,
      log_odds < 0  ~ 0,
      log_odds == 0 ~ sample(c(0, 1), 1)   # random choice
    ),
    PosteriorBlue = log_odds
  )
})

results = bind_rows(results)
res_df = cbind(res_df, results)

###

# Experiment.Name = exp1_clean$Experiment.Name
# exp1_clean = cbind(exp1_clean[,1:8],res_df[,c(2,3)])
# exp1_clean$accuracy_bayes = ifelse(exp1_clean$Correct.Response == exp1_clean$Optimalchoice, 1, 0)
# exp1_clean$Experiment.Name = Experiment.Name
# 
# exp1_clean$Done.by = 'Exp1'
# 
# data_clean = rbind(alex_clean, divyaj_clean)


Experiment.Name = exp6_clean$Experiment.Name
exp6_clean$Optimalchoice = NA
exp6_clean$PosteriorBlue = NA
exp6_clean$Optimalchoice[response_idx] = res_df$Optimalchoice
exp6_clean$PosteriorBlue[response_idx] = res_df$PosteriorBlue
exp6_clean$accuracy_bayes = ifelse(exp6_clean$Correct.Response == exp6_clean$Optimalchoice, 1, 0)
exp6_clean = exp6_clean[,-9]
exp6_clean$Experiment.Name = Experiment.Name

exp6_clean$Done.by = 'Exp6'

data_clean = rbind(alex_clean, divyaj_clean, exp6_clean)





######## Get json information

response_idx = which(data_clean$Task=='response')
slider_idx = which(data_clean$Task=='slider')

# data_clean$blue_rel = NA 
# data_clean$blue_rel[response_idx] = sapply(data_clean$Stimulus.Info[response_idx], function(txt) {
#   data = fromJSON(txt)
#   vals = ifelse(data$color == "blue", data$reliability, 1 - data$reliability)
#   list(vals)
# })



results = lapply(data_clean$Stimulus.Info[response_idx], function(txt) {
  d = fromJSON(txt)
  blue_rel = ifelse(d$color == "blue", d$reliability, 1 - d$reliability)
  list(
    blue_rel = list(blue_rel),
    blue_50 = sum(d$color == "blue" & d$reliability == 0.5),
    blue_55 = sum(d$color == "blue" & d$reliability == 0.55),
    blue_65 = sum(d$color == "blue" & d$reliability == 0.65),
    red_50 = sum(d$color == "red" & d$reliability == 0.5),
    red_55 = sum(d$color == "red" & d$reliability == 0.55),
    red_65 = sum(d$color == "red" & d$reliability == 0.65)
  )
})

res_df = bind_rows(results)



# results = lapply(exp1_clean$Stimulus.Info, function(txt) {
#   d = fromJSON(txt)
#   blue_rel = ifelse(d$color == "blue", d$reliability, 1 - d$reliability)
#   list(
#     blue_rel = list(blue_rel),
#     blue_50 = sum(d$color == "blue" & d$reliability == 0.5),
#     blue_55 = sum(d$color == "blue" & d$reliability == 0.55),
#     blue_65 = sum(d$color == "blue" & d$reliability == 0.65),
#     red_50 = sum(d$color == "red" & d$reliability == 0.5),
#     red_55 = sum(d$color == "red" & d$reliability == 0.55),
#     red_65 = sum(d$color == "red" & d$reliability == 0.65)
#   )
# })
# 
# res_df_exp1 = bind_rows(results)

####### Get slider information and put together

results = sapply(data_clean$Stimulus[slider_idx], function(slider_info) {
  reliability = as.integer(substr(slider_info, 17,18))
})

names(results) <- NULL

data_processed = data_clean[which(data_clean$Task=='response'),]
data_processed$slider_reliability = results


results = sapply(data_clean$Response[slider_idx], function(slider_info) {
  response = slider_info
})

data_processed$slider_response = results
data_processed = cbind(data_processed,res_df)

# exp1_clean$slider_reliability = NA
# exp1_clean$slider_response = NA
# exp1_clean = cbind(exp1_clean, res_df_exp1)
# 
# data_processed = rbind(data_processed, exp1_clean)



# data_processed = data_clean[which(data_clean$Task=='response'),]
# data_processed = cbind(data_processed, res_df)
# c(1,1,1,1,1,1)- unlist(data_clean$blue_rel[1])


######## Make sure we process it with Button 0 red (revert the Button0Blue)
# 
# button0blue_indx = which(data_processed$randomiser.475f=='Button0Blue')
# data_processed$Response[button0blue_indx] = 1 - as.numeric(data_processed$Response[button0blue_indx])
# data_processed$Correct.Response[button0blue_indx] = 1 - as.numeric(data_processed$Correct.Response[button0blue_indx])
# data_processed$Optimalchoice[button0blue_indx] = 1 - as.numeric(data_processed$Optimalchoice[button0blue_indx])
# data_processed$randomiser.475f[button0blue_indx] = 'Button0Red'

####### Optimal Observer and Log-odds



# results = lapply(data_processed$blue_rel, function(blue_rel) {
#   blue_rel = unlist(blue_rel)
#   log_odds = round(sum(log(blue_rel/(1-blue_rel))), digits = 4)
#   list(
#     log_odds = log_odds,
#     BDT_choice = ifelse(log_odds>0, 1, 0)
#   )
# })
# 
# results = bind_rows(results)
# data_processed = cbind(data_processed,results)

# b = c(0.50, 0.65, 0.45, 0.50, 0.5, 0.35)
# round(sum(log(b/(1-b))), digits = 4)


######## participants accuracy vs. BDT accuracy

performance = data.frame(df_participant_experiment)
performance$Accuracy = NA
performance$BDT_accuracy = NA

for (i in 1:nrow(performance)) {
  ID = performance$Participant.Private.ID[i]
  selected_participant = data_processed[which(data_processed$Participant.Private.ID==ID),]
  performance$Accuracy[i] = sum(selected_participant$Response == selected_participant$Correct.Response)/nrow(selected_participant)
  performance$BDT_accuracy[i] = sum(selected_participant$Optimalchoice == selected_participant$Correct.Response)/nrow(selected_participant)
  performance$Optimal_Choice[i] = sum(selected_participant$Optimalchoice == selected_participant$Response)/nrow(selected_participant)
  
}


######## clean up workspace
rm(list = setdiff(ls(), c('data_processed','df_participant_experiment','performance')))


####### Visualize
par(mfrow = c(1, 3), oma = c(0, 0, 3, 0))

plot_performance = function(df_performance, exp_done.by, second_plot){
  plot_df = df_performance[which(df_performance$Done.by==exp_done.by),]
  plot_df = plot_df[order(plot_df$Accuracy),]
  
  plot(plot_df$Accuracy,
       type = "p",
       pch = 19,
       xlab = "Index (sorted)",
       ylab = "Performance",
       ylim = c(0.4,1),
       main = exp_done.by)
  if (second_plot == 'BDT'){
    points(plot_df$BDT_accuracy,
       type = "p",
       pch = 0)
    abline(h = mean(plot_df$BDT_accuracy), col = 'gray')
  }else{
    plot_df = plot_df[order(plot_df$Optimal_Choice),]
    points(plot_df$Optimal_Choice,
           type = "p",
           pch = 1)
    abline(h = mean(plot_df$Optimal_Choice), col = 'gray')
  }

  abline(h = mean(plot_df$Accuracy))
}

# plot_performance(performance, 'Exp1', 'BDT')
plot_performance(performance, 'Exp6', 'BDT')
plot_performance(performance, 'Alex', 'BDT')
plot_performance(performance, 'Divyaj', 'BDT')


mtext("Participants vs. BDT", outer = TRUE, cex = 1.5, font = 2)

### Optimal Choices vs. Correct Choices

par(mfrow = c(1, 3), oma = c(0, 0, 3, 0))

# plot_performance(performance, 'Exp1', 'Optimal')
plot_performance(performance, 'Exp6', 'Optimal')
plot_performance(performance, 'Alex', 'Optimal')
plot_performance(performance, 'Divyaj', 'Optimal')

mtext("Correct Choices vs. Optimal Choices", outer = TRUE, cex = 1.5, font = 2)

#### stats of performance


## confirm normality

shapiro.test(performance$Accuracy[performance$Done.by=='Exp6'])
shapiro.test(performance$Accuracy[performance$Done.by=='Alex'])
shapiro.test(performance$Accuracy[performance$Done.by=='Divyaj'])

shapiro.test(performance$BDT_accuracy[performance$Done.by=='Exp6'])
shapiro.test(performance$BDT_accuracy[performance$Done.by=='Alex'])
shapiro.test(performance$BDT_accuracy[performance$Done.by=='Divyaj'])

## accuracy and BDT accuracy are normally distributed
performance$Done.by = as.factor(performance$Done.by)

fit = aov(Accuracy ~ Done.by, data = performance)
summary(fit)
TukeyHSD(fit)

fit = aov(BDT_accuracy ~ Done.by, data = performance)
summary(fit)
TukeyHSD(fit)


kruskal.test(Optimal_Choice ~ Done.by, data = performance)
library(FSA)
dunnTest(Optimal_Choice ~ Done.by, data = performance, method = "bonferroni")


shapiro.test(performance$Optimal_Choice[performance$Done.by=='Exp6'])
shapiro.test(performance$Optimal_Choice[performance$Done.by=='Alex'])
shapiro.test(performance$Optimal_Choice[performance$Done.by=='Divyaj'])

# ggplot(performance, aes(x = Optimal_Choice, fill = Done.by)) +
#   geom_histogram(alpha = 0.5, position = "identity", bins = 8) +
#   labs(title = "Histogram of Performance",
#        x = "proportions of optimal choice",
#        y = "Count") +
#   theme_minimal()


######## clean up workspace
rm(list = setdiff(ls(), c('data_processed','df_participant_experiment','performance')))




####### Likelihood of choosing the blue option vs. the net number of samples for blue or red

likelihood_blue = data.frame(df_participant_experiment)

likelihood_blue$Net_blue = -6
temp = likelihood_blue
for (i in -5:6) {
  temp$Net_blue = i
  likelihood_blue = rbind(likelihood_blue, temp)
}

len_ = nrow(likelihood_blue)
likelihood_blue = rbind(likelihood_blue, likelihood_blue, likelihood_blue)
likelihood_blue$Reliability = c(rep('50',len_), rep('55',len_), rep('65',len_))

likelihood_blue$Count = 0
likelihood_blue$Blue_choice = 0
likelihood_blue$Blue_BDT_choice = 0

likelihood_final =  likelihood_blue %>%
  distinct(Net_blue, Reliability, Done.by)

likelihood_final$Count = 0
likelihood_final$Blue_choice = 0
likelihood_final$Blue_BDT_choice = 0

data_processed$Net.Blue.50 = data_processed$blue_50 - data_processed$red_50
data_processed$Net.Blue.55 = data_processed$blue_55 - data_processed$red_55
data_processed$Net.Blue.65 = data_processed$blue_65 - data_processed$red_65

for(i in 1:nrow(data_processed)){
# for(i in 1){
  ID = data_processed$Participant.Private.ID[i]
  # Exp_name = data_processed$Experiment.Name[i]
  Done_by = data_processed$Done.by[i]
  
  choose_blue = as.numeric(data_processed$Response[i])
  choose_BDT_blue = as.numeric(data_processed$Optimalchoice[i])
  net_50 = data_processed$blue_50[i] - data_processed$red_50[i]
  net_55 = data_processed$blue_55[i] - data_processed$red_55[i]
  net_65 = data_processed$blue_65[i] - data_processed$red_65[i]
  
  row_50 = which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='50' & likelihood_blue$Net_blue==net_50)
  row_55 = which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='55' & likelihood_blue$Net_blue==net_55)
  row_65 = which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='65' & likelihood_blue$Net_blue==net_65)
  likelihood_blue$Count[row_50] = likelihood_blue$Count[row_50] + 1
  likelihood_blue$Count[row_55] = likelihood_blue$Count[row_55] + 1
  likelihood_blue$Count[row_65] = likelihood_blue$Count[row_65] + 1
  likelihood_blue$Blue_choice[row_50] = likelihood_blue$Blue_choice[row_50] + choose_blue
  likelihood_blue$Blue_choice[row_55] = likelihood_blue$Blue_choice[row_55] + choose_blue
  likelihood_blue$Blue_choice[row_65] = likelihood_blue$Blue_choice[row_65] + choose_blue
  likelihood_blue$Blue_BDT_choice[row_50] = likelihood_blue$Blue_BDT_choice[row_50] + choose_BDT_blue
  likelihood_blue$Blue_BDT_choice[row_55] = likelihood_blue$Blue_BDT_choice[row_55] + choose_BDT_blue
  likelihood_blue$Blue_BDT_choice[row_65] = likelihood_blue$Blue_BDT_choice[row_65] + choose_BDT_blue
  
  row_50 = which(likelihood_final$Reliability=='50' & likelihood_final$Net_blue==net_50 & likelihood_final$Done.by==Done_by)
  row_55 = which(likelihood_final$Reliability=='55' & likelihood_final$Net_blue==net_55 & likelihood_final$Done.by==Done_by)
  row_65 = which(likelihood_final$Reliability=='65' & likelihood_final$Net_blue==net_65 & likelihood_final$Done.by==Done_by)
  likelihood_final$Count[row_50] = likelihood_final$Count[row_50] + 1
  likelihood_final$Count[row_55] = likelihood_final$Count[row_55] + 1
  likelihood_final$Count[row_65] = likelihood_final$Count[row_65] + 1
  likelihood_final$Blue_choice[row_50] = likelihood_final$Blue_choice[row_50] + choose_blue
  likelihood_final$Blue_choice[row_55] = likelihood_final$Blue_choice[row_55] + choose_blue
  likelihood_final$Blue_choice[row_65] = likelihood_final$Blue_choice[row_65] + choose_blue
  likelihood_final$Blue_BDT_choice[row_50] = likelihood_final$Blue_BDT_choice[row_50] + choose_BDT_blue
  likelihood_final$Blue_BDT_choice[row_55] = likelihood_final$Blue_BDT_choice[row_55] + choose_BDT_blue
  likelihood_final$Blue_BDT_choice[row_65] = likelihood_final$Blue_BDT_choice[row_65] + choose_BDT_blue
  
  
  # 
  # likelihood_blue$Count[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='50' & likelihood_blue$Net_blue==net_50)] = 
  #   likelihood_blue$Count[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='50' & likelihood_blue$Net_blue==net_50)] + 1
  # likelihood_blue$Blue_choice[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='50' & likelihood_blue$Net_blue==net_50)] = 
  #   likelihood_blue$Blue_choice[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='50' & likelihood_blue$Net_blue==net_50)] + choose_blue
  # likelihood_blue$Count[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='55' & likelihood_blue$Net_blue==net_55)] = 
  #   likelihood_blue$Count[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='55' & likelihood_blue$Net_blue==net_55)] + 1
  # likelihood_blue$Blue_choice[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='55' & likelihood_blue$Net_blue==net_55)] = 
  #   likelihood_blue$Blue_choice[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='55' & likelihood_blue$Net_blue==net_55)] + choose_blue
  # likelihood_blue$Count[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='65' & likelihood_blue$Net_blue==net_65)] = 
  #   likelihood_blue$Count[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='65' & likelihood_blue$Net_blue==net_65)] + 1
  # likelihood_blue$Blue_choice[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='65' & likelihood_blue$Net_blue==net_65)] = 
  #   likelihood_blue$Blue_choice[which(likelihood_blue$Participant.Private.ID==ID & likelihood_blue$Reliability=='65' & likelihood_blue$Net_blue==net_65)] + choose_blue
}

likelihood_blue$P_blue = likelihood_blue$Blue_choice/likelihood_blue$Count
likelihood_blue$P_BDT_blue = likelihood_blue$Blue_BDT_choice/likelihood_blue$Count

likelihood_final$P_blue = likelihood_final$Blue_choice/likelihood_final$Count
likelihood_final$P_BDT_blue = likelihood_final$Blue_BDT_choice/likelihood_final$Count


######## clean up workspace
rm(list = setdiff(ls(), c('data_processed','df_participant_experiment','performance','likelihood_blue','likelihood_final')))

######## check likelihood by participants
library(ggplot2)

data_to_fit = likelihood_blue[which(likelihood_blue$Done.by=='Alex'),]

ggplot(na.omit(data_to_fit), aes(Net_blue, P_BDT_blue, colour = Reliability))+
  geom_smooth()

data_to_fit = likelihood_blue[which(likelihood_blue$Done.by=='Divyaj'),]

ggplot(na.omit(data_to_fit), aes(Net_blue, P_BDT_blue, colour = Reliability))+
  geom_smooth()

data_to_fit = likelihood_blue[which(likelihood_blue$Done.by=='Exp6'),]

ggplot(na.omit(data_to_fit), aes(Net_blue, P_BDT_blue, colour = Reliability))+
  geom_smooth()




###### likelihood looking at
# likelihood_final_Alex = likelihood_final[which(likelihood_final$Done.by=='Alex'),]
# likelihood_final_Divyaj = likelihood_final[which(likelihood_final$Done.by=='Divyaj'),]
# likelihood_final_Exp1 = likelihood_final[which(likelihood_final$Done.by=='Exp1'),]

####### Visualize naive probability vs. Net number of square favoring blue
data_to_fit = likelihood_final[which(likelihood_final$Done.by=='Alex'),]


ggplot(na.omit(data_to_fit), aes(Net_blue, P_blue, colour = Reliability)) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-3, 3), ylim = c(0, 1))

ggplot(na.omit(data_to_fit), aes(Net_blue, P_BDT_blue, colour = Reliability)) +
  geom_line() +
  coord_cartesian(xlim =c(-3, 3), ylim = c(0, 1))


data_to_fit = likelihood_final[which(likelihood_final$Done.by=='Divyaj'),]


ggplot(na.omit(data_to_fit), aes(Net_blue, P_blue, colour = Reliability)) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-3, 3), ylim = c(0, 1))

ggplot(na.omit(data_to_fit), aes(Net_blue, P_BDT_blue, colour = Reliability)) +
  geom_line() +
  coord_cartesian(xlim =c(-3, 3), ylim = c(0, 1))




# data_to_fit = likelihood_final[which(likelihood_final$Done.by=='Exp1'),]
data_to_fit = likelihood_final[which(likelihood_final$Done.by=='Exp6'),]


ggplot(na.omit(data_to_fit), aes(Net_blue, P_blue, colour = Reliability)) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-3, 3), ylim = c(0, 1))

ggplot(na.omit(data_to_fit), aes(Net_blue, P_BDT_blue, colour = Reliability)) +
  geom_line() +
  coord_cartesian(xlim =c(-3, 3), ylim = c(0, 1))

####### Visualize fitted probability vs. Net number of square favoring blue
library(lme4)
library(sjPlot)

data_processed$Done.by = as.factor(data_processed$Done.by)
data_processed$Response = as.numeric(data_processed$Response)
data_processed$Optimalchoice = as.numeric(data_processed$Optimalchoice)



data_to_fit = rbind(data_processed[data_processed$Done.by=='Exp6',],data_processed[data_processed$Done.by!='Exp6',])
data_to_fit$Done.by = relevel(data_to_fit$Done.by, ref = "Exp6")
# data_to_fit = data_processed # somehow this has a convergence issue
model = glmer(Response ~ (1 | Participant.Private.ID) + Done.by * Net.Blue.50 + Done.by * Net.Blue.55 + Done.by * Net.Blue.65, data = data_to_fit, family = binomial)

model <- glmer(
  Response ~ (1 | Participant.Private.ID) +
    Done.by * Net.Blue.50 +
    Done.by * Net.Blue.55 +
    Done.by * Net.Blue.65,
  data = data_to_fit,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)




summary(model)

plot_model(model, type = "pred", terms = c("Net.Blue.50","Done.by"), axis.lim = c(0, 1))
plot_model(model, type = "pred", terms = c("Net.Blue.55","Done.by"), axis.lim = c(0, 1))
plot_model(model, type = "pred", terms = c("Net.Blue.65","Done.by"), axis.lim = c(0, 1))

p.50 <- plot_model(model, type = "pred", terms = c("Net.Blue.50","Done.by"), axis.lim = c(0, 1))
p.55 <- plot_model(model, type = "pred", terms = c("Net.Blue.55","Done.by"), axis.lim = c(0, 1))
p.65 <- plot_model(model, type = "pred", terms = c("Net.Blue.65","Done.by"), axis.lim = c(0, 1))


pred.s1.prob = p.50[["data"]][["predicted"]]
pred.s1.se = p.50[["data"]][["std.error"]]
pred.s1.conf.low = p.50[["data"]][["conf.low"]]
pred.s1.conf.high = p.50[["data"]][["conf.high"]]
pred.s1.x = p.50[["data"]][["x"]]
pred.s1.group = p.50[["data"]][["group"]]

pred.s2.prob = p.55[["data"]][["predicted"]]
pred.s2.se = p.55[["data"]][["std.error"]]
pred.s2.conf.low = p.55[["data"]][["conf.low"]]
pred.s2.conf.high = p.55[["data"]][["conf.high"]]
pred.s2.x = p.55[["data"]][["x"]]
pred.s2.group = p.55[["data"]][["group"]]

pred.s3.prob = p.65[["data"]][["predicted"]]
pred.s3.se = p.65[["data"]][["std.error"]]
pred.s3.conf.low = p.65[["data"]][["conf.low"]]
pred.s3.conf.high = p.65[["data"]][["conf.high"]]
pred.s3.x = p.65[["data"]][["x"]]
pred.s3.group = p.65[["data"]][["group"]]







# data_to_fit = data_processed
data_to_fit = data_to_fit

model.BDT = glmer(Optimalchoice ~ (1 | Participant.Private.ID) + Done.by * Net.Blue.50 + Done.by * Net.Blue.55 + Done.by * Net.Blue.65, data = data_to_fit, family = binomial)
# model.BDT = glmer(Optimalchoice ~ (1 | Participant.Private.ID) + Net.Blue.50 + Net.Blue.55 + Net.Blue.65, data = data_to_fit, family = binomial)

# no random variable
# model.BDT <- glm(Optimalchoice ~ Done.by * Net.Blue.50 + Done.by * Net.Blue.55 + Done.by * Net.Blue.65,
#                    data = data_to_fit, family = binomial)

# model.BDT <- glm(Optimalchoice ~ Net.Blue.50 + Net.Blue.55 + Net.Blue.65,
#                  data = data_to_fit, family = binomial)


summary(model.BDT)

plot_model(model.BDT, type = "pred", terms = c("Net.Blue.50","Done.by"), axis.lim = c(0, 1))
plot_model(model.BDT, type = "pred", terms = c("Net.Blue.55","Done.by"), axis.lim = c(0, 1))
plot_model(model.BDT, type = "pred", terms = c("Net.Blue.65","Done.by"), axis.lim = c(0, 1))

plot_model(model.BDT, type = "pred", terms = c("Net.Blue.50"), axis.lim = c(0, 1))
plot_model(model.BDT, type = "pred", terms = c("Net.Blue.55"), axis.lim = c(0, 1))
plot_model(model.BDT, type = "pred", terms = c("Net.Blue.65"), axis.lim = c(0, 1))

tab <- with(data_to_fit, table(Net.Blue.50, Net.Blue.55, Net.Blue.65, Optimalchoice))
tab






####### prediction and visualization
library(tidyr)

# Range for sweeping predictor
blue_range = c(-6:6)

# Two experiment levels
Done_by = c("Alex", "Divyaj","Exp1")

# Create data grid for each Net.Blue variable
new_dat <- expand.grid(
  Net.Blue.50 = 0,
  Net.Blue.55 = 0,
  Net.Blue.65 = 0,
  Done.by = Done_by,
  Var = c("Net.Blue.50", "Net.Blue.55", "Net.Blue.65"),  # to identify which is being swept
  x = blue_range
)

# For each row, assign the sweeping variable's value
new_dat <- new_dat %>%
  mutate(
    Net.Blue.50 = ifelse(Var == "Net.Blue.50", x, Net.Blue.50),
    Net.Blue.55 = ifelse(Var == "Net.Blue.55", x, Net.Blue.55),
    Net.Blue.65 = ifelse(Var == "Net.Blue.65", x, Net.Blue.65)
  )

new_dat_BDT = new_dat

new_dat$pred_logit <- predict(model, newdata = new_dat, re.form = NA)  # fixed effects only

new_dat$pred_prob <- plogis(new_dat$pred_logit)


new_dat_BDT$pred_logit <- predict(model.BDT, newdata = new_dat_BDT, re.form = NA)  # fixed effects only
new_dat_BDT$pred_prob <- plogis(new_dat_BDT$pred_logit)


### try predicting CI


# Function to generate fitted values for bootstrap
pred_fun <- function(fit) {
  plogis(predict(fit, newdata = new_dat, re.form = NA))
}

# Parametric bootstrap
set.seed(123)
boot_res <- bootMer(model, FUN = pred_fun, nsim = 20)

# 95% intervals for each row of new_dat
new_dat$pred_low  <- apply(boot_res$t, 2, quantile, 0.025)
new_dat$pred_high <- apply(boot_res$t, 2, quantile, 0.975)


###

new_dat <- new_dat %>%
  rename(Net_blue = x,
         Reliability = Var,
         P_blue = pred_prob)  # rename the "Var" column to "P_blue"

new_dat <- new_dat %>%
  mutate(
    Reliability = gsub("Net\\.Blue\\.", "", Reliability),  # remove the text part
    Reliability = as.numeric(Reliability)
  )

new_dat_long <- new_dat %>%
  select(Done.by, Net_blue, Reliability, P_blue)


new_dat_BDT <- new_dat_BDT %>%
  rename(Net_blue = x,
         Reliability = Var,
         P_blue = pred_prob)  # rename the "Var" column to "P_blue"

new_dat_BDT <- new_dat_BDT %>%
  mutate(
    Reliability = gsub("Net\\.Blue\\.", "", Reliability),  # remove the text part
    Reliability = as.numeric(Reliability)
  )

new_dat_BDT_long <- new_dat_BDT %>%
  select(Done.by, Net_blue, Reliability, P_blue)

#######


ggplot(new_dat_long[which(new_dat_long$Done.by=='Alex'),], aes(Net_blue, P_blue, colour = as.factor(Reliability))) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-6, 6), ylim = c(0, 1))


ggplot(new_dat_long[which(new_dat_long$Done.by=='Divyaj'),], aes(Net_blue, P_blue, colour = as.factor(Reliability))) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-6, 6), ylim = c(0, 1))


ggplot(new_dat_long[which(new_dat_long$Done.by=='Exp1'),], aes(Net_blue, P_blue, colour = as.factor(Reliability))) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-6, 6), ylim = c(0, 1))




ggplot(new_dat_BDT_long[which(new_dat_BDT_long$Done.by=='Alex'),], aes(Net_blue, P_blue, colour = as.factor(Reliability))) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-6, 6), ylim = c(0, 1))


ggplot(new_dat_BDT_long[which(new_dat_BDT_long$Done.by=='Divyaj'),], aes(Net_blue, P_blue, colour = as.factor(Reliability))) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-6, 6), ylim = c(0, 1))


ggplot(new_dat_BDT_long[which(new_dat_BDT_long$Done.by=='Exp1'),], aes(Net_blue, P_blue, colour = as.factor(Reliability))) +
  # geom_point() +
  geom_line() +
  coord_cartesian(xlim =c(-6, 6), ylim = c(0, 1))



print(new_dat_BDT_long[which(new_dat_BDT_long$Done.by=='Exp1'),])


