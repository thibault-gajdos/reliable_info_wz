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

save(data_processed, file = './results/data_preprocessed.rdata')
save(performance, file = './results/performance.rdata')






########  the rest are not the most essential code for analysis and preprocessing  ########
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





