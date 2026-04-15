library(lme4)
library(sjPlot)
library(ggplot2)

rm(list=ls())
load('./results/data_preprocessed.rdata')

# red_row = which(data_processed$Correct.Response==0)
red_row = which(data_processed$Optimalchoice==0)

data_processed$OptimalAccuracy = 0
optimal_row = which(data_processed$Response==data_processed$Optimalchoice)
data_processed$OptimalAccuracy[optimal_row]=1

data_processed$Net.Correct.50 = data_processed$Net.Blue.50
data_processed$Net.Correct.55 = data_processed$Net.Blue.55
data_processed$Net.Correct.65 = data_processed$Net.Blue.65

data_processed$Net.Correct.50[red_row] = 0 - data_processed$Net.Correct.50[red_row]
data_processed$Net.Correct.55[red_row] = 0 - data_processed$Net.Correct.55[red_row]
data_processed$Net.Correct.65[red_row] = 0 - data_processed$Net.Correct.65[red_row]

data_processed$Done.by = as.factor(data_processed$Done.by)
data_processed$Accuracy = as.numeric(data_processed$Accuracy)
data_processed$OptimalAccuracy = as.numeric(data_processed$OptimalAccuracy)

data_to_fit = rbind(data_processed[data_processed$Done.by=='Exp6',],data_processed[data_processed$Done.by!='Exp6',])
# data_to_fit$Done.by = relevel(data_to_fit$Done.by, ref = "Exp6")

model = glmer(Accuracy ~ (1 | Participant.Private.ID) + Done.by * Net.Correct.50 + Done.by * Net.Correct.55 + Done.by * Net.Correct.65, data = data_to_fit, family = binomial)
# model = lmer(Accuracy ~ (1 | Participant.Private.ID) + Done.by * Net.Correct.50 + Done.by * Net.Correct.55 + Done.by * Net.Correct.65, data = data_to_fit)

summary(model)

p.50 <- plot_model(model, type = "pred", terms = c("Net.Correct.50","Done.by"), axis.lim = c(0, 1))
p.55 <- plot_model(model, type = "pred", terms = c("Net.Correct.55","Done.by"), axis.lim = c(0, 1))
p.65 <- plot_model(model, type = "pred", terms = c("Net.Correct.65","Done.by"), axis.lim = c(0, 1))


p.50
p.55
p.65

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



# source 1
df1 <- data.frame(
  x       = pred.s1.x,
  prob    = pred.s1.prob,
  se      = pred.s1.se,
  conf.low = pred.s1.conf.low,
  conf.high = pred.s1.conf.high,
  group   = pred.s1.group,
  series  = "50%"
)

# source 2
df2 <- data.frame(
  x       = pred.s2.x,
  prob    = pred.s2.prob,
  se      = pred.s2.se,
  conf.low = pred.s2.conf.low,
  conf.high = pred.s2.conf.high,
  group   = pred.s2.group,
  series  = "55%"
)

# source 3
df3 <- data.frame(
  x       = pred.s3.x,
  prob    = pred.s3.prob,
  se      = pred.s3.se,
  conf.low = pred.s3.conf.low,
  conf.high = pred.s3.conf.high,
  group   = pred.s3.group,
  series  = "65%"
)

df_all <- rbind(df1, df2, df3)

my_colors = c('#0099FF', '#00CC33', '#009933')

ggplot(df_all, aes(x = x, y = prob, color = series, fill = series)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.20, color = NA) +
  facet_wrap(~ group) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Net Number of Correct Cues",
       y = "Predicted Accuracy",
       color = "Reliability",
       fill = "Reliability") +
  theme_bw()


library(patchwork)


(p.50 +
    scale_color_manual(values = c("#0099FF", "#0099FF", "#0099FF"))+
    scale_fill_manual(values = c("#0099FF", "#0099FF", "#0099FF")) +
    aes(linetype = group) +
    scale_linetype_manual(values = c("solid", "longdash", "dotted")))|
  
  (p.55 +
     scale_color_manual(values = c("#00CC33", "#00CC33", "#00CC33"))+
     scale_fill_manual(values = c("#00CC33", "#00CC33", "#00CC33")) +
     aes(linetype = group) +
     scale_linetype_manual(values = c("solid", "longdash", "dotted")))|
  
  (p.65 +
     scale_color_manual(values = c("#009933", "#009933", "#009933"))+
     scale_fill_manual(values = c("#009933", "#009933", "#009933")) +
     aes(linetype = group) +
     scale_linetype_manual(values = c("solid", "longdash", "dotted")))





######## different way of plotting #########

library(lme4)
library(sjPlot)
library(ggplot2)

rm(list = setdiff(ls(), c("data_processed", "data_to_fit")))


for (i in -5:5) {
  print(sum(which(data_processed$Net.Correct.50==i)))
}

df_num_50 = data.frame(num_50 = c(-5:5))
for (i in df_num_50$num_50) {
  df_num_50$num_trials[which(df_num_50$num_50==i)] = sum(which(data_processed$Net.Correct.50==i))
}

data_processed$Absolute.Posterior = abs(data_processed$PosteriorBlue)

hist(data_processed$Absolute.Posterior, breaks = 2000)

# hist(log(data_processed$Absolute.Posterior), xlim = c(-5,2))


data_to_fit = rbind(data_processed[data_processed$Done.by=='Exp6',],data_processed[data_processed$Done.by!='Exp6',])

# model = glmer(Accuracy ~ (1 | Participant.Private.ID) + Done.by * Net.Correct.50 * Absolute.Posterior, data = data_to_fit, family = binomial)
model = glmer(OptimalAccuracy ~ (1 | Participant.Private.ID) + Done.by * Net.Correct.50 * Absolute.Posterior, data = data_to_fit, family = binomial)

# model = lmer(Accuracy ~ (1 | Participant.Private.ID) + Done.by * Net.Correct.50 * Absolute.Posterior, data = data_to_fit)

summary(model)


# plot_model(model, 
#            type = "pred", 
#            terms = c("Absolute.Posterior", "Net.Correct.50[all]", "Done.by"),
#            ci.lvl = 0.95) +
#   labs(y = "Predicted Accuracy", 
#        title = "Predicted Accuracy by Posterior and Net Correct") +
#   theme_minimal()





library(ggeffects)

pr <- predict_response(model, terms = c("Absolute.Posterior [0:2 by=0.1]", "Net.Correct.50 [all]", "Done.by"))


pr$group_num <- as.numeric(as.character(pr$group))


ggplot(pr, aes(x = x, y = predicted, group = group_num, color = group_num)) +
  geom_line(size = 1, alpha = 0.8) +
  facet_wrap(~facet) +
  scale_color_viridis_c(option = "plasma") + 
  labs(
    title = "Accuracy Prediction by Net Correct Score",
    x = "Absolute Posterior",
    y = "Predicted Accuracy",
    color = "Net.Correct.50"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


############ Try to calculate the loss of optimal accuracy


data_processed$round.abs.post = round(data_processed$Absolute.Posterior, digits = 5)
uniq.abs.post = unique(data_processed$round.abs.post)
net.correct.50 = c(-5:5)

freq_matrix = expand.grid(net.correct.50 = net.correct.50, uniq.abs.post = uniq.abs.post)
freq_matrix$freq = 0

for (i in 1:nrow(freq_matrix)) {
  print(i)
  i_50 = freq_matrix$net.correct.50[i]
  i_post = freq_matrix$uniq.abs.post[i]
  freq_matrix$freq[i] = length(which(data_processed$round.abs.post==i_post & data_processed$Net.Correct.50==i_50))
}

sum(freq_matrix$freq)

freq_matrix$freq = freq_matrix$freq/sum(freq_matrix$freq)

predicted_Alex = predict(model, data.frame(Done.by = 'Alex', 
                                           Net.Correct.50 = freq_matrix$net.correct.50, 
                                           Absolute.Posterior = freq_matrix$uniq.abs.post),
                         re.form = NA,
                         type = "response")

predicted_Divyaj = predict(model, data.frame(Done.by = 'Divyaj', 
                                             Net.Correct.50 = freq_matrix$net.correct.50, 
                                             Absolute.Posterior = freq_matrix$uniq.abs.post),
                           re.form = NA,
                           type = "response")

predicted_Exp6 = predict(model, data.frame(Done.by = 'Exp6', 
                                             Net.Correct.50 = freq_matrix$net.correct.50, 
                                             Absolute.Posterior = freq_matrix$uniq.abs.post),
                           re.form = NA,
                         type = "response")

baseline_Alex = predict(model, data.frame(Done.by = 'Alex', 
                                           Net.Correct.50 = 0, 
                                           Absolute.Posterior = freq_matrix$uniq.abs.post),
                         re.form = NA,
                        type = "response")

baseline_Divyaj = predict(model, data.frame(Done.by = 'Divyaj', 
                                             Net.Correct.50 = 0, 
                                             Absolute.Posterior = freq_matrix$uniq.abs.post),
                           re.form = NA,
                          type = "response")

baseline_Exp6 = predict(model, data.frame(Done.by = 'Exp6', 
                                           Net.Correct.50 = 0, 
                                           Absolute.Posterior = freq_matrix$uniq.abs.post),
                         re.form = NA,
                        type = "response")

freq_matrix = cbind(freq_matrix, predicted_Alex, predicted_Divyaj, predicted_Exp6,
                    baseline_Alex, baseline_Divyaj, baseline_Exp6)

freq_matrix$gain_loss_Alex = freq_matrix$predicted_Alex-freq_matrix$baseline_Alex
freq_matrix$gain_loss_Divyaj = freq_matrix$predicted_Divyaj - freq_matrix$baseline_Divyaj
freq_matrix$gain_loss_Exp6 = freq_matrix$predicted_Exp6-freq_matrix$baseline_Exp6

freq_matrix$multiplied_Alex = freq_matrix$freq * freq_matrix$gain_loss_Alex
freq_matrix$multiplied_Divyaj = freq_matrix$freq * freq_matrix$gain_loss_Divyaj
freq_matrix$multiplied_Exp6 = freq_matrix$freq * freq_matrix$gain_loss_Exp6


sum(freq_matrix$multiplied_Alex[which(freq_matrix$multiplied_Alex>0)])
sum(freq_matrix$multiplied_Alex[which(freq_matrix$multiplied_Alex<0)])
sum(freq_matrix$multiplied_Divyaj[which(freq_matrix$multiplied_Divyaj>0)])
sum(freq_matrix$multiplied_Divyaj[which(freq_matrix$multiplied_Divyaj<0)])
sum(freq_matrix$multiplied_Exp6[which(freq_matrix$multiplied_Exp6>0)])
sum(freq_matrix$multiplied_Exp6[which(freq_matrix$multiplied_Exp6<0)])

final_gain_loss_Alex = sum(freq_matrix$multiplied_Alex)
final_gain_loss_Divyaj = sum(freq_matrix$multiplied_Divyaj)
final_gain_loss_Exp6 = sum(freq_matrix$multiplied_Exp6)


final_gain_loss_Alex
final_gain_loss_Divyaj
final_gain_loss_Exp6


########### Try another mixed models to see contributions of parameters
library(posterior)
library(ggplot2)
library(dplyr)
library(tidybayes)
rm(list = setdiff(ls(), c("data_processed")))

## this should really be done in the preprocessing
data_processed <- data_processed %>%
  group_by(Done.by) %>%
  mutate(new_id = as.numeric(factor(Participant.Private.ID, levels = unique(Participant.Private.ID)))) %>%
  ungroup()

data_processed$alpha = NA
data_processed$beta = NA
data_processed$lambda = NA
data_processed$theta = NA
data_processed$psi = NA


chosen_exps = c('Alex','Divyaj','Exp6')


for (chosen_exp in chosen_exps) {
  
  # additional_suffix = '_original_edit'
  # additional_suffix = '_redo'
  additional_suffix = ''
  
  fit <- readRDS(paste('./results/',chosen_exp,'/log_full',additional_suffix,'.rds',sep = ''))
  
  
  indiv_draws <- fit %>%
    spread_draws(params[n, v])

  
  indiv_summary <- indiv_draws %>%
    group_by(n, v) %>%
    summarise(
      mean_val = mean(params),
      lower_95 = quantile(params, 0.025),
      upper_95 = quantile(params, 0.975)
    )
  
  for (i in 1:max(indiv_summary$n)) {
    selected_row = which(data_processed$new_id==i & data_processed$Done.by==chosen_exp)
    alpha_ = indiv_summary$mean_val[which(indiv_summary$n==i&indiv_summary$v==1)]
    beta_ = indiv_summary$mean_val[which(indiv_summary$n==i&indiv_summary$v==2)]
    lambda_ = indiv_summary$mean_val[which(indiv_summary$n==i&indiv_summary$v==3)]
    theta_ = indiv_summary$mean_val[which(indiv_summary$n==i&indiv_summary$v==4)]
    psi_ = indiv_summary$mean_val[which(indiv_summary$n==i&indiv_summary$v==5)]
    
    data_processed$alpha[selected_row] = alpha_
    data_processed$beta[selected_row] = beta_
    data_processed$lambda[selected_row] = lambda_
    data_processed$theta[selected_row] = theta_
    data_processed$psi[selected_row] = psi_
    
  }
}

data_to_fit = data_processed

## plan A
model.A = glm(Accuracy ~ alpha + beta + lambda + theta + psi, data = data_to_fit, family = binomial)

summary(model.A)

## plan B

df_id_unique <- data_to_fit %>%
  select(Done.by, new_id, alpha, beta, lambda, theta, psi) %>%
  distinct()
df_id_unique$optimal_accuracy = 0
for (i in 1:nrow(df_id_unique)) {
  id_ = df_id_unique$new_id[i]
  exp_ = df_id_unique$Done.by[i]
  df_indiv = data_processed[which(data_processed$new_id==id_ & data_processed$Done.by==exp_),]
  df_id_unique$optimal_accuracy[i] = sum(df_indiv$OptimalAccuracy)/nrow(df_indiv)
  
}

model.B = lm(optimal_accuracy ~ alpha + beta + lambda + theta + psi, data = df_id_unique)
summary(model.B)

car::vif(model.B)
plot(model.B, which = 2)

model.C = lm(optimal_accuracy ~ (alpha + beta + lambda + theta + psi)^2, 
             data = df_id_unique)
summary(model.C)
car::vif(model.C)

anova(model.B, model.C)

model.D = lm(optimal_accuracy ~ alpha + beta + lambda + theta + psi + 
               alpha:theta + alpha:psi + beta:psi + theta:psi, 
             data = df_id_unique)
summary(model.D)
car::vif(model.D)

anova(model.C, model.D)
anova(model.B, model.D)


par(mfrow = c(1,5))
plot(df_id_unique$alpha, df_id_unique$optimal_accuracy)
plot(df_id_unique$beta, df_id_unique$optimal_accuracy)
plot(df_id_unique$lambda, df_id_unique$optimal_accuracy)
plot(df_id_unique$theta, df_id_unique$optimal_accuracy)
plot(df_id_unique$psi, df_id_unique$optimal_accuracy)





