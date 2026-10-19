############################
## Performance Statistics ##
############################

load('./results/performance.rdata')

#### Visualize 

# par(mfrow = c(1, 3), oma = c(0, 0, 3, 0))
par(mfrow = c(1, 4), oma = c(0, 0, 3, 0))

plot_performance = function(df_performance, exp_done.by, second_plot, main_text){
  plot_df = df_performance[which(df_performance$Done.by==exp_done.by),]
  plot_df = plot_df[order(plot_df$Accuracy),]
  
  plot(plot_df$Accuracy,
       type = "p",
       pch = 19,
       xlab = "Participants",
       ylab = "Performance",
       ylim = c(0.4,1),
       main = main_text)
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
plot_performance(performance, 'Exp6', 'BDT', 'Control')
plot_performance(performance, 'Alex', 'BDT', 'Mitigation 1')
plot_performance(performance, 'Divyaj', 'BDT', 'Mitigation 2')
plot_performance(performance, 'mitigation3', 'BDT', 'Mitigation 3')


mtext("Participants vs. BDT", outer = TRUE, cex = 1.5, font = 2)

### Optimal Choices vs. Correct Choices

par(mfrow = c(1, 4), oma = c(0, 0, 3, 0))

# plot_performance(performance, 'Exp1', 'Optimal')
plot_performance(performance, 'Exp6', 'Optimal', 'Control')
plot_performance(performance, 'Alex', 'Optimal', 'Mitigation 1')
plot_performance(performance, 'Divyaj', 'Optimal', 'Mitigation 2')
plot_performance(performance, 'mitigation3', 'Optimal', 'Mitigation 3')

mtext("Correct Choices vs. Optimal Choices", outer = TRUE, cex = 1.5, font = 2)

### stats of performance

## confirm normality

shapiro.test(performance$Accuracy[performance$Done.by=='Exp6'])
shapiro.test(performance$Accuracy[performance$Done.by=='Alex'])
shapiro.test(performance$Accuracy[performance$Done.by=='Divyaj'])
shapiro.test(performance$Accuracy[performance$Done.by=='mitigation3'])

shapiro.test(performance$BDT_accuracy[performance$Done.by=='Exp6'])
shapiro.test(performance$BDT_accuracy[performance$Done.by=='Alex'])
shapiro.test(performance$BDT_accuracy[performance$Done.by=='Divyaj'])
shapiro.test(performance$BDT_accuracy[performance$Done.by=='mitigation3'])

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
shapiro.test(performance$Optimal_Choice[performance$Done.by=='mitigation3'])





##########################################
## Choosing blue vs. net number of blue ##
##########################################
library(lme4)
library(sjPlot)
library(ggplot2)
library(dplyr)
library(readxl)

rm(list=ls())

BDT_choice <- read_excel("data/Exp6/exp6_BDTchoice.xlsx")
load('./results/data_preprocessed.rdata')

#### simulated BDT
model.BDT = glm(resp~source1+source2+source3, data = BDT_choice, family = binomial)
BDT.50 = plot_model(model.BDT, type = "pred", terms = c("source1"), axis.lim = c(0, 1))
BDT.55 = plot_model(model.BDT, type = "pred", terms = c("source2"), axis.lim = c(0, 1))
BDT.65 = plot_model(model.BDT, type = "pred", terms = c("source3"), axis.lim = c(0, 1))

pred.s1.prob = BDT.50[["data"]][["predicted"]]
pred.s1.se = BDT.50[["data"]][["std.error"]]
pred.s1.conf.low = BDT.50[["data"]][["conf.low"]]
pred.s1.conf.high = BDT.50[["data"]][["conf.high"]]
pred.s1.x = BDT.50[["data"]][["x"]]

pred.s2.prob = BDT.55[["data"]][["predicted"]]
pred.s2.se = BDT.55[["data"]][["std.error"]]
pred.s2.conf.low = BDT.55[["data"]][["conf.low"]]
pred.s2.conf.high = BDT.55[["data"]][["conf.high"]]
pred.s2.x = BDT.55[["data"]][["x"]]

pred.s3.prob = BDT.65[["data"]][["predicted"]]
pred.s3.se = BDT.65[["data"]][["std.error"]]
pred.s3.conf.low = BDT.65[["data"]][["conf.low"]]
pred.s3.conf.high = BDT.65[["data"]][["conf.high"]]
pred.s3.x = BDT.65[["data"]][["x"]]

df1.BDT <- data.frame(
  x       = pred.s1.x,
  prob    = pred.s1.prob,
  se      = pred.s1.se,
  conf.low = pred.s1.conf.low,
  conf.high = pred.s1.conf.high,
  group   = 'BDT',
  series  = "50%"
)

df2.BDT <- data.frame(
  x       = pred.s2.x,
  prob    = pred.s2.prob,
  se      = pred.s2.se,
  conf.low = pred.s2.conf.low,
  conf.high = pred.s2.conf.high,
  group   = 'BDT',
  series  = "55%"
)

df3.BDT <- data.frame(
  x       = pred.s3.x,
  prob    = pred.s3.prob,
  se      = pred.s3.se,
  conf.low = pred.s3.conf.low,
  conf.high = pred.s3.conf.high,
  group   = 'BDT',
  series  = "65%"
)

df_all.BDT = rbind(df1.BDT, df2.BDT, df3.BDT)

#### experiments
data_processed$Done.by = as.factor(data_processed$Done.by)
data_processed$Response = as.numeric(data_processed$Response)
data_processed$Optimalchoice = as.numeric(data_processed$Optimalchoice)

data_to_fit = rbind(data_processed[data_processed$Done.by=='Exp6',],data_processed[data_processed$Done.by!='Exp6',])
data_to_fit$Done.by = relevel(data_to_fit$Done.by, ref = "Exp6")
# data_to_fit = data_processed # somehow this has a convergence issue
# model = glmer(Response ~ (1 | Participant.Private.ID) + Done.by * Net.Blue.50 + Done.by * Net.Blue.55 + Done.by * Net.Blue.65, data = data_to_fit, family = binomial)


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

# plot_model(model, type = "pred", terms = c("Net.Blue.50","Done.by"), axis.lim = c(0, 1))
# plot_model(model, type = "pred", terms = c("Net.Blue.55","Done.by"), axis.lim = c(0, 1))
# plot_model(model, type = "pred", terms = c("Net.Blue.65","Done.by"), axis.lim = c(0, 1))

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
  
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.3, color = NA) +
  geom_line(size = 0.5) +
  facet_wrap(~ group) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Net Number of Blue",
       y = "Predicted Probability of Choosing Blue",
       color = "Reliability",
       fill = "Reliability") +
  theme_bw()


library(tidyverse)

df_all.BDT = rbind(df_all.BDT,df_all)

df_all_sorted <- df_all.BDT %>%
  arrange(series, group, x)

df_all_sorted$group <- factor(df_all_sorted$group, 
                              levels = c("Exp6", "Alex", "Divyaj", "mitigation3", "BDT"))

ggplot(df_all_sorted, aes(x = x, y = prob, color = series, fill = series, 
                          group = interaction(series, group))) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, 
              color = NA) +
  
  # b. 后画 Line，增加终极修正：`group = group`
  geom_line(aes(linetype = group, # 映射线型
                group = group),   # 终极修正：显式指定绘图分组
            size = 0.6,
            alpha = 0.8,
            # 增加位置微调，在视觉上进一步杜绝重叠和勾连
            position = position_dodge(width = 0.1)) + 
  
  # c. 分面与标度
  facet_wrap(~ series) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_linetype_manual(values = c("Exp6" = "solid", 
                                   "Alex" = "longdash", 
                                   "Divyaj" = "dotted",
                                   "mitigation3" = "twodash",
                                   "BDT" = "dotdash"),
                        labels = c("Exp6" = "Control",
                                   "Alex" = "Mitigation 1",
                                   "Divyaj" = "Mitigation 2",
                                   "mitigation3" = "Mitigation 3",
                                   "BDT" = "BDT")) +
  
  # d. 其他设置
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  labs(x = "Net Number of Blue",
       y = "Predicted Probability of Choosing Blue",
       color = "Reliability",
       fill = "Reliability",
       linetype = "Experiment Group") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines"),
        legend.position = "right")





library(patchwork)


(p.50 +
    scale_color_manual(values = c("#F8766D", "#F8766D", "#F8766D","#F8766D"))+
    scale_fill_manual(values = c("#F8766D", "#F8766D", "#F8766D","#F8766D")) +
    aes(linetype = group) +
    scale_linetype_manual(values = c("solid", "longdash", "dotted","twodash")))|
  
  (p.55 +
     scale_color_manual(values = c("#00BA38", "#00BA38", "#00BA38", "#00BA38"))+
     scale_fill_manual(values = c("#00BA38", "#00BA38", "#00BA38", "#00BA38")) +
     aes(linetype = group) +
     scale_linetype_manual(values = c("solid", "longdash", "dotted","twodash")))|
  
  (p.65 +
     scale_color_manual(values = c("#619CFF", "#619CFF", "#619CFF", "#619CFF"))+
     scale_fill_manual(values = c("#619CFF", "#619CFF", "#619CFF", "#619CFF")) +
     aes(linetype = group) +
     scale_linetype_manual(values = c("solid", "longdash", "dotted","twodash")))




################################################
## Slider response vs. Net congruent evidence ##
################################################
load('./results/data_preprocessed.rdata')

rm(list=setdiff(ls(), "data_processed"))

# chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'
chosen_exp = 'mitigation3'



data = data_processed[which(data_processed$Done.by==chosen_exp),]
# data = data_processed

data$Net.Cong.50 = data$Net.Blue.50
data$Net.Cong.55 = data$Net.Blue.55
data$Net.Cong.65 = data$Net.Blue.65

data$Net.Cong.50[which(data$Response==0)] = 0 - data$Net.Cong.50[which(data$Response==0)]
data$Net.Cong.55[which(data$Response==0)] = 0 - data$Net.Cong.55[which(data$Response==0)]
data$Net.Cong.65[which(data$Response==0)] = 0 - data$Net.Cong.65[which(data$Response==0)]

data$slider_response = as.numeric(data$slider_response)-50

data$slider_reliability = as.factor(data$slider_reliability)
data$Participant.Private.ID = as.factor(data$Participant.Private.ID)


library(lme4)



lm.slider = lmer(slider_response ~  (1 | Participant.Private.ID) + slider_reliability * Net.Cong.50 + slider_reliability * Net.Cong.55 + slider_reliability * Net.Cong.65, data = data)

lm.slider = lmer(slider_response ~  (1 | Participant.Private.ID) + slider_reliability * Net.Cong.50 + slider_reliability * Net.Cong.55 + slider_reliability * Net.Cong.65, data = data)

summary(lm.slider)
library(sjPlot)

# plot_model(lm.slider, type = "pred", terms = c("Net.Cong.50","slider_reliability"), axis.lim = c(-50, 50))
# plot_model(lm.slider, type = "pred", terms = c("Net.Cong.55","slider_reliability"), axis.lim = c(-50, 50))
# plot_model(lm.slider, type = "pred", terms = c("Net.Cong.65","slider_reliability"), axis.lim = c(-50, 50))


p.50 <- plot_model(lm.slider, type = "pred", terms = c("Net.Cong.50","slider_reliability"), axis.lim = c(-50, 50))
p.55 <- plot_model(lm.slider, type = "pred", terms = c("Net.Cong.55","slider_reliability"), axis.lim = c(-50, 50))
p.65 <- plot_model(lm.slider, type = "pred", terms = c("Net.Cong.65","slider_reliability"), axis.lim = c(-50, 50))


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


# s1
df1 <- data.frame(
  x       = pred.s1.x,
  prob    = pred.s1.prob,
  se      = pred.s1.se,
  conf.low = pred.s1.conf.low,
  conf.high = pred.s1.conf.high,
  group   = pred.s1.group,
  series  = "50%"
)

# s2
df2 <- data.frame(
  x       = pred.s2.x,
  prob    = pred.s2.prob,
  se      = pred.s2.se,
  conf.low = pred.s2.conf.low,
  conf.high = pred.s2.conf.high,
  group   = pred.s2.group,
  series  = "55%"
)

# s3
df3 <- data.frame(
  x       = pred.s3.x,
  prob    = pred.s3.prob,
  se      = pred.s3.se,
  conf.low = pred.s3.conf.low,
  conf.high = pred.s3.conf.high,
  group   = pred.s3.group,
  series  = "65%"
)

df1 = df1[which(df1$group=='50'),]
df2 = df2[which(df2$group=='55'),]
df3 = df3[which(df3$group=='65'),]


df_all <- rbind(df1, df2, df3)

my_colors = c('#0099FF', '#00CC33', '#009933')

ggplot(df_all, aes(x = x, y = prob, color = series, fill = series)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.20, color = NA) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_y_continuous(limits = c(-50, 50)) +
  scale_x_continuous(limits = c(-6, 6)) +
  labs(title = chosen_exp,
       x = "Net Number of Congruent Evidence",
       y = "Rating of Introspective Report",
       color = "Reliability",
       fill = "Reliability") +
  theme_bw()


#################################
## learning and sliding window ##
#################################


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

accuracy_window_exp_df$Done.by = factor(accuracy_window_exp_df$Done.by, levels = c("Exp6", "Alex", "Divyaj", "mitigation3"))


ggplot(accuracy_window_exp_df, aes(Window_end, Accuracy))+
  geom_line(aes(colour = Done.by))+
  theme_classic() +
  labs(
    x = "Trial",
    y = "Accuracy",
    colour = "Experiment",
    title = ''
  ) + scale_colour_discrete(labels = c("Exp6" = "Control",
                                      "Alex" = "Mitigation 1",
                                      "Divyaj" = "Mitigation 2",
                                      "mitigation3" = "Mitigation 3"))
  



time_seq_df$Done.by = factor(time_seq_df$Done.by, levels = c("Exp6", "Alex", "Divyaj", "mitigation3"))

model.1 = lm(OptimalAccuracy~Trial*Done.by, data = time_seq_df)
model.2 = lmer(OptimalAccuracy~Trial*Done.by + Done.by + (1|Participant.Private.ID), data = time_seq_df)

lme4:::anova.merMod(model.2, model.1)

plot_model(model.1, type = "pred", terms = c("Trial","Done.by"))+
  theme_classic() +
  labs(
    x = "Trial",
    y = "Accuracy",
    colour = "Experiment",
    title = ''
  ) + scale_colour_discrete(labels = c("Exp6" = "Control",
                                       "Alex" = "Mitigation 1",
                                       "Divyaj" = "Mitigation 2",
                                       "mitigation3" = "Mitigation 3"))

  


