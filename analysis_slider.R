rm(list = setdiff(ls(), c('data_processed','df_participant_experiment','performance','likelihood_blue','likelihood_final')))

chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'



data = data_processed[which(data_processed$Done.by==chosen_exp),]

data$Net.Cong.50 = data$Net.Blue.50
data$Net.Cong.55 = data$Net.Blue.55
data$Net.Cong.65 = data$Net.Blue.65

data$Net.Cong.50[which(data$Response==0)] = 0 - data$Net.Cong.50[which(data$Response==0)]
data$Net.Cong.55[which(data$Response==0)] = 0 - data$Net.Cong.55[which(data$Response==0)]
data$Net.Cong.65[which(data$Response==0)] = 0 - data$Net.Cong.65[which(data$Response==0)]

data$slider_response = as.numeric(data$slider_response)-50




library(lme4)

lm.slider = lmer(slider_response ~ (1 | Participant.Private.ID) + slider_reliability * Net.Cong.50 + slider_reliability * Net.Cong.55 + slider_reliability * Net.Cong.65, data = data)

library(sjPlot)

plot_model(lm.slider, type = "pred", terms = c("Net.Cong.50","slider_reliability"), axis.lim = c(-50, 50))
plot_model(lm.slider, type = "pred", terms = c("Net.Cong.55","slider_reliability"), axis.lim = c(-50, 50))
plot_model(lm.slider, type = "pred", terms = c("Net.Cong.65","slider_reliability"), axis.lim = c(-50, 50))


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

