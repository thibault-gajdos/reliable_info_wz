library(ggplot2)
library(dplyr)

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

df_all <- rbind(df1, df2, df3)

my_colors = c('#0099FF', '#00CC33', '#009933')

ggplot(df_all, aes(x = x, y = prob, color = series, fill = series)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.20, color = NA) +
  facet_wrap(~ group) +
  # scale_color_manual(values = my_colors) +
  # scale_fill_manual(values = my_colors) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Net Number of Blue",
       y = "Predicted Probability",
       color = "Reliability",
       fill = "Reliability") +
  theme_bw()


library(patchwork)


(p.50 +
  scale_color_manual(values = c("#F8766D", "#F8766D", "#F8766D"))+
  scale_fill_manual(values = c("#F8766D", "#F8766D", "#F8766D")) +
  aes(linetype = group) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")))|

(p.55 +
  scale_color_manual(values = c("#00BA38", "#00BA38", "#00BA38"))+
  scale_fill_manual(values = c("#00BA38", "#00BA38", "#00BA38")) +
  aes(linetype = group) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")))|

(p.65 +
  scale_color_manual(values = c("#619CFF", "#619CFF", "#619CFF"))+
  scale_fill_manual(values = c("#619CFF", "#619CFF", "#619CFF")) +
  aes(linetype = group) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted")))

