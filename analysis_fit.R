library(readr)
library(ggplot2)

# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'

#### extract the fit

########

# Extract posterior samples
if(chosen_exp =='Divyaj'){
  fit = read_rds(paste('./results/',chosen_exp,'/log_basic.rds', sep = ''), refhook = NULL)
  post <- rstan::extract(fit)
}else{
  fit = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)
  post <- rstan::extract(fit)
}

# Participant-level posterior means

a_mean <- apply(post$alpha, 2, mean)
b_mean <- apply(post$beta, 2, mean)
if (chosen_exp != 'Divyaj'){
  w_mean <- apply(post$w, c(2,3), mean)
}

# a_mean = alpha_mean
# b_mean = beta_mean


# Group means
a_g <- mean(a_mean)
b_g <- mean(b_mean)

a_g
b_g

w_g <- apply(w_mean, 2, mean)

w_g <- c(w_g, 1) # add 1 at the end


gamma_g <- exp(b_g)

# Objective reliability grid
p_grid <- seq(0.001, 0.999, length.out = 200)

# Distortion function (Equation 6)
phi <- function(p, a, gamma) {
  (gamma * p^a) / (gamma * p^a + (1 - p)^a)
}

distorted <- phi(p_grid, a_g, gamma_g)


phi(0.5, a_g, gamma_g)

df <- data.frame(
  p = p_grid * 100,
  distorted = distorted * 100
)


highlight <- data.frame(
  p = c(0.5, 0.55, 0.65)
)

highlight$distorted <- phi(highlight$p, a_g, gamma_g)
highlight$p = highlight$p*100
highlight$distorted = highlight$distorted*100



ggplot(df, aes(x = p, y = distorted)) +
  # main sigmoid curve
  geom_line(linewidth = 1.6, colour = "black") +
  # identity line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # vertical guide lines
  geom_segment(
    data = highlight,
    aes(x = p, xend = p, y = 0, yend = distorted, col = '#F2F2F2'),
    linewidth = 0.6
  ) +
  # horizontal guide lines
  geom_segment(
    data = highlight,
    aes(x = 0, xend = p, y = distorted, yend = distorted, col = '#F2F2F2'),
    linewidth = 0.6
  ) +
  # highlighted points
  geom_point(
    data = highlight,
    aes(colour = factor(p)),
    size = 5
  ) +
  scale_colour_manual(
    values = c("50" = "#0099FF",
               "55" = "#00CC33",
               "65" = "#009933")
  ) +
  scale_x_continuous(
    limits = c(0, 100), breaks = c(0, 50, 55, 65, 100),
    expand = c(0, 1)
  ) +
  scale_y_continuous(
    limits = c(0, 100), breaks = c(0, 50, 65, 80, 100),
    expand = c(0, 1)
  )+
  labs(
    x = "Reliability",
    y = "Distorted reliability"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 1),
    axis.ticks.length = unit(6, "pt")
  )


##### plot w
df <- data.frame(
  no.seq = c(1:6),
  weights = w_g
)

ggplot(df, aes(x = no.seq, y = weights)) +
  # 1. Add vertical lines from the x-axis to the points
  geom_segment(aes(xend = no.seq, yend = 0.5), color = "grey70") +
  # 2. Add the main line
  geom_line(size = 1.3) +
  # 3. Add the large circular points
  geom_point(size = 5, color = "grey50") +
  # 4. Add the horizontal dashed reference line at y = 1.0
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "grey30") +
  labs(
    x = NULL, # The second image removes the x-axis title
    y = NULL  # The second image removes the y-axis title
  ) +
  # 5. Define discrete-like labels for the continuous x-axis
  scale_x_continuous(
    breaks = 1:6, 
    labels = c("1st", "2nd", "3rd", "4th", "5th", "6th"),
    limits = c(0.8, 6.2), # Add a little padding on the sides
    expand = c(0, 0)
  ) +
  # 6. Set y-axis limits and breaks
  scale_y_continuous(
    breaks = seq(0.6, 1.1, by = 0.1),
    limits = c(0.55, 1.1),
    expand = c(0, 0)
  ) +
  # 7. Apply a clean theme and remove the grid
  theme_classic() +
  theme(
    axis.line = element_line(size = 0.8),
    axis.ticks = element_line(size = 0.8),
    axis.text = element_text(color = "black", size = 12),
    plot.title = element_blank() # Removing title to match the goal image
  )


##########################################################
## Slope of introspective against distorted reliability ##
##########################################################


# Participant-level posterior means

a_mean <- apply(post$alpha, 2, mean)
b_mean <- apply(post$beta, 2, mean)



# Distortion function

phi <- function(p, a, b) {
  gamma = exp(b)
  return((gamma * p^a) / (gamma * p^a + (1 - p)^a))
}

distorted_50 = c()
distorted_55 = c()
distorted_65 = c()

# Distorted probabilities of participants
for (i in c(1:length(a_mean))){
  a_i = a_mean[i]
  b_i = b_mean[i]
  distorted_50 = c(distorted_50, phi(0.5, a_i, b_i))
  distorted_55 = c(distorted_55, phi(0.55, a_i, b_i))
  distorted_65 = c(distorted_65, phi(0.65, a_i, b_i))
}

# distorted_50_Exp6 = distorted_50
# distorted_55_Exp6 = distorted_55
# distorted_65_Exp6 = distorted_65

# distorted_50_Alex = distorted_50
# distorted_55_Alex = distorted_55
# distorted_65_Alex = distorted_65

distorted_50_Divyaj = distorted_50
distorted_55_Divyaj = distorted_55
distorted_65_Divyaj = distorted_65

##### stats of distortion

list_50 <- list(Control = distorted_50_Exp6, Mitigation1 = distorted_50_Alex, Mitigation2 = distorted_50_Divyaj)
list_50_long <- stack(list_50)


list_55 <- list(Control = distorted_55_Exp6, Mitigation1 = distorted_55_Alex, Mitigation2 = distorted_55_Divyaj)
list_55_long <- stack(list_55)


list_65 <- list(Control = distorted_65_Exp6, Mitigation1 = distorted_65_Alex, Mitigation2 = distorted_65_Divyaj)
list_65_long <- stack(list_65)

res_anova <- aov(values ~ ind, data = list_50_long)
summary(res_anova)
TukeyHSD(res_anova)

res_anova <- aov(values ~ ind, data = list_55_long)
summary(res_anova)
TukeyHSD(res_anova)

res_anova <- aov(values ~ ind, data = list_65_long)
summary(res_anova)
TukeyHSD(res_anova)







# get the participants' slopes

load('./results/data_preprocessed.rdata')

data_processed$slider_reliability = as.factor(data_processed$slider_reliability)

data_chosen = data_processed[which(data_processed$Done.by==chosen_exp),]

data_chosen$Net.Cong.50 = data_chosen$Net.Blue.50
data_chosen$Net.Cong.55 = data_chosen$Net.Blue.55
data_chosen$Net.Cong.65 = data_chosen$Net.Blue.65

data_chosen$Net.Cong.50[which(data_chosen$Response==0)] = 0 - data_chosen$Net.Cong.50[which(data_chosen$Response==0)]
data_chosen$Net.Cong.55[which(data_chosen$Response==0)] = 0 - data_chosen$Net.Cong.55[which(data_chosen$Response==0)]
data_chosen$Net.Cong.65[which(data_chosen$Response==0)] = 0 - data_chosen$Net.Cong.65[which(data_chosen$Response==0)]

data_chosen$slider_response = as.numeric(data_chosen$slider_response)-50


slope_50 = c()
slope_55 = c()
slope_65 = c()


for (id_ in unique(data_chosen$Participant.Private.ID)){
  data_participant = data_chosen[which(data_chosen$Participant.Private.ID == id_),]
  lm.slider = lm(slider_response ~  slider_reliability * Net.Cong.50 + slider_reliability * Net.Cong.55 + slider_reliability * Net.Cong.65, data = data_participant)
  slope_50 = c(slope_50, lm.slider$coefficients[4])
  slope_55 = c(slope_55, lm.slider$coefficients[5]+lm.slider$coefficients[9])
  slope_65 = c(slope_65, lm.slider$coefficients[6]+lm.slider$coefficients[12])
}

df50 <- data.frame(
  distorted_reliability = distorted_50*100,
  slope_ = slope_50,
  reliability = '50'
)

df55 <- data.frame(
  distorted_reliability = distorted_55*100,
  slope_ = slope_55,
  reliability = '55'
)

df65 <- data.frame(
  distorted_reliability = distorted_65*100,
  slope_ = slope_65,
  reliability = '65'
)

df_all = rbind(df50, df55, df65)

my_colors = c('#0099FF', '#00CC33', '#009933')

ggplot(df_all, aes(distorted_reliability, slope_, col = reliability))+
  geom_point() +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(limits = c(-10, 20)) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title = chosen_exp,
       x = "Distorted Reliability",
       y = "Slope of introspective reports",
       color = "Reliability") +
  theme_bw()


library(patchwork)
#################  50

fit <- lm(slope_ ~ I(distorted_reliability^2), data = df50)

grid <- data.frame(distorted_reliability = seq(min(df50$distorted_reliability), max(df50$distorted_reliability), length.out = 20))
preds <- predict(fit, newdata = grid, interval = "confidence")
plot_data <- cbind(grid, preds)


plot.50 = ggplot(df50, aes(distorted_reliability, slope_)) +
  geom_point(shape = 21, fill = "#0099FF", size = 1) +
  # The mean prediction line
  geom_line(data = plot_data, aes(y = fit), color = "red", size = 1) +
  # The dashed confidence interval lines
  geom_line(data = plot_data, aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = plot_data, aes(y = upr), color = "red", linetype = "dashed") +
  # Reference lines and styling
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(
    limits = c(-10, 20),
    expand = c(0.005, 0.005)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = chosen_exp,
       x = "Distorted reliability of 50% reliable sources",
       y = "Slope of introspective reports")
  labs(title = chosen_exp, 
       x = " ", 
       y = " ")

###############  55

fit <- lm(slope_ ~ I(distorted_reliability^2), data = df55)

grid <- data.frame(distorted_reliability = seq(min(df55$distorted_reliability), max(df55$distorted_reliability), length.out = 20))
preds <- predict(fit, newdata = grid, interval = "confidence")
plot_data <- cbind(grid, preds)

plot.55 = ggplot(df55, aes(distorted_reliability, slope_)) +
  geom_point(shape = 21, fill = "#00CC33", size = 1) +
  # The mean prediction line
  geom_line(data = plot_data, aes(y = fit), color = "red", size = 1) +
  # The dashed confidence interval lines
  geom_line(data = plot_data, aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = plot_data, aes(y = upr), color = "red", linetype = "dashed") +
  # Reference lines and styling
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(
    limits = c(-10, 20),
    expand = c(0.005, 0.005)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = chosen_exp,
       x = "Distorted reliability of 55% reliable sources",
       y = "Slope of introspective reports")
  labs(title = chosen_exp, 
       x = " ", 
       y = " ")


###############  65

fit <- lm(slope_ ~ I(distorted_reliability^2), data = df65)

grid <- data.frame(distorted_reliability = seq(min(df65$distorted_reliability), max(df65$distorted_reliability), length.out = 20))
preds <- predict(fit, newdata = grid, interval = "confidence")
plot_data <- cbind(grid, preds)

plot.65 = ggplot(df65, aes(distorted_reliability, slope_)) +
  geom_point(shape = 21, fill = "#009933", size = 1) +
  # The mean prediction line
  geom_line(data = plot_data, aes(y = fit), color = "red", size = 1) +
  # The dashed confidence interval lines
  geom_line(data = plot_data, aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(data = plot_data, aes(y = upr), color = "red", linetype = "dashed") +
  # Reference lines and styling
  scale_x_continuous(
    limits = c(0, 100),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(
    limits = c(-10, 20),
    expand = c(0.005, 0.005)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  theme_classic() +
  labs(title = chosen_exp,
       x = "Distorted reliability of 55% reliable sources",
       y = "Slope of introspective reports")
  labs(title = chosen_exp, 
       x = " ", 
       y = " ")

plot.50 + plot.55 + plot.65


