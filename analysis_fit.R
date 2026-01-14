library(readr)
library(ggplot2)

chosen_exp = 'Alex'
# chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'



fit_log_seq = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)

fit_log_basic = read_rds(paste('./results/',chosen_exp,'/log_basic.rds', sep = ''), refhook = NULL)

#### extract the fit

########

# Extract posterior samples
if(chosen_exp =='Divyaj'){
  post <- rstan::extract(fit_log_basic)
}else{
  post <- rstan::extract(fit_log_seq)
}

# Participant-level posterior means

a_mean <- apply(post$alpha, 2, mean)
b_mean <- apply(post$beta, 2, mean)
if (chosen_exp != 'Divyaj'){
  w_mean <- apply(post$w, c(2,3), mean)
}



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

ggplot(df, aes(x = p, y = distorted)) +
  geom_line(size = 1.3, color = "#0072B2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = chosen_exp,
    x = "Objective reliability (%)",
    y = "Subjective reliability (%)"
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal(base_size = 15)

##### plot w
df <- data.frame(
  no.seq = c(1:6),
  weights = w_g
)

ggplot(df, aes(x = no.seq, y = weights)) + 
  geom_line(size = 1.3) +
  labs(
    title = chosen_exp,
    x = "Order in the sequence",
    y = "Sequential weights"
  ) +
  scale_x_continuous(limits = c(1, 6)) +
  scale_y_continuous(limits = c(0.5, 1.1))


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

ggplot(df50, aes(distorted_reliability, slope_))+
  geom_point() +
  scale_y_continuous(limits = c(-10, 15)) +
  scale_x_continuous(limits = c(0, 100))

ggplot(df55, aes(distorted_reliability, slope_))+
  geom_point() +
  scale_y_continuous(limits = c(-10, 15)) +
  scale_x_continuous(limits = c(0, 100))

ggplot(df65, aes(distorted_reliability, slope_))+
  geom_point() +
  scale_y_continuous(limits = c(-10, 15)) +
  scale_x_continuous(limits = c(0, 100))


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
  
