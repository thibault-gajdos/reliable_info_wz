
# chosen_exp = 'Alex'
chosen_exp = 'Divyaj'
# chosen_exp = 'Exp6'



fit_log_seq = read_rds(paste('./results/',chosen_exp,'/log_seq.rds', sep = ''), refhook = NULL)

fit_log_basic = read_rds(paste('./results/',chosen_exp,'/log_basic.rds', sep = ''), refhook = NULL)




# Extract posterior samples
if(chosen_exp =='Divyaj'){
  post <- rstan::extract(fit_log_basic)
}else{
  post <- rstan::extract(fit_log_seq)
}


# Example group-level parameters
mu_alpha <- post$mu_alpha
mu_beta  <- post$mu_beta
mu_w1    <- post$mu_w1
mu_w2    <- post$mu_w2
mu_w3    <- post$mu_w3
mu_w4    <- post$mu_w4
mu_w5    <- post$mu_w5

histogram_func <- function(param, n_breaks, param_name, xlim_){
  hist(param,
       breaks = n_breaks,
       main = param_name,
       # freq = FALSE,
       col = "skyblue3",
       xlim = xlim_,
       ylab = ''
       )
  abline(v = mean(param), col = 'red', lwd = 2)
  ci <- quantile(param, probs = c(0.005, 0.995))
  abline(v = ci, col = "gray56", lwd = 2, lty = 2)
}

par(mfrow = c(1,7))
histogram_func(mu_alpha, 20, 'a', c(1,9))
histogram_func(mu_beta, 20, 'b', c(0,0.8))
histogram_func(mu_w1, 20, 'w1', c(0.6,1.2))
histogram_func(mu_w2, 20, 'w2', c(0.6,1.2))
histogram_func(mu_w3, 20, 'w3', c(0.6,1.2))
histogram_func(mu_w4, 20, 'w4', c(0.6,1.2))
histogram_func(mu_w5, 20, 'w5', c(0.2,1.2))

