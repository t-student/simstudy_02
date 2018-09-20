

library(pwr)


# Analtical n for 80 power
pwr.2p.test(h = ES.h(p1 = 0.10, p2 = 0.05), sig.level = 0.05, power = .80)

# Simulation for power at given n
fixed_compare_prop <- function(n_sim, n_per_arm, prop_ctrl, prop_trt){
  
  y_ctrl <- rbinom(n_sim, n_per_arm, prop_ctrl)
  y_trt <- rbinom(n_sim, n_per_arm, prop_trt)
  
  p_hat_ctrl <- y_ctrl / n_per_arm
  p_hat_trt <- y_trt / n_per_arm
  
  sd <- sqrt((p_hat_ctrl * (1-p_hat_ctrl)/n_per_arm) +
               (p_hat_trt * (1-p_hat_trt)/n_per_arm))
  
  confint <- p_hat_trt - p_hat_ctrl  - 
    qnorm(0.025) * sd
  
  return(mean(confint < 0))
}
n_sim <- 10
n_per_arm <- 425
prop_ctrl <- 0.10
prop_trt <- 0.05
fixed_compare_prop(n_sim, n_per_arm, prop_ctrl, prop_trt)





# What do things look like at the 3rd month?

n_per_month_per_arm <- 40
time_point <- 3
target_time_point <- 12
  
# beta binomial
n_per_arm <- time_point * n_per_month_per_arm 

# For now just fix the number of success based on the prob of success
# y_ctrl <- rbinom(n_sim, n_per_arm, ctrl_rate)
# y_trt <- rbinom(n_sim, n_per_arm, trt_rate)

y_ctrl <- n_per_arm * prop_ctrl
y_trt <- n_per_arm * prop_trt


# Sampling random variables from the posterior distributions of prop_ctrl and prop_trt
# Assumes a beta(1,1) (uniform) prior
post_ctrl <- rbeta(n_sim, 1 + y_ctrl, 1 + n_per_arm - y_ctrl)
post_trt <- rbeta(n_sim, 1 + y_trt, 1 + n_per_arm - y_trt)

#Calulating posterior samples from pi1 - pi2
posterior <- post_ctrl - post_trt


sample(1:length(posterior), size = 1, prob = posterior)


time_point <- 12

n_per_arm <- time_point * n_per_month_per_arm 

y_ctrl <- n_per_arm * post_ctrl
y_trt <- n_per_arm * post_trt














