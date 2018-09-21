

library(pwr)
library(ggplot2)

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

n_sim = 1000
n_per_month_per_arm = 40
time_point = 1
target_time_point = 12
prop_ctrl = 0.1
prop_trt = 0.05


  





l1 <- sim_forecast()

par(mfrow=c(2,1))
plot(density(l1$post_ctrl - l1$post_trt),
     main = paste("Posterior at interim, mnth", time_point), xlim=c(-0.1, 0.2))
abline(v = 0.05, col = "red")
plot(density(l1$post_p_ctrl - l1$post_p_trt),
     main = paste("Post Predictive at completion, mnth", target_time_point), xlim=c(-0.1, 0.2))
abline(v = 0.05, col = "red")
par(mfrow=c(1,1))




