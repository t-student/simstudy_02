

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

sim_forecast <- function(n_sim = 1000,
                         n_per_month_per_arm = 40,
                         time_point = 3,
                         target_time_point = 12,
                         prop_ctrl = 0.1,
                         prop_trt = 0.05){
  
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
  
  # Calulating posterior for the difference between proportions
  # at the currect timepoint from pi1 - pi2
  post_diff <- post_ctrl - post_trt
  
  # Now you want to know what the number of successes is at the end of the 
  # study for each arm. This is a posterior predictive - you need to incorporate 
  # the uncertainty (posterior) in the ctrl and trt arms.
  n_per_arm <- target_time_point * n_per_month_per_arm 
  y_rep_ctrl <- rbinom(n_sim, size = n_per_arm, prob = post_ctrl)
  y_rep_trt <- rbinom(n_sim, size = n_per_arm, prob = post_trt)

  
  # Posterior predictives at end of study. This incorporates the uncertainty
  # in the estimate of the number of successes at the end of the trial.
  post_p_ctrl <- numeric()
  for(i in 1:length(y_rep_ctrl)){
    post_p_ctrl <- c(post_p_ctrl, 
                     rbeta(n_sim, 1 + y_rep_ctrl[i], 
                           1 + n_per_arm - y_rep_ctrl[i]))
  }

  post_p_trt <- numeric()
  for(i in 1:length(y_rep_trt)){
    post_p_trt <- c(post_p_trt, 
                    rbeta(n_sim, 1 + y_rep_trt[i], 
                          1 + n_per_arm - y_rep_trt[i]))
  }
  
  lret <- list(post_ctrl = post_ctrl, 
               post_trt = post_trt,
               post_p_ctrl = post_p_ctrl,
               post_p_trt = post_p_trt)
  
  return(lret)
  
}

  





l1 <- sim_forecast()

par(mfrow=c(2,1))
plot(density(l1$post_ctrl - l1$post_trt),
     main = paste("Posterior at interim, mnth", time_point), xlim=c(-0.1, 0.2))
abline(v = 0.05, col = "red")
plot(density(l1$post_p_ctrl - l1$post_p_trt),
     main = paste("Post Predictive at completion, mnth", target_time_point), xlim=c(-0.1, 0.2))
abline(v = 0.05, col = "red")
par(mfrow=c(1,1))



