install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")





res_grid <- 1000
n_sim = 1000
n_per_month_per_arm = 40
time_point = 3
target_time_point = 12
prop_ctl = 0.1
prop_trt = 0.05


# Parameter grid
p_grid <- seq(0, 1, length.out = res_grid)
prior <- rep(1, res_grid)

# Observed events
n_per_arm <- time_point * n_per_month_per_arm 
y_ctl <- n_per_arm * prop_ctl
y_trt <- n_per_arm * prop_trt

# Likelihood
lik_ctl <- dbinom(y_ctl, n_per_arm, prob = p_grid)
lik_trt <- dbinom(y_trt, n_per_arm, prob = p_grid)

# Posterior - normally we do not have this luxury
post_p_ctl <- prior * lik_ctl
post_p_trt <- prior * lik_trt

# Make posterior integrate to 1
post_p_ctl <- post_p_ctl / sum(post_p_ctl)
post_p_trt <- post_p_trt / sum(post_p_trt)

# par(mfrow = c(2,1))
# plot(p_grid, post_p_ctl)
# plot(p_grid, post_p_trt)
# par(mfrow = c(1,1))

# Posteriors can also be obtained from sampling from grid
samp_post_p_ctl <- sample(p_grid, prob = post_p_ctl, size = 1e4, replace = T)
samp_post_p_trt <- sample(p_grid, prob = post_p_trt, size = 1e4, replace = T)

rethinking::dens(samp_post_p_ctl)
rethinking::dens(samp_post_p_trt)
rethinking::dens(samp_post_p_ctl - samp_post_p_trt)

# Simulate what the trial looks like at the end
n_per_arm <- target_time_point * n_per_month_per_arm 

y_rep_ctl <- rbinom(1e4, size = n_per_arm, prob = samp_post_p_ctl)
y_rep_trt <- rbinom(1e4, size = n_per_arm, prob = samp_post_p_trt)

pp_end_ctl <- y_rep_ctl / n_per_arm
pp_end_trt <- y_rep_trt / n_per_arm

rethinking::dens(pp_end_ctl)
rethinking::dens(pp_end_trt)
rethinking::dens(pp_end_ctl - pp_end_trt)




