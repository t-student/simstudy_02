

library(rjags)
library(coda)

data = read.csv("data1.csv")
head(data)
N <- length(data$y) # There are 7 total studies. 


##### now prepare dat for JAGS
## N is the number of entries (e.g., 7)
## y is the outcome in the data (e.g., ln(OR))
## V is the variance in the data
dat <- list("N" = N, "y" = data$y, "V" = data$V)  # names list of numbers

##### Initial values
inits <- list( d = 0.0 )

cat("model
    {
    
    for ( i in 1:N ) {
    
    P[i] <- 1/V[i]
    
    y[i] ~ dnorm( d, P[i] )
    
    }
    
    ### Define the priors
    d ~ dnorm( 0, 0.00001 )
    
    ### Transform the ln(OR) to OR
    OR <- exp( d )
    
    }", file="aspirinFE.txt")


jags.m <- jags.model( file = "aspirinFE.txt", 
                      data=dat, inits=inits, 
                      n.chains=1, n.adapt=500 )

## specify parameters to be monitored
params <- c("d", "OR")

## run JAGS and save posterior samples
samps <- coda.samples( jags.m, params, n.iter=10000 )

## summarize posterior samples
summary(samps)

summary(window(samps, start=5001))  # Burn in of 5000. Start at 5001

plot(samps)

















