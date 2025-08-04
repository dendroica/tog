# Load necessary library
library(rjags)
library(R2jags)

# JAGS code
sink("GrowthCurve.txt")
cat("
model{

# Priors

 L_inf.est ~ dunif(0, 200)
 k.est ~ dunif(0, 2)
 t0.est ~ dunif(-5, 5)
 Var_L.est ~ dunif(0,100)   # Variability in length at age

# Calculated value
 tau.est <- 1/Var_L.est

# Likelihood
 for (i in 1:N.AgeLen) {
    Len_hat[i] <- L_inf.est*(1-exp(-k.est*(Age[i]-t0.est)))
    Len[i] ~ dnorm(Len_hat[i], tau.est)
 } #i
}
    ",fill=TRUE)
sink()

# Bundle data
jags.data <- list("N.AgeLen", "Age", "Len")

# Initial values
jags.inits <- function(){ list(L_inf.est=runif(n=1, min=0, max=max(Len)),
                               k.est=runif(n=1, min=0, max=2),
                               t0.est=runif(n=1, min=-5, max=5),
                               Var_L.est=runif(n=1, min=0, max=100))}

model.file <- 'GrowthCurve.txt'

# Parameters monitored
jags.params <- c("L_inf.est", "k.est", "t0.est", "Var_L.est")

# Call JAGS from R
jagsfit <- jags(data=jags.data, jags.params, inits=jags.inits,
                n.chains = 3, n.thin=1, n.iter = 10000,
                model.file)
print(jagsfit)
plot(jagsfit)
