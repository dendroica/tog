# Fitting von Bertalanffy curve to simulated data
rm(list=ls()) # Clear Environment

# Choose von Bertalanffy parameter values and other simulation settings
L_inf <- 80  # e.g., 80-cm fish
k <- 0.3 # Slope
t_0 <- -0.5 # Age at length 0
MaxAge <- 12
SystSample <- 5  # Number per age group
N.AgeLen <- SystSample * MaxAge
Age <- rep(1:MaxAge, each=SystSample)
Var_L <- 10 # Variance about age-length relationship
Len <- rnorm(n=N.AgeLen,L_inf*(1-exp(-k*(Age-t_0))), sd=sqrt(Var_L))

plot(Age, Len, xlab="Age", ylab="Length (cm)")
