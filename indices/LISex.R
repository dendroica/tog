# None of these look super correlated

# Check the variance inflation factor for a more statistical check.
mod = lm(Tautog ~ Year + Month + Station + SurfaceTemp+Salinity+DO, data = dat)
vif(mod)

# You want GVIF to be less than ~3. Year, Month, and Surface temp are > 3 
# Drop one (whichever you think is less informative) and test again. 
# will drop month
mod = lm(Tautog ~ Year + SurfaceTemp+Salinity+DO, data = dat)
vif(mod)

#mod = lm(Tautog ~ Station +  SurfaceTemp+Salinity+DO, data = dat)
#vif(mod)

# removed month and all are <3 now

#=========================#
##### Model Selection ####
#=========================#

# We're modeling count data, so we want to use a model that expects
# that, like a Poisson or a negative binomial. Survey data generally
# don't follow a Poisson distribution: zeros are overrepresented in the
# data and the variance is much greater than the mean. 

mean(dat$PosTow)

# This dataset has 11.67086% positive sets which is below
# of what is considered zero-inflated (~60% zero records).
# So we'll start with a negative binomial and then look
# at some zero-inflated options.
#### Negative Binomial ####
NB2 <- glmmTMB(Tautog ~ Year + SurfaceTemp+Salinity+DO, #when just year and surface temp, super high STD ERROR
               data = dat,
               family = nbinom2)

# Check the Std. Error of the estimates; high SEs (1-2 orders of magnitude
# or more greater than the estimate indicate problems with the fit.
summary(NB2)

# Next we'll check the dispersion
disp3 <- function(mod = NA) {
  E <- residuals(mod, type = "pearson")
  d <- sum(E^2) / summary(mod)$AICtab[5]
  return(d)
}

disp3(NB2)

# You want dispersion to be ~1. This model is at 1.014251  
# Next We'll still look at some zero-inflated models to cover our bases.

#========================================================================#
#### Zero-inflated models ####
#========================================================================#

#========================================================================#
# Zero-inflated models actually fit two models: the probability of
# catching a fish (a binomial model based whether the count is zero
# or greater than zero) and the catch rate of fish (if you caught fish,
# how many did you catch?). You can use the same factors in both parts
# of the model, or different ones for each component.
#========================================================================#

#### Zero-inflated negative binomial ####
# We'll start with the same factors in each half of the model.

ZINB <- glmmTMB(Tautog ~ Year + SurfaceTemp+Salinity+DO ,
                ziformula = ~.,
                data = dat,
                family = nbinom2)

# Model convergence problem!
summary(ZINB)

#dropping year from zi formula
ZINB.2 <- glmmTMB(Tautog ~ Year + SurfaceTemp ,
                  ziformula = ~ SurfaceTemp+Salinity+DO,
                  data = dat,
                  family = nbinom2)

summary(ZINB.2)

# We need a slightly different function to get the residuals to 
# calculate dispersion from a zero-inflated model

TMB.ZI.resids = function(mod,y.dat)
{
  v = family(mod)$variance
  p = predict(mod, type = 'zprob') #zi probability
  mu = predict(mod, type='conditional') #mean of the conditional distribution
  pred = predict(mod, type='response') #(1-p)*mu
  k = sigma(mod) #sigma gives the overdispersion parameter = theta = k
  pvar = (1-p)*v(mu,k)+mu^2*(p^2+p)
  pearson.resid = (y.dat - pred)/sqrt(pvar)
  return(pearson.resid)
}

E = TMB.ZI.resids(ZINB.2, y.dat=dat$Tautog)

#now check overdispersion
(d = sum(E^2)/summary(ZINB.2)$AICtab[5])

# dispersion = 0.9508786  
# no improvement on the dispersion.


#### Zero-altered negative binomial ####
# AKA a hurdle model

#ZANB <- glmmTMB(Tautog ~ Station + SurfaceTemp+Salinity+DO,
#                ziformula = ~.,
#                data = dat,
#                family = truncated_nbinom2(link = "log"))
#summary(ZANB)

#convergence problems, will remove year
# ZANB.2 <- glmmTMB(Tautog ~  SurfaceTemp,
#                 ziformula = ~ Year +  SurfaceTemp,
#                 data = dat,
#                 family = truncated_nbinom2(link = "log"))
# summary(ZANB.2)
# 

# Check overdispersion
#E = TMB.ZI.resids(ZANB, y.dat=dat$Tautog)
#(d = sum(E^2)/summary(ZANB)$AICtab[5])

#dispersion: 0.9661554 


# Use AIC to compare across models
AICtab(NB2, ZINB.2)

# NB2 is best but ZINB.2 is close second


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Fixed effects for fixed stations ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For fixed station surveys, you can treat station as a fixed effect.

# ZINB.2.re <- glmmTMB(Tautog ~ Year + (1|Station) +  SurfaceTemp,
#                      ziformula = ~Station +  SurfaceTemp,
#                      data = dat,
#                      family = nbinom2)
# 
# AICtab(ZINB.2.re, ZINB.2)

#not an improvement from zero inflated model

#===================================================================#
#### General additive models (GAMs) ####
# Sometimes the relationship between environmental predictors
# and CPUE is not linear -- e.g., there is a range of preferred
# temps for a species where CPUE peaks
# GAMs can capture this relationship
#===================================================================#

GAM.NB <- gam(Tautog~ Year + s(SurfaceTemp)+s(Salinity)+s(DO),
              data = dat, family = 'nb')
summary(GAM.NB) #has big errors because can't do zero inflated
AICtab(GAM.NB, NB2, ZINB.2)

#NB.2 still prefered model, but GAM and ZINB are close 2nd and 3rd



#==============================================#
#### Use the DHARMa package to examine fit ####
#==============================================#

# See the vignette at https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# for examples of what good and bad diagnostics look like with this package

sim.NB2 <- simulateResiduals(NB2)
sim.ZINB <- simulateResiduals(ZINB.2)
#sim.ZANB <- simulateResiduals(ZANB)
sim.GAM <- simulateResiduals(GAM.NB)

plot(sim.NB2, quantreg=T) 
plot(sim.ZINB, quantreg=T)
#plot(sim.ZANB, quantreg=T) #didn't work/shows bad stuff
plot(sim.GAM, quantreg=T) 
# Compare residuals vs. factors NBI
vars <- names(NB2$frame)
vars <- vars[vars!="Tautog"]
for(v in vars){ #not working
  plotResiduals(sim.NB2, form=dat[,v], sub=v, quantreg=T)
}

### this below code is not working?



# Compare residuals vs. factors ZINB
vars <- names(ZINB$frame)
vars <- vars[vars!="Tautog"]
for(v in vars){ #not working
  plotResiduals(sim.ZINB, form=dat[,v], sub=v, quantreg=T)
}

# Compare residuals vs. factors
#vars <- names(ZANB$frame)
#vars <- vars[vars!="Tautog" ]
#for(v in vars){
#  plotResiduals(sim.ZANB, form=dat[,v], sub=v, quantreg=T)
#}

# Note that gam() output doesn't have "frame" but it does have "model"
vars <- names(GAM.NB$model)
vars <- vars[vars!="Tautog"]
for(v in vars){
  plotResiduals(sim.GAM, form=dat[,v], sub=v, quantreg=T)
}

#NB2$call
GAM.NB$call

##end not working


#==============================================#
#### Select factors for inclusion in model ####
#==============================================#

# AIC and the DHARMa diagnostics indicate the zero-inflated
# negative binomial is the better choice compared to the NB and ZANB

# Explore what factors to include in the model
# There are a number of different ways to do this. Here we will use both
# AIC and the summary statistics to decide. We need YEAR in the model
# no matter what, so we will always keep that.


# summary(ZINB.2)
# 
# #try to drop surfact temp or station
# #first station
# ZINB.S <- glmmTMB(Tautog ~ Year  +  SurfaceTemp ,
#                   ziformula = ~ SurfaceTemp ,
#                   data = dat,
#                   family = nbinom2)
# summary(ZINB.S)
# 
# # try removing surface temp
# ZINB.T <- glmmTMB(Tautog ~ Year + Station  ,
#                   ziformula = ~ Station ,
#                   data = dat,
#                   family = nbinom2)
# summary(ZINB.T)
# 
# #dropping year from zi formula
# ZINB.M <- glmmTMB(Tautog ~ Year + Month + Station +  SurfaceTemp ,
#                   ziformula = ~ Month+ Station +  SurfaceTemp ,
#                   data = dat,
#                   family = nbinom2)
# 
# 
# AICtab(ZINB.2, ZINB.M,ZINB.S,ZINB.T)
# 
# #month is an improvement over ZINB.2
# #check the residuals
# sim.ZINB.M <- simulateResiduals(ZINB.M)
# 
# plot(sim.ZINB, quantreg=T)
# plot(sim.ZINB.M, quantreg=T)
# #residuals are good for model with month 
# # but bc of VIF we'll stick with month left out of the model
# 
# 
# summary(ZINB.2)
# #all parameters are significant other than some years and station
# # can't drop years
# # and don't want to drop any more stations
# # so will leave as final model
# 
