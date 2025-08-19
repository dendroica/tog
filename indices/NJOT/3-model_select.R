library(buildmer)
library(glmmTMB)
#cleanup to put data source in
root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)
load(file.path(root, "output/tog/index/NJOTmod2.RData"))
source("./indices/bootstrap_functions.R")
#library(mgcv)
bmc <- buildmerControl(include= ~offset(lnEffort) + YEAR)
#mod <- as.formula("CPUE ~ YEAR + STRATA + DEPTH + BTEMP + BSAL + BDO")
nb <- buildglmmTMB(mod, dat, family = nbinom2, buildmerControl = bmc)
NB00 <- glmmTMB(formula(nb), offset=lnEffort, data = dat, family = nbinom2)
#out <- boot.NB(NB00) #best yet...94.4%!
#...though from a recent effort not including cruise 6 I think I got even higher
#...and with BTEMP included? and BSAL not in the model for consideration
#better selected via AIC

#terms in ZI formula informed by best selected model above...
#...but playing with this changes which model is selected
#I don't think I understand the ZI formula specification well enough
#this way, the 2 best selected models come out the same...
#...so more comparing the model families here
bmc2 <- buildmerControl(include= ~offset(lnEffort) + YEAR, args=list(ziformula = ~ STRATA))
zinb <- buildglmmTMB(mod, dat, family = nbinom2, 
                   buildmerControl = bmc2)
ZINB00 <- glmmTMB(formula(zinb), offset=lnEffort, data = dat, 
                  ziformula = ~STRATA,
                  family = nbinom2)
#Zout00 <- boot.NB(ZINB00) #best yet...95.1% of bootstrap runs converged

#CONVERGENCE PROBLEM
#zanb <- buildglmmTMB(mod, 
#                data = dat,
#                buildmerControl = bmc2,
#                family = truncated_nbinom2(link = "log"))
ZANB <- glmmTMB(CPUE ~ 1 + STRATA + YEAR + BTEMP, 
                ziformula = ~ BTEMP, data = dat,
                offset=lnEffort,family = truncated_nbinom2(link = "log"))
summary(ZANB)
#out7 <- boot.ZI(ZANB)

#ZANB00 <- glmmTMB(CPUE ~ offset(lnEffort) + 1 + STRATA + BDO + YEAR, 
#                ziformula = ~ STRATA + DEPTH + BDO + BTEMP, 
#                data = dat, #YEAR + 
#offset=lnEffort,
#               family = truncated_nbinom2(link = "log"))

ZANB1 <- glmmTMB(mod, 
                                ziformula = ~ BTEMP, data = dat,
                                offset=lnEffort,family = truncated_nbinom2(link = "log"))

#When looking at AIC and bootstrapping, GAM is best
#however is it overfitting? is this really important?
#my reading led me to that GAM might be overkill and "beside the point" for this purpose
#namely, all I really want here is the index, not to really dig into the nature of the response to the predictors
#bmc2 <- buildmerControl(include= ~offset(lnEffort) + YEAR) #, crit='F'
#bestgam <- buildgamm4(CPUE~ YEAR + s(STRATA, bs="re") + s(DEPTH) + s(BDO)+s(BTEMP)+s(BSAL),
#         data = dat, family = nb(), buildmerControl = bmc2)
#GAM.NB <- gam(formula(bestgam),
#              data = dat, family = 'nb', method="ML")
#bootgam <- boot.GAM(GAM.NB, nboots=1000)
AICtab(NB00, ZINB00, ZANB, ZANB1) #, GAM.NB

#nb2.coefs <- data.frame(estimate = c(coef(summary(NB00))$cond[, "Estimate"], coef(GAM.NB)),
#                        model    =  c(rep("glmmTMB", 38), rep("mgcv::gam", 57)),
#                        term     = c(names(coef(summary(NB00))$cond[, "Estimate"]), names(coef(GAM.NB)))
#)
#save(dat, NB00, out, file="NJOTmodel.RData")