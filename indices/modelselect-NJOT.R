#cleanup to put data source in

library(buildmer)
bmc <- buildmerControl(include= ~offset(lnEffort))
nb <- buildglmmTMB(mod, dat, family = nbinom2, 
                   buildmerControl = bmc)

NB00 <- glmmTMB(CPUE ~ 1 + STRATA + YEAR + BTEMP, offset=lnEffort, data = dat, family = nbinom2)
out00 <- boot.NB(NB00) #best yet...

bmc2 <- buildmerControl(include= ~offset(lnEffort) + YEAR, args=list(ziformula = ~ STRATA + BTEMP + BDO))
zinb <- buildglmmTMB(mod, dat, family = nbinom2, 
                   buildmerControl = bmc2)
ZINB00 <- glmmTMB(CPUE ~ 1 + YEAR + STRATA + BDO + BTEMP, offset=lnEffort, data = dat, 
                  ziformula = ~STRATA + BTEMP + BDO,
                  family = nbinom2)
Zout00 <- boot.NB(ZINB00) #best yet...

bmc3 <- buildmerControl(include= ~offset(lnEffort) + YEAR, args=list(ziformula = ~ STRATA))

GAM.NB <- gam(CPUE~ YEAR + STRATA + DEPTH + s(BDO)+s(BTEMP),
              data = dat, family = 'nb') #still wins

AICtab(GAM.NB, NB00, ZINB00)
#zanb <- buildglmmTMB(mod, 
#                data = dat,
#                buildmerControl = bmc3, #offset=lnEffort,
#                family = truncated_nbinom2(link = "log"))

#ZANB <- glmmTMB(mod, 
#                ziformula = ~ BTEMP, data = dat,
#                offset=lnEffort,family = truncated_nbinom2(link = "log"))
#summary(ZANB) #convergence problem

#ZANB <- glmmTMB(CPUE ~ 1 + STRATA + YEAR + BTEMP, 
#                ziformula = ~ BTEMP, data = dat,
#                offset=lnEffort,family = truncated_nbinom2(link = "log"))
#                summary(ZANB) #convergence problem

#ZANB00 <- glmmTMB(CPUE ~ offset(lnEffort) + 1 + STRATA + BDO + YEAR, 
#                ziformula = ~ STRATA + DEPTH + BDO + BTEMP, 
#                data = dat, #YEAR + 
                #offset=lnEffort,
#                family = truncated_nbinom2(link = "log"))
#summary(ZANB00) # ok
#out7 <- boot.ZI(ZANB00)
