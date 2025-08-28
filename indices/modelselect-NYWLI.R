#clean up to include input data here

library(buildmer)
mod <- as.formula("Tautog ~ Year + SurfaceTemp + Salinity + DO")
bmc <- buildmerControl(include= ~ Year)
nb <- buildglmmTMB(mod, dat, family = nbinom2, 
                   buildmerControl = bmc)
#Formula:          Tautog ~ 1 + Year + SurfaceTemp + Salinity

NB <- glmmTMB(Tautog ~ 1 + Year + SurfaceTemp + Salinity, #when just year and surface temp, super high STD ERROR
               data = dat,
               family = nbinom2)

bmc2 <- buildmerControl(include= ~ Year, args=list(ziformula = ~ SurfaceTemp + Salinity))
zinb <- buildglmmTMB(mod,
                data = dat,
                family = nbinom2, buildmerControl = bmc2)
#Formula: Tautog ~ 1 + Year + SurfaceTemp + Salinity

ZINB <- glmmTMB(Tautog ~ 1 + Year + SurfaceTemp + Salinity ,
                ziformula = ~ SurfaceTemp + Salinity,
                data = dat,
                family = nbinom2)

#ZINB.2 <- glmmTMB(Tautog ~ Year + SurfaceTemp ,
#                  ziformula = ~ SurfaceTemp+Salinity+DO,
#                  data = dat,
#                  family = nbinom2)

#bmc3 <- buildmerControl(include= ~ Year, args=list(ziformula = ~ SurfaceTemp))
#zanb <- buildglmmTMB(mod,
#                data = dat,
#                family = truncated_nbinom2(link = "log"), buildmerControl = bmc3)

#ZANB <- glmmTMB(Tautog ~ Station + SurfaceTemp+Salinity+DO,
#                ziformula = ~.,
#                data = dat,
#                family = truncated_nbinom2(link = "log"))

mod2 <- as.formula("Tautog ~ Year + SurfaceTemp +Salinity + DO")
bmc4 <- buildmerControl(include= ~ Year) 
#, crit='F
#gam <- buildgamm4(mod2, data = dat, family = nbinom2, buildmerControl = bmc4) doesn't work

GAM.NB <- gam(Tautog~ Year + s(SurfaceTemp)+s(Salinity)+s(DO),
              data = dat, family = 'nb') #still wins
AICtab(NB, ZINB)
