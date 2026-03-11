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

#bmc3 <- buildmerControl(include= ~ Year, args=list(ziformula = ~ SurfaceTemp))
#zanb <- buildglmmTMB(mod,
#                data = dat,
#                family = truncated_nbinom2(link = "log"), buildmerControl = bmc3)

#ZANB <- glmmTMB(Tautog ~ Station + SurfaceTemp+Salinity+DO,
#                ziformula = ~.,
#                data = dat,
#                family = truncated_nbinom2(link = "log"))

bmc4 <- buildmerControl(include= ~ Year) 
#gam <- buildgamm4(mod, data = dat, family = nbinom2, buildmerControl = bmc4) doesn't work

GAM.NB <- gam(Tautog~ Year + s(SurfaceTemp)+s(Salinity), #+s(DO),
              data = dat, family = 'nb') #still wins
AICtab(NB, ZINB, GAM.NB)

# We will bootstrap the index to get the CVs and confidence intervals.
# Use boot.NB() for non-zero-inflated models and boot.GAM() for
# GAMs. boot.ZI(), boot.NB(), and boot.GAM() are custom functions included in the
# "bootstrap_functions.r" file. If you have a low percent (<50%) of converged
# runs, it's a sign that the model may not be robust.
SE2 <- boot.GAM(GAM.NB, nboots = 1000) #100% converged
#Warning messages:
#  1: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#     Fitting terminated with step failure - check results carefully
#  2: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#     Fitting terminated with step failure - check results carefully
#  3: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#     Fitting terminated with step failure - check results carefully

index.out <- data.frame(
  Year = as.numeric(unique(dat$Year)),
  # Station=as.character(unique(dat$Station)), #use unique rather than levels bc removed 3 years
  Index = predict(GAM.NB, newdata = p.data, type = "response")
)

index.out <- cbind.data.frame(index.out, SE2)
index.out$Year <- as.integer(levels(p.data$Year))

allyrs <- 1989:2024
missing <- allyrs[!allyrs %in% p.data$Year]
index.out <- index.out[index.out$Year > 1987, ]
index.out <- rbind(index.out, c(1997, -1, -1, -1, -1), c(2010, -1, -1, -1, -1))
index.out <- index.out[order(index.out$Year), ]
index.out$CV <- index.out$SE / index.out$Index
index.out$scale <- index.out$Index / mean(index.out$Index)
index.out$Year <- index.out$Year + 1 #this survey needs to be lagged by a year

ggplot(index.out) +
  geom_ribbon(aes(x = Year, ymin = LCI, ymax = UCI), alpha = 0.4) +
  geom_point(aes(x = Year, y = Index), shape = 16) +
  geom_line(aes(x = Year, y = Index)) +
  ylim(c(0, NA)) +
  theme_bw()