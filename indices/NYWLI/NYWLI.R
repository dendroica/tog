# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tautog indices standardization
# 2025 Assessment update
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#----Load packages----
options(scipen = 999)
library(car)
library(tidyverse)
library(lmtest)
library(glmmTMB)
library(DHARMa)
library(bbmle)
library(mgcv)
library(mgcViz)
library(readxl)
library(buildmer)

base_path <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(base_path, usr, loc)

wlidata <- read_excel(file.path(root, "data/tog", "2025_SA_NY_Tautog_Survey_Data_1984-2024.xlsx"), sheet = "WLIS Seine")
# Source helper functions for bootstrapping
source("./indices/bootstrap_functions.r")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# -- Start NY WLI seine survey -----
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Clean and prep data ####
head(wlidata)

## Variables ##
# Year
# Month
# Body of water: where it was sampled
# TOW ID: specific ID based on year
# Station: fixed station ID
# Surface Temp: Surface temperature
# Salinity: Surface salinity
# DO: Surface dissolved oxygen (assuming surface_)
# Tautog: Number of Taugtoh observed in sample
# Region: LIS (long island sound) or NYB (new york bight)
# typo in Body of water Manhasset Bay and Manhassett Bay (one t is corect)
wlidata$`Body of water` <- ifelse(wlidata$`Body of water` == "Manhassett Bay", "Manhasset Bay", wlidata$`Body of water`)
wlidata <- subset(wlidata, Region == "NYB")
summary(wlidata)

# First, create a new data frame with only the variables you're interested
# in exploring. For this data set, we're not interested in day of month or
# sample ID, but we do want everything else. We want year, month, and stratum
# to be factors, but we'll leave the others as continuous variables.

wlidata2 <- wlidata %>%
  mutate(
    Year = as.factor(Year),
    Month = as.factor(Month),
    Station = as.factor(Station)
  ) %>%
  select(!c(Date, `TOW ID`, Region))
head(wlidata2)


# Check to see if you have any years where you have zero
# catch (no positive tows/hauls/sets)
wlidata2$Tautog <- wlidata2$`# Tautog`
xtabs(Tautog ~ Year, wlidata2)

# 1984 and 2010 have 0 catch so want to drop those years
str(wlidata2)
rmyrs <- c("1984", "2010")

wlidata3 <- subset(wlidata2, !Year %in% rmyrs)
# xtabs(Tautog~Year, wlidata3)
# it is working but it replaces the value with a 0 when you use xtabs
unique(wlidata3$Year)
# wlidata3<-wlidata2

# Decide if you need to subset your data to specific strata, months, etc.
# based on when/where you most reliably catch (or expect to catch) your species.
# Check mean catch:
barplot(tapply(wlidata3$Tautog, wlidata3[, "Month"], mean), xlab = "Month", ylab = "Mean CPUE")
barplot(tapply(wlidata3$Tautog, wlidata3[, "Station"], mean), xlab = "Station", ylab = "Mean CPUE")
# don't catch a lot in May or June but will leave for now
# some stations don't have tautog catch: JAM07, JAM11, JAM13, JAM14
# maybe should remove but will continue with all stations for now
unique(wlidata3$Month)
# Check the proportion of positive samples:
# Calculate the proportion of positive tows/sets/hauls
wlidata3$PosTow <- ifelse(wlidata3$Tautog > 0, 1, 0)
barplot(tapply(wlidata3$PosTow, wlidata3[, "Month"], mean), xlab = "Month", ylab = "Proportion Positive")
barplot(tapply(wlidata3$PosTow, wlidata3[, "Station"], mean), xlab = "Station", ylab = "Proportion Positive")

# Check the sample size for different factors:
ggplot(wlidata3) +
  geom_bar(aes(x = Year)) +
  facet_wrap(~Month, ncol = 1) +
  theme_bw()
xtabs(~ Year + Month, wlidata3)
ggplot(wlidata3) +
  geom_bar(aes(x = Year)) +
  facet_wrap(~Station, ncol = 1) +
  theme_bw()
xtabs(~ Year + Station, wlidata3)
# Tautog has highest catch in May and June,
# less so in Sept and Oct but those have high proportion of positive catch
# so will leave all months in for now

# In the end, we are going to calculate the index by predicting CPUE by
# year by holding all other factors/co-variates constant at one level.
# R assigns levels of factors alphabetically by default, but the first level
# may not be exactly what you want. If the first level is not well-sampled
# or if it has a low CPUE, high variance, etc. you might want to re-level
# your factors.
barplot(tapply(wlidata3$Tautog, wlidata3[, "Month"], function(x) sd(x) / mean(x)),
      xlab = "Month", ylab = "CV")
barplot(tapply(wlidata3$Tautog, wlidata3[, "Station"], function(x) sd(x) / mean(x)),
        xlab = "Station", ylab = "CV")

# checking which stations catch tautog and during what years
new <- wlidata3 %>%
  group_by(Year, Station) %>%
  summarise(N = sum(Tautog))
ggplot(data = new, aes(x = Station, y = N)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Year, scales = "free")

barplot(tapply(wlidata3$Tautog, wlidata3$Station, mean), xlab = "Station", ylab = "CPUE")
stations <- unique(wlidata3$Station[wlidata3$Year == 2024])
wlidata3$SurfaceTemp <- wlidata3$`Surface Temp`
wlidata3$`Surface Temp` <- NULL
wlidata4 <- subset(wlidata3, Station %in% stations)
unique(wlidata4$Station)


# checking which stations catch tautog and during what years
new2 <- wlidata4 %>%
  group_by(Year, Station) %>%
  summarise(N = sum(Tautog))
ggplot(data = new2, aes(x = Station, y = N)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~Year, scales = "free")


# Generally speaking, we don't want to work with less than 10%
# positive tows/hauls/sets -- it suggests that this survey may not be doing
# a good job of sampling that species. Now that we've subset down to the best
# months, strata, etc., let's check the annual proportion positive
barplot(tapply(wlidata4$PosTow, wlidata4$Year, mean), ylab = "Proportion positive")
abline(h = 0.1, col = "red")


# We need complete cases to compare across models (e.g., a model with temperature
# compared to a model without temperature), so we'll drop records
# that have missing values in the covariates we care about. But first, make sure
# you have observations of all your covariates in all years; if you didn't
# collect information on temperature in all years, for example, you'll have
# to either drop temperature from the model, or drop those years from the
# index. A quick set of plots can help identify years with missing covariates.
wlidata4$`Body of water` <- NULL
wlidata4$`# Tautog` <- NULL

for (i in names(wlidata4)) {
  y <- wlidata4[, i][[1]]
  plot(as.numeric(as.character(wlidata4$Year)), y, ylab = i, xlab = "Year")
  print(i)
  print(tapply(y, wlidata4$Year, function(x) round((sum(is.na(x)) / length(x)) * 100, 1)))
}

# check for outliers?
plot(wlidata4$Salinity)
plot(wlidata4$SurfaceTemp)
plot(wlidata4$DO)

# salinity and DO don't have values in 1984, DO does not have values in 1986, 1997
# will drop these values from standardization
colnames(wlidata4)

# take complete cases
indata <- wlidata4[complete.cases(wlidata4), ]

# If each tow/set/haul represents the same amount of effort, no effort
# offset is needed. Here, soak time varies from set to set, so we will
# create an effort offset to account for that.

# Final data set for exploration
dat <- as.data.frame(indata)

# We've looked at mean catch by factor; now let's look at mean catch
# as it relates to continuous variables
plot(dat$Tautog ~ dat[, "Salinity"], xlab = "Salinity")
plot(dat$Tautog ~ dat[, "DO"], xlab = "DO")
plot(dat$Tautog ~ dat[, "SurfaceTemp"], xlab = "SurfaceTemp")

## -CHECK COLLINEARITY-##
# We don't want to include covariates that are correlated with each other
pairs(~ Month + Station + SurfaceTemp + Salinity + DO, data = dat)
p.data <- data.frame(
  Year = levels(dat$Year),
  SurfaceTemp = mean(dat$SurfaceTemp),
  Salinity = mean(dat$Salinity),
  DO = mean(dat$DO)
)
p.data <- p.data[p.data$Year > 1986,]
p.data <- p.data[!p.data$Year %in% c(1997, 2010),]

mod <- as.formula("Tautog ~ Year + SurfaceTemp + Salinity + DO")
bmc <- buildmerControl(include= ~ Year)
nb <- buildglmmTMB(mod, dat, family = nbinom2, 
                   buildmerControl = bmc)
#Formula:          Tautog ~ 1 + Year + SurfaceTemp + Salinity

NB <- glmmTMB(formula(nb), #when just year and surface temp, super high STD ERROR
              data = dat,
              family = nbinom2)

bmc2 <- buildmerControl(include= ~ Year, args=list(ziformula = ~ SurfaceTemp + Salinity))
zinb <- buildglmmTMB(mod,
                     data = dat,
                     family = nbinom2, buildmerControl = bmc2)
#Formula: Tautog ~ 1 + Year + SurfaceTemp + Salinity

ZINB <- glmmTMB(formula(zinb) ,
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
#gam <- buildgamm4(mod, data = dat, buildmerControl = bmc4) #works without the family specification

GAM.NB <- gam(Tautog~ Year + s(SurfaceTemp)+s(Salinity) +s(DO),
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
  Year = as.numeric(unique(as.character(dat$Year))),
  # Station=as.character(unique(dat$Station)), #use unique rather than levels bc removed 3 years
  Index = predict(GAM.NB, newdata = p.data, type = "response")
)
index.out <- cbind.data.frame(index.out, SE2)

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
#save(index.out, file="NYWLI.RData")

sim.GAM <- simulateResiduals(GAM.NB)
plot(sim.GAM, quantreg=T) 
vars <- names(GAM.NB$model)
vars <- vars[vars!="Tautog"]
for(v in vars){ #doesn't work?
  plotResiduals(sim.GAM, form=dat[,v], sub=v, quantreg=T)
}
