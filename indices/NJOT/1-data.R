library(car)
library(lmtest)
library(glmmTMB)
library(DHARMa)
library(bbmle)
library(ggeffects)
library(ggplot2)
require(readxl)
require(lattice)
require(emmeans)
require(rmarkdown)
library(reshape2)
library(doBy)
library(dplyr)
library(foreign)
root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)
source("./indices/bootstrap_functions.R")

geomean0 <- function(x) {
  exp(mean(log(x + 1))) - 1
}

pctPos <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  mean(x)
}

disp3 <- function(mod = NA) {
  E <- residuals(mod, type = "pearson")
  d <- sum(E^2) / summary(mod)$AICtab[5]
  return(d)
}

indata <- read.dbf(file.path(root, "data/tog/TOABUN.DBF"))

# limit to just yrs >1989 (1988 30 min tows; 1989 6 cruises; 1990 5 cruises (Dec+Feb combined into single January cruise))
indata <- indata[indata$YEAR>1988,]

# relabel strata:
forMerge <- data.frame("STRATUM"=12:26,"stratum2"=rep(c("inner","middle","outer"),times=5))
indata <- merge(indata,forMerge)                                                # YEAR order gets shuffled here
indata <- indata[order(indata$YEAR),]
indata$depth <- indata$MAXDEPTH

keepVars <- c("ID","CRUCODE","STRATUM","YRMODA","MINOUT","TOWLENGTH","depth","TEMPBOT","SALBOT","DOBOT","stratum2")

# how many records would we lose to NAs:
indata$Month <- substr(indata$YRMODA,5,6)
indata$YOY <- as.numeric(indata$NUMBER)

# Based on EDA, keeping just inshore stratum and later cruises
indata2 <- indata[as.numeric(substr(indata$CRUCODE,5,5)) %in% c(4:6),]
indata2 <- indata2[indata2$stratum2=="inner",]# | indata2$stratum2=="middle"]
indata2 <- indata2[complete.cases(indata2[,keepVars]),] # keep only complete cases
# Check for outliers or weirdo values
dat = data.frame(CPUE = indata2$YOY,
                 YEAR = as.factor(indata2$YEAR),
                 MONTH = as.factor(indata2$Month),
                 VESSEL=as.factor(indata2$VESSEL),
                 STRATA=as.factor(indata2$STRATUM),
                 STEMP = scale(indata2$TEMPSURF, center = TRUE, scale = TRUE), #Z.scr(indata2$TEMPSURF), 
                 BTEMP = scale(indata2$TEMPBOT, center = TRUE, scale = TRUE), #Z.scr(indata2$TEMPBOT),
                 SSAL = scale(indata2$SALSURF, center = TRUE, scale = TRUE), #Z.scr(indata2$SALSURF),
                 BSAL = scale(indata2$SALBOT, center = TRUE, scale = TRUE), #Z.scr(indata2$SALBOT),
                 SDO = scale(indata2$DOSURF, center = TRUE, scale = TRUE), #Z.scr(indata2$DOSURF),
                 BDO = scale(indata2$DOBOT, center = TRUE, scale = TRUE), #Z.scr(indata2$DOBOT),
                 DEPTH = scale(indata2$depth, center = TRUE, scale = TRUE), #Z.scr(indata2$depth),
                 EFFORT = indata2$MINOUT)

# Calculate the proportion of positive tows/sets/hauls
dat$PosTow <- ifelse(dat$CPUE > 0, 1, 0)

# create offset for variable tow durations
dat$lnEffort <- log(dat$EFFORT)
save(dat, file = "NJOT.RData")