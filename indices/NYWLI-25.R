#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tautog indices standardizatin
# 2025 Assessment update
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#----Load packages----

#install.packages('DHARMa')
#install.packages('bbmle')
#install.packages('mgcViz')

library(car)
library(tidyverse)
library(lmtest)
library(glmmTMB)
library(DHARMa)
library(bbmle)
library(mgcv)
library(mgcViz)
library(readxl)

root <- "C:/Users"
usr <- "galax"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)

wlidata <- read_excel(file.path(root, "data/tog", "2025_SA_NY_Tautog_Survey_Data_1984-2024.xlsx"), sheet="WLIS Seine")
# Source helper functions for bootstrapping
source("./indices/bootstrap_functions.r")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# -- Start NY WLI sein survey -----
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Clean and prep data ####

# Read in the data: set-by-set data from a stratified
# gillnet survey.
#wlidata <- read.csv("NY-wli-1984.csv", header = TRUE)
head(wlidata)


## Variables ##
# Year
# Month
# Body.of.water-- where it was sampled
# TOW.ID - specif id based on year
# Station - fixed station id
# SurfaceTemp--Surface temperature
# Salinity--Surface salinity 
# DO--Surface dissolved oxygen (assuming surface_)
# Tautog--Number of Taugtoh observed in sample
# Region-- LIS (long island sound) or NYB (new york bight)


# subset to just LIS
wlidata <- subset(wlidata, Region=="NYB")

summary(wlidata)

#typo in Body of water Manhasset Bay and Manhassett Bay (one t is corect)
wlidata$Body.of.water<-ifelse(wlidata$`Body of water`=="Manhassett Bay", "Manhasset Bay",wlidata$`Body of water`)
unique(wlidata$Body.of.water)



# First, create a new dataframe with only the variables you're interested
# in exploring. For this dataset, we're not interested in day of month or
# sample ID, but we do want everything else. We want year, month, and stratum
# to be factors, but we'll leave the others as continuous variables. 

wlidata2 <- wlidata %>%
  mutate(Year = as.factor(Year),
         Month = as.factor(Month),
         Body.of.water = as.factor(Body.of.water),
         Station= as.factor(Station)) %>%
  select(!c(Date, `TOW ID`,Region))

head(wlidata2)


# Check to see if you have any years where you have zero
# catch (no positive tows/hauls/sets) 
wlidata2$Tautog <- wlidata2$`# Tautog`
xtabs(Tautog~Year, wlidata2)

#1985, 1994, and 2009 have 0 catch so want to drop those years
str(wlidata2)

rmyrs<-c('1985','1994','2009')

wlidata3<-subset(wlidata2, ! Year %in% rmyrs)
# xtabs(Tautog~Year, wlidata3)
#it is working but it replaces the value with a 0 when you use xtabs
unique(wlidata3$Year)
#wlidata3<-wlidata2

# Decide if you need to subset your data to specific strata, months, etc.
# based on when/where you most reliably catch (or expect to catch) your species.
# Check mean catch:
vals<-names(wlidata3)
for(i in vals){
  if(is.factor(wlidata3[,i])){
    barplot(tapply(wlidata3$Tautog, wlidata3[,i], mean), xlab=i, ylab="Mean CPUE")
  }
}


#don't catch a log in May or June but will leave for now
#some stations don't have tautog catch

# unique(wlidata2$Station)
# xtabs(Tautog~Station, wlidata2)

# LNB06, LNB09, LNB10, MAN08, MAN09 have 0 catch ever
# maybe should remove but will continue with all stations for now

unique(wlidata3$Month)


# Check the proportion of positive samples:
# Calculate the proportion of positive tows/sets/hauls
wlidata3$PosTow <- ifelse(wlidata3$Tautog > 0, 1, 0)

for(i in names(wlidata3)){
  if(is.factor(wlidata3[,i])){
    barplot(tapply(wlidata3$PosTow, wlidata3[,i], mean), xlab=i, ylab="Proportion Positive")
  }
}

# Check the sample size for different factors:
vars <- names(wlidata3)
vars <- vars[!(vars %in% c("Year", "Tautog", "PosTow"))]
for(i in vars){
  if(is.factor(wlidata3[,i])){
    tmp <- ggplot(wlidata3) + geom_bar(aes(x=Year)) + 
      facet_wrap(~wlidata3[,i], ncol=1) + theme_bw()
    print(tmp)
    print(xtabs(~wlidata3$Year+wlidata3[,i]))
  }
}


# Tautog has highest catch in May and June, 
# less so in Sept and Oct but those have high proportion of positive catch 
# so will leave all months in for now
#indata3 <- droplevels(indata2[indata2$MONTH %in% c(8:11),])


# In the end, we are going to calculate the index by predicting CPUE by
# year by holding all other factors/covariates constant at one level. 
# R assigns levels of factors alphabetically by default, but the first level
# may not be exactly what you want. If the first level is not well-sampled
# or if it has a low CPUE, high variance, etc. you might want to relevel
# your factors.

for(i in vars){
  if(is.factor(wlidata3[,i])){
    barplot(tapply(wlidata3$Tautog, wlidata3[,i], mean), xlab=i, ylab="CPUE")
    barplot(tapply(wlidata3$Tautog, wlidata3[,i], function(x) sd(x)/mean(x)), 
            xlab=i, ylab="CV")
  }
}

#checking which stations catch tautog and during what years
new<-wlidata3 %>% group_by(Year, Station) %>% summarise(N=sum(Tautog))
ggplot(data=new, aes(x=Station, y=N))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Year, scales='free')

barplot(tapply(wlidata3$Tautog, wlidata3$Station, mean), xlab="Station", ylab="CPUE")

unique(wlidata3$Station[wlidata3$Year==1984])
stations<-unique(wlidata3$Station[wlidata3$Year==2024])
wlidata3$SurfaceTemp <- wlidata3$`Surface Temp`
wlidata3$`Surface Temp` <-NULL
wlidata4<-subset(wlidata3,Station %in% stations)
unique(wlidata4$Station)


#checking which stations catch tautog and during what years
new2<-wlidata4 %>% group_by(Year, Station) %>% summarise(N=sum(Tautog))
ggplot(data=new2, aes(x=Station, y=N))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Year, scales='free')



# Generally speaking, we don't want to work with indices with less than 10%
# positive tows/hauls/sets -- it suggests that this survey may not be doing
# a good job of sampling that species. Now that we've subset down to the best
# months, strata, etc., let's check the annual proportion positive
barplot(tapply(wlidata4$PosTow, wlidata4$Year, mean), ylab="Proportion positive")
abline(h=0.1, col="red")



# We need complete cases to compare across models (e.g., a model with temperature
# compared to a model without temperature), so we'll drop records
# that have missing values in the covariates we care about. But first, make sure
# you have observations of all your covariates in all years; if you didn't
# collect information on temperature in all years, for example, you'll have
# to either drop temperature from the model, or drop those years from the 
# index. A quick set of plots can help identify years with missing covariates.
wlidata4$`Body of water` <- NULL
wlidata4$`# Tautog` <- NULL

for(i in names(wlidata4)) {
  y <- wlidata4[,i][[1]]
  plot(as.numeric(as.character(wlidata4$Year)), y, ylab=i, xlab="Year")
  print(i)
  print(tapply(y, wlidata4$Year, function(x) round((sum(is.na(x))/length(x))*100,1)))
}

#1985, 1994, 2009 are empty
#check for outliers?
plot(wlidata4$Salinity)
plot(wlidata4$SurfaceTemp)
plot(wlidata4$DO)

# surface temp has one extreme outlier that == 243??? in 2016
# could just be a typo of 24.3 but am going to remove
# get filtered out when stations are removed
#wlidata4$Tautog[wlidata4$SurfaceTemp==243.0]
#View(wlidata3)
#wlidata4<-subset(wlidata4, SurfaceTemp != 243)
#View(wlidata4)


# salinity and DO don't have values in 1984, DO does not have values in 1986, 1997
# will drop these values from standardization
colnames(wlidata4)
#wlidata5<-wlidata4[,-which(names(df) %in% c("z","u"))]
#colnames(wlidata5)


# take complete cases
indata <- wlidata4[complete.cases(wlidata4),]

# If each tow/set/haul represents the same amount of effort, no effort
# offset is needed. Here, soak time varies from set to set, so we will
# create an effort offset to account for that.

# this survey has no effort category so we will skip
#indata4$lnEffort <- log(indata4$EFFORT)


# Final dataset for exploration
dat <- indata

# We've looked at mean catch by factor; now let's look at mean catch
# as it relates to continuous variables
for(i in colnames(dat)){
  if(is.numeric(dat[,i])){
    plot(dat$Tautog~dat[,i], xlab=i)
  }
}

##-CHECK COLLINEARITY-##
# We don't want to include covariates that are correlated with each other
pairs(~Month+Station+Body.of.water+SurfaceTemp+Salinity+DO,data=dat)

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

dat <- as.data.frame(dat)

### this below code is not working?

# Compare residuals vs. factors NBI
vars <- names(NB2$frame)
vars <- vars[vars!="Tautog"]
for(v in vars){ #not working
  plotResiduals(sim.NB2, form=dat[,v], sub=v, quantreg=T)
}

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


#=============================#
#### Calculate the Index ####
#=============================#

#best <-NB2
best <- GAM.NB
# We will calculate the index by predicting the mean CPUE in each year while
# holding the other factors constant at one level and holding continuous values
# constant at their mean values. 

# What factors did ZINB.2
#NB2$call
GAM.NB$call
p.data <- data.frame(Year=levels(dat$Year), 
                     #Station=levels(dat$Station)[1],
                     SurfaceTemp=mean(dat$SurfaceTemp),
                     DO=mean(dat$DO),
                     Salinity=mean(dat$Salinity))

# The bootstrap_functions.R script also has a function to do this.
#p.data <- expand.pred(best$frame)
p.data <- expand.pred(GAM.NB$model)

# We may also calculate the marginal mean by expanding the data to get every
# combination of factor in the model and then calculating the mean CPUE
# over all combinations for each year. Your TC/SAS can decide which approach
# is preferred.

index.out <- data.frame(
  Year = as.numeric(unique(dat$Year)), #PICK UP HERE JESS
  #Station=as.character(unique(dat$Station)), #use unique rather than levels bc removed 3 years
                        Index= predict(best, newdata=p.data, type="response"))

# We will bootstrap the index to get the CVs and confidence intervals.
# Use boot.NB() for non-zero-inflated models and boot.GAM() for
# GAMs. boot.ZI(), boot.NB(), and boot.GAM() are custom functions included in the 
# "bootstrap_functions.r" file.

#SE <- boot.ZI(best, nboots=1000)
SE <- boot.GAM(best, nboots=1000) #100% convergence! JESS
#only 94.9% of bootstraps converged..
# If you have a low percent (<50%) of converged runs, it's a sign that the model
# may not be robust. 

index.out <- cbind.data.frame(index.out, SE)


index.out$Year <- as.integer(levels(unique(dat$YEAR)))
index.out <- index.out[index.out$Year > 1988,]
index.out <- rbind(index.out, c(1994, -1, -1, -1, -1), c(2009, -1, -1, -1, -1))
index.out <- index.out[order(index.out$Year),]
index.out$CV <- index.out$SE/index.out$Index

jpeg("NY_WLI_index.jpeg", width=8, height=5, units="in",res=600)
ggplot(index.out) + geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), alpha=0.4) +
  geom_point(aes(x=Year,y=Index),shape=16)+
    geom_line(aes(x=Year, y=Index)) + 
  ylim(c(0,NA)) + theme_bw()
dev.off()

write.csv(index.out, "NYWLI_index.csv", row.names=F)







# compare to previous index
index.out$scale<-index.out$Index/mean(index.out$Index)
index.out$newYear<-index.out$Year+1
#update<-read.csv("NYWLIindexfromlastupdate.csv",header=TRUE)
#update<-update[-c(1,3,12,27),]
#plot(update$Index)
#update$scale<-update$Index/mean(update$Index)

jpeg("NYWLI_compare.jpeg", width=8, height=5, units="in",res=600)
ggplot(index.out) + 
  geom_point(aes(x=newYear,y=scale),shape=16)+
  geom_line(aes(x=newYear, y=scale),linetype = 1) + 
  #geom_point(data=update,aes(x=Year,y=scale),shape=16,col="blue")+
  #geom_line(data=update,aes(x=Year, y=scale),linetype = 1,col="blue") +
  #geom_path(linejoin = "mitre")+
  ylim(c(0,NA)) + theme_bw()#+ ggtitle("scaled to mean")
dev.off()