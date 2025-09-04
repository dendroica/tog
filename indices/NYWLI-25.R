#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tautog indices standardizatin
# 2025 Assessment update
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#----Load packages----
#install.packages('DHARMa')
#install.packages('bbmle')
#install.packages('mgcViz')
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

rmyrs<-c('1984','2010')

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

dat <- as.data.frame(dat)

#=============================#
#### Calculate the Index ####
#=============================#
ZINB <- glmmTMB(Tautog ~ 1 + Year + SurfaceTemp + Salinity ,
                ziformula = ~ SurfaceTemp + Salinity,
                data = dat,
                family = nbinom2)
GAM.NB <- gam(Tautog~ Year + s(SurfaceTemp)+s(Salinity)+s(DO),
              data = dat, family = 'nb')
SE2 <- boot.GAM(GAM.NB, nboots=1000)
#best <-NB2
best <- ZINB
# We will calculate the index by predicting the mean CPUE in each year while
# holding the other factors constant at one level and holding continuous values
# constant at their mean values. 

# What factors did ZINB.2

p.data <- data.frame(Year=levels(dat$Year), 
                     #Station=levels(dat$Station)[1],
                     SurfaceTemp=mean(dat$SurfaceTemp),
                     #DO=mean(dat$DO),
                     Salinity=mean(dat$Salinity))

# The bootstrap_functions.R script also has a function to do this.
#p.data <- expand.pred(best$frame)
p.data <- expand.pred(best$frame, re=best$modelInfo$grpVar)

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
SE <- boot.ZI(best, nboots=1000) 
# If you have a low percent (<50%) of converged runs, it's a sign that the model
# may not be robust. 

index.out <- cbind.data.frame(index.out, SE)

index.out$Year <- as.integer(levels(p.data$Year))
allyrs <- 1989:2024
missing <- allyrs[!allyrs %in% p.data$Year]
index.out <- index.out[index.out$Year > 1987,]
index.out <- rbind(index.out, c(1997, -1, -1, -1, -1), c(2010, -1, -1, -1, -1))
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