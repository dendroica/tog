#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This script calculates total catch in numbers of fish
# the length frequencies of different components of catch
# and combines those with the filled-in ALKs to calculate catch-at-age...
# then weight-at-age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:\\Users\\jgorzo\\OneDrive - New Jersey Office of Information Technology\\Documents")

library(tidyverse)
library(readxl)
library(ggplot2)

#Find weight of recreationally harvested fish.
#There are small sample sizes for commercially harvested, so we'll use rec weights to infer comm weights.

life <- read_xlsx("data\\tog/Tautog Data Template 2025_NJDEP.xlsx", sheet = "LifeHistory", skip = 6) 
life24 <- read_xlsx("data\\tog/Tautog Data Template 2025_NJDEP_2024-update.xlsx", sheet = "LifeHistory", skip = 6) 
#the only other place you could dig this out would be ocean trawl...? ventless trap survey??
#ALS? MRIP type 9?


names(life)[names(life)=="Year"] <- "Date"
life$Year <- as.integer(format(life$Date, '%Y'))
life24$`Weight (g)` <- life24$`Weight (g)` * 1000
life24$`Total Length (cm)` <- life24$`Total Length (cm)` / 10

lifehistory <- rbind(life[,names(life24)], life24) %>%
  rename("Weight" = "Weight (g)") %>%  
  mutate(Length = floor(`Total Length (cm)`)) %>% 
  select(Year, Age, Weight, Region, Length)

lifehistory <- lifehistory[!is.na(lifehistory$Weight),]
table(lifehistory$Year)

ggplot(data = lifehistory, aes(x = Weight)) + geom_histogram() + facet_wrap(~Year) + xlab("Recreational Fish Weight (g)")

#We only need to use the recreational weights 
weights_meansnj <- lifehistory %>% group_by(Year) %>% summarize(MeanWeight = mean(Weight, na.rm = TRUE), sdWeight = sd(Weight, na.rm = TRUE), .groups = "keep")

#Length-weight relationships for recreation harvested fish
ggplot(data=lifehistory, aes(x=Length, y=Weight, group=as.factor(Year), colour=as.factor(Year))) + geom_point()
LWallnj <- nls(Weight~alpha*(Length^beta), data = lifehistory, start = c(alpha=5e6, beta = 3))

LWpars <- bind_rows(lapply(c(1:4), function(i){
  #if (i < 4) {
  nlstmpnj <- nls(Weight~alpha*(Length^beta), data = filter(lifehistory, Year == 2020+i), start = c(alpha = 5e6, beta = 3))
  #} else {nls.tmpj <- LWallnj}
  #LW.listnj[[i]]<- nls.tmpnj
  LWdftmpnj <- data.frame(Year = 2020+i, a = summary(nlstmpnj)$parameters[1,1], b = summary(nlstmpnj)$parameters[2,1])
  #preds.tmpnj <- data.frame(Length = min(weights_recnj_3$Length.CM, na.rm = TRUE):max(weights_recnj_3$Length.CM, na.rm = TRUE))
  #preds.tmpnj$Pred <- LW.df.tmpnj$a*((preds.tmpnj$Length)^LW.df.tmpnj$b)
  #preds.listnj[[i]] <- preds.tmpnj
  #ggplot(data = preds.listnj[[i]], aes(x = Length, y = Pred)) + geom_point() + ggtitle(paste(2020+i))
}))

#Load commercial catch data
commcatchnj <- read_xlsx("data\\tog/Regional_Comm_landings_MT_07.18.25.xlsx", sheet = "MT")[,c("Year","NYB-NJ")]
names(commcatchnj)[names(commcatchnj)=="NYB-NJ"] <- "Comm" #metric tons
commcatchnj$Comm <- commcatchnj$Comm*1000000
commcatchyrsumnj <- commcatchnj %>% left_join(weights_meansnj) %>% mutate(CommCatchNumFish = Comm/MeanWeight)


#Load recreational catch and discard

totalcatchnj <- read.csv("data\\tog/rec/RecData/Tautog_MRIP_TotalCatch_2021-2024_NJNYB.csv", header = TRUE) %>% 
  mutate(DiscardMortality = Released.B2*0.025) %>% 
  mutate(TotalRecCatch = Harvest.A.B1+DiscardMortality)
totalcatchnj <- left_join(totalcatchnj, commcatchyrsumnj) %>% mutate(TotalCatch = TotalRecCatch + CommCatchNumFish)

#Load ALKs, convert to proportions

indir <- "./output/tog/alk/filled/opercboth"
alk2021numnj <- read.csv(file.path(indir, "NJNYB-ALK_2021_filled.csv")) %>% select(2:13) %>% mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) #add row sum
alk2021propnj <- alk2021numnj %>% mutate(across(2:12, .fns= function(x){x/rowsum})) %>% replace(is.na(.), 0)
alk2022numnj <- read.csv(file.path(indir, "NJNYB-ALK_2022_filled.csv")) %>% select(2:13) %>% mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) #add row sum
alk2022propnj <- alk2022numnj %>% mutate(across(2:12, .fns= function(x){x/rowsum})) %>% replace(is.na(.), 0)
alk2023numnj <- read.csv(file.path(indir, "NJNYB-ALK_2023_filled.csv")) %>% select(2:13) %>% mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) #add row sum
alk2023propnj <- alk2023numnj %>% mutate(across(2:12, .fns= function(x){x/rowsum})) %>% replace(is.na(.), 0)
alk2024numnj <- read.csv(file.path(indir, "NJNYB-ALK_2024_filled.csv")) %>% select(2:13) %>% mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) #add row sum
alk2024propnj <- alk2024numnj %>% mutate(across(2:12, .fns= function(x){x/rowsum})) %>% replace(is.na(.), 0)
names(alk2021propnj)[1] <- "Length"
names(alk2022propnj)[1] <- "Length"
names(alk2023propnj)[1] <- "Length"
names(alk2024propnj)[1] <- "Length"

#Length Frequencies for Recreational Catch and Discard, modified from discard_LFnj.R from Samarah Nehemiah for LIS
harvest_LFnj <- read.csv("./output/tog/NJNYB_RecHarvest_Frequencies.csv")
discard_LFnj <- read.csv("./output/tog/discard_LF.csv")[,2:6]
names(harvest_LFnj)<- c("Length", "2021", "2022", "2023", "2024")
names(discard_LFnj)<- c("Length", "2021", "2022", "2023", "2024")
discard_LF_propnj <- discard_LFnj %>% mutate(across(`2021`:`2024`, .fns = function(x){x/sum(x,na.rm = TRUE)}))
harvest_LF_propnj <- harvest_LFnj %>% mutate(across(`2021`:`2024`, .fns = function(x){x/sum(x, na.rm = TRUE)}))

#Recreational Catch Catch-at-Age
#MRIP
recharvestcatchatage2021nj <- left_join(harvest_LFnj[,1:2], alk2021propnj) %>% rename(Number = `2021`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% select(-c("Number", "Total.Count"))
recharvestcatchatage2022nj <- left_join(harvest_LFnj[,c(1,3)], alk2022propnj) %>% rename(Number = `2022`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% select(-c("Number", "Total.Count"))
recharvestcatchatage2023nj <- left_join(harvest_LFnj[,c(1,4)], alk2023propnj) %>% rename(Number = `2023`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% select(-c("Number", "Total.Count"))
recharvestcatchatage2024nj <- left_join(harvest_LFnj[,c(1,5)], alk2024propnj) %>% rename(Number = `2024`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)%>% select(-c("Number", "Total.Count"))

#Commercial Catch Catch-at-Age and Weight-at-Age after Katie Drew's template
#She fits a length-weight relationship, then convert the length-bins to weight bins.
#Then calculates the sample weight frequencies by multiplying the numbers at length by the weight at length
#Then she calculates the portion of the sample weight in each length bin 

alklist <- list(alk2021propnj[,-13], alk2022propnj[,-13], alk2023propnj[,-13], alk2024propnj[,-13]) 

##Recreational Discard Catch-at-Age
recdiscardcatchatage2021_prop <- left_join(discard_LF_propnj[,1:2], alk2021propnj) %>% rename(Number = `2021`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2021_num <- recdiscardcatchatage2021_prop[,c(1,3:14)]
recdiscardcatchatage2021_num[,c(2:13)]<- recdiscardcatchatage2021_num[,c(2:13)]*totalcatchnj$DiscardMortality[which(totalcatchnj$Year==2021)]
recdiscardcatchatage2021_num <- recdiscardcatchatage2021_num[recdiscardcatchatage2021_num$Length %in% 29:59,]

recdiscardcatchatage2022_prop <- left_join(discard_LF_propnj[,c(1,3)], alk2022propnj) %>% rename(Number = `2022`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2022_num <- recdiscardcatchatage2022_prop[,c(1,3:14)]
recdiscardcatchatage2022_num[,c(2:13)]<- recdiscardcatchatage2022_num[,c(2:13)]*totalcatchnj$DiscardMortality[which(totalcatchnj$Year==2022)]
recdiscardcatchatage2022_num <- recdiscardcatchatage2022_num[recdiscardcatchatage2022_num$Length %in% 29:59,]

recdiscardcatchatage2023_prop <- left_join(discard_LF_propnj[,c(1,4)], alk2023propnj) %>% rename(Number = `2023`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2023_num <- recdiscardcatchatage2023_prop[,c(1,3:14)]
recdiscardcatchatage2023_num[,c(2:13)]<- recdiscardcatchatage2023_num[,c(2:13)]*totalcatchnj$DiscardMortality[which(totalcatchnj$Year==2023)]
recdiscardcatchatage2023_num <- recdiscardcatchatage2023_num[recdiscardcatchatage2023_num$Length %in% 29:59,]

recdiscardcatchatage2024_prop <- left_join(discard_LF_propnj[,c(1,5)], alk2024propnj) %>% rename(Number = `2024`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2024_num <- recdiscardcatchatage2024_prop[,c(1,3:14)]
recdiscardcatchatage2024_num[,c(2:13)]<- recdiscardcatchatage2024_num[,c(2:13)]*totalcatchnj$DiscardMortality[which(totalcatchnj$Year==2024)]
recdiscardcatchatage2024_num <- recdiscardcatchatage2024_num[recdiscardcatchatage2024_num$Length %in% 29:59,]

caa2 <- list(
  recdiscardcatchatage2021_num[,2:12] + recharvestcatchatage2021nj[,2:12], #/1000,
  recdiscardcatchatage2022_num[,2:12] + recharvestcatchatage2022nj[,2:12], #/1000,
  recdiscardcatchatage2023_num[,2:12] + recharvestcatchatage2023nj[,2:12], #/1000,
  recdiscardcatchatage2024_num[,2:12] + recharvestcatchatage2024nj[,2:12] #/1000
)

caa <- cbind(X1=c(0,0,0,0), rbind(
  apply(recdiscardcatchatage2021_num[,2:12] + recharvestcatchatage2021nj[,2:12], 2, sum), #/1000,
  apply(recdiscardcatchatage2022_num[,2:12] + recharvestcatchatage2022nj[,2:12], 2, sum), #/1000,
  apply(recdiscardcatchatage2023_num[,2:12] + recharvestcatchatage2023nj[,2:12], 2, sum), #/1000,
  apply(recdiscardcatchatage2024_num[,2:12] + recharvestcatchatage2024nj[,2:12], 2, sum) #/1000
))

waas <- list()

for(y in 1:4){
  alk.in <-as.data.frame(alklist[[y]])
  
  #subset to the seasonal comm harvest
  comm.in <- totalcatchnj$Comm[y] #this is in metric tons converted to g
  
  #Get your yearly L-W pars
  a <- LWpars[y,2]
  b <- LWpars[y,3]
  
  #subset your LF
  LF.vec <- harvest_LFnj[,1+y]
  
  # Convert length bins to weights. Our length bins are floored, so we'll
  # add 0.5 to get to the center of the bin for the weight calculations.
  # If you rounded your lengths, you don't need to add anything.
  W_LBin <- a*((alk.in[,1]+0.5)^b)
  WAAL <- apply(caa2[[y]], 2, function(x) x*W_LBin)
  nums <- apply(caa2[[y]], 2, sum)
  weights <- apply(WAAL, 2, sum)
  waa <- weights/nums
  waas[[y]] <- waa
  
  print(paste("total weight", 2020+y))
  print((sum(weights) + comm.in)/1000000)
}

caa <- as.data.frame(caa) #final output: catch-at-age
mysum <- apply(caa, 1, sum)
prop <- apply(caa[,1:12], 2, function(x) x/mysum)
waa <- cbind(X1=c(0,0,0,0), bind_rows(waas))/1000 #final output: weight-at-age
#for the ASAP inputs, these appear to be scaled?

