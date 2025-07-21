#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This script calculates total catch in numbers of fish, the length frequencies of different components of catch, and combines those with the filled-in ALKs to calculate catch-at-age, then weight-at-age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:\\Users\\galax\\OneDrive - New Jersey Office of Information Technology\\Documents")

library(tidyverse)
library(readxl)

#Find weight of recreationally harvested fish.
  #There are small sample sizes for commercially harvested, so we'll use rec weights to infer comm weights.
  #All weight data was from Virginia

weights_recnj <- read_xlsx("data\\tog/Tautog Data Template 2025_NJDEP.xlsx", sheet = "LifeHistory", skip = 6) 
#the only other place you could dig this out would be ocean trawl...? ventless trap survey??
#ALS? MRIP type 9?
#weights_comnj <- read_xlsx("./Data/State Data Submissions/VA Tautog data template 2021-2024.xlsx", sheet = "LifeHistory (Com)", skip = 9)
names(weights_recnj)[1] <- "Date"
weights_recnj$Year <- as.integer(format(weights_recnj$Date, '%Y'))

table(weights_recnj$Year)
#table(weights_com$Year)

table(weights_recnj$Year[which(!is.na(weights_recnj$`Weight (g)`))])
#table(weights_com$Year[which(!is.na(weights_com$`Weight (g)`))])

ggplot(data = weights_recnj, aes(x = `Weight (g)`)) + geom_histogram() + facet_wrap(~Year) + xlab("Recreational Fish Weight (g)")
#ggplot(data = weights_com, aes(x = `Weight (g)`)) + geom_histogram() + facet_wrap(~Year) + xlab("Commercial Fish Weight (g)")

#weights_recnj$Source = "Recreational"
#weights_com$Source = "Commercial"
#weights_bothnj <- rbind(weights_recnj, weights_com)

#weights_2nj <- weights_both %>% select(Year, FishID, `Length (Mm)`, Age, `Weight (g)`, Sex, Region, Source)
weights_recnj_2 <- weights_recnj %>% select(Year, `Total Length (cm)`, Age, `Weight (g)`, Region)

#weights_means_bothnj <- weights_2 %>% group_by(Year, Source) %>% summarize(MeanWeight = mean(`Weight (g)`, na.rm = TRUE), sdWeight = sd(`Weight (g)`, na.rm = TRUE), .groups = "keep")

#weights_means_combinednj <- weights_2 %>% group_by(Year) %>% summarize(MeanWeight = mean(`Weight (g)`, na.rm = TRUE), sdWeight = sd(`Weight (g)`, na.rm = TRUE), .groups = "keep")

#We only need to use the recreational weights 
weights_meansnj <- weights_recnj_2 %>% group_by(Year) %>% summarize(MeanWeight = mean(`Weight (g)`, na.rm = TRUE), sdWeight = sd(`Weight (g)`, na.rm = TRUE), .groups = "keep")

#Length-weight relationships for recreationally harvested fish
weights_recnj_3 <- weights_recnj_2 %>%  mutate(Length.CM = floor(`Total Length (cm)`), Weight = `Weight (g)`)
LW.allnj <- nls(Weight~alpha*(Length.CM^beta), data = weights_recnj_3, start = c(alpha=5e6, beta = 3))

LW.pars <- data.frame()
LW.listnj <- vector(mode = "list", length = 4)
preds.listnj <- vector(mode = "list", length = 4)

for(i in 1:4){
  if (i < 4) {
    nls.tmpnj <- nls(Weight~alpha*(Length.CM^beta), data = filter(weights_recnj_3, Year == 2020+i), start = c(alpha = 5e6, beta = 3))
  } else {nls.tmpj <- LW.allnj}
  LW.listnj[[i]]<- nls.tmpnj
  LW.df.tmpnj <- data.frame(Year = 2020+i, a = summary(nls.tmpnj)$parameters[1,1], b = summary(nls.tmpnj)$parameters[2,1])
  LW.pars <- rbind(LW.pars, LW.df.tmpnj)
  preds.tmpnj <- data.frame(Length = min(weights_recnj_3$Length.CM, na.rm = TRUE):max(weights_recnj_3$Length.CM, na.rm = TRUE))
  preds.tmpnj$Pred <- LW.df.tmpnj$a*((preds.tmpnj$Length)^LW.df.tmpnj$b)
  preds.listnj[[i]] <- preds.tmpnj
  ggplot(data = preds.listnj[[i]], aes(x = Length, y = Pred)) + geom_point() + ggtitle(paste(2020+i))
}

#Load commercial catch data
commcatchnj <- read_xlsx("data\\tog/Regional_Comm_landings_MT_07.18.25.xlsx", sheet = "MT")[,c(1,3)] #CHANGE THIS TO NEW FILE
names(commcatchnj)[2] <- "Comm" #metric tons
commcatchnj$Comm <- commcatchnj$Comm*1000000
commcatchyrsumnj <- commcatchnj %>% left_join(weights_meansnj) %>% mutate(Comm.Catch.Num.Fish = Comm/MeanWeight)


#Load recreational catch and discard

totalcatchnj <- read.csv("data\\tog/rec/RecData/Tautog_MRIP_TotalCatch_2021-2024_NJNYB.csv", header = TRUE) #Tautog_MRIP_TotalCatch_2021-202
totalcatchnj <- mutate(totalcatchnj, Discard.Mortality = Released.B2*0.025, Total.Rec.Catch = Harvest.A.B1+Discard.Mortality)
totalcatchnj <- left_join(totalcatchnj, commcatchyrsumnj) %>% mutate(Total.Catch = Total.Rec.Catch + Comm.Catch.Num.Fish)

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
#alk2021propnj$Length <- as.integer(alk2021propnj$Length)
#alk2022propnj$Length <- as.integer(alk2022propnj$Length)
#alk2023propnj$Length <- as.integer(alk2023propnj$Length)
#alk2024propnj$Length <- as.integer(alk2024propnj$Length)

#Length Frequencies for Recreational Catch and Discard, modified from discard_LFnj.R from Samarah Nehemiah for LIS
harvest_LFnj <- read.csv("./output/tog/NJNYB_RecHarvest_Frequencies.csv")
discard_LFnj <- read.csv("./output/tog/discard_LF.csv")[,2:6]
names(harvest_LFnj)<- c("Length", "2021", "2022", "2023", "2024")
names(discard_LFnj)<- c("Length", "2021", "2022", "2023", "2024")
discard_LF_propnj <- discard_LFnj %>% mutate(across(`2021`:`2024`, .fns = function(x){x/sum(x,na.rm = TRUE)}))
harvest_LF_propnj <- harvest_LFnj %>% mutate(across(`2021`:`2024`, .fns = function(x){x/sum(x, na.rm = TRUE)}))
#Manually adding in 77 and 78cm to match for commercial harvest
#harvest_LFnj[64,]<-c(77,0,0,0,0)
#harvest_LFnj[65,]<-c(78,0,0,0,0)
0.229 +	0.079 +	4.276	+ 12.542 + 85.797 +	136.81 + 93.457 + 44.299 +	21.249+ 10.776 + 15.11 + 39.39

#Recreational Catch Catch-at-Age
#MRIP
recharvestcatchatage2021nj <- left_join(harvest_LFnj[,1:2], alk2021propnj) %>% rename(Number = `2021`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% select(-c("Number", "Total.Count"))
recharvestcatchatage2022nj <- left_join(harvest_LFnj[,c(1,3)], alk2022propnj) %>% rename(Number = `2022`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% select(-c("Number", "Total.Count"))
recharvestcatchatage2023nj <- left_join(harvest_LFnj[,c(1,4)], alk2023propnj) %>% rename(Number = `2023`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% select(-c("Number", "Total.Count"))
recharvestcatchatage2024nj <- left_join(harvest_LFnj[,c(1,5)], alk2024propnj) %>% rename(Number = `2024`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)%>% select(-c("Number", "Total.Count"))

#Commercial Catch Catch-at-Age
#We just use the average weight to convert commercial harvest from g to numbers. And then we apply the LF and ALK to those numbers.
oldcomharvestcatchatage2021_propnj <- left_join(harvest_LF_propnj[,1:2], alk2021propnj) %>% rename(Number = `2021`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
oldcomharvestcatchatage2021_numnj <- oldcomharvestcatchatage2021_propnj[,c(1,3:14)]
oldcomharvestcatchatage2021_numnj[,c(2:13)]<-oldcomharvestcatchatage2021_numnj[,c(2:13)]*totalcatchnj$Comm.Catch.Num.Fish[which(totalcatchnj$Year==2021)]

oldcomharvestcatchatage2022_propnj <- left_join(harvest_LF_propnj[,c(1,3)], alk2022propnj) %>% rename(Number = `2022`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
oldcomharvestcatchatage2022_numnj <- oldcomharvestcatchatage2022_propnj[,c(1,3:14)]
oldcomharvestcatchatage2022_numnj[,c(2:13)]<-oldcomharvestcatchatage2022_numnj[,c(2:13)]*totalcatchnj$Comm.Catch.Num.Fish[which(totalcatchnj$Year==2022)]

oldcomharvestcatchatage2023_propnj <- left_join(harvest_LF_propnj[,c(1,4)], alk2023propnj) %>% rename(Number = `2023`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
oldcomharvestcatchatage2023_numnj <- oldcomharvestcatchatage2023_propnj[,c(1,3:14)]
oldcomharvestcatchatage2023_numnj[,c(2:13)]<-oldcomharvestcatchatage2023_numnj[,c(2:13)]*totalcatchnj$Comm.Catch.Num.Fish[which(totalcatchnj$Year==2023)]

#oldcomharvestcatchatage2024_propnj <- left_join(harvest_LF_propnj[,c(1,5)], alk2024propnj) %>% rename(Number = `2024`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
#oldcomharvestcatchatage2024_numnj <- oldcomharvestcatchatage2024_propnj[,c(1,3:14)]
#oldcomharvestcatchatage2024_numnj[,c(2:13)]<-oldcomharvestcatchatage2024_numnj[,c(2:13)]*totalcatchnj$Comm.Catch.Num.Fish[which(totalcatchnj$Year==2024)]

#Commercial Catch Catch-at-Age and Weight-at-Age after Katie Drew's template
#She fits a length-weight relationship, then convert the length-bins to weight bins.
#Then calculates the sample weight frequencies by multiplying the numbers at length by the weight at length
#Then she calculates the portion of the sample weight in each length bin 

alklist <- list(alk2021propnj[,-13], alk2022propnj[,-13], alk2023propnj[,-13], alk2024propnj[,-13]) 

#Create 
CAA.comm <- data.frame()

#You may also be interested in average weight-at-age in the catch
WAA.comm <- data.frame()

CAAL.comm <- vector(mode = "list", length = 4)
WAAL.comm <- vector(mode = "list", length = 4)

##Recreational Discard Catch-at-Age
recdiscardcatchatage2021_prop <- left_join(discard_LF_propnj[,1:2], alk2021propnj) %>% rename(Number = `2021`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2021_num <- recdiscardcatchatage2021_prop[,c(1,3:14)]
recdiscardcatchatage2021_num[,c(2:13)]<- recdiscardcatchatage2021_num[,c(2:13)]*totalcatchnj$Discard.Mortality[which(totalcatchnj$Year==2021)]
recdiscardcatchatage2021_num <- recdiscardcatchatage2021_num[recdiscardcatchatage2021_num$Length %in% 29:59,]

recdiscardcatchatage2022_prop <- left_join(discard_LF_propnj[,c(1,3)], alk2022propnj) %>% rename(Number = `2022`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2022_num <- recdiscardcatchatage2022_prop[,c(1,3:14)]
recdiscardcatchatage2022_num[,c(2:13)]<- recdiscardcatchatage2022_num[,c(2:13)]*totalcatchnj$Discard.Mortality[which(totalcatchnj$Year==2022)]
recdiscardcatchatage2022_num <- recdiscardcatchatage2022_num[recdiscardcatchatage2022_num$Length %in% 29:59,]

recdiscardcatchatage2023_prop <- left_join(discard_LF_propnj[,c(1,4)], alk2023propnj) %>% rename(Number = `2023`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2023_num <- recdiscardcatchatage2023_prop[,c(1,3:14)]
recdiscardcatchatage2023_num[,c(2:13)]<- recdiscardcatchatage2023_num[,c(2:13)]*totalcatchnj$Discard.Mortality[which(totalcatchnj$Year==2023)]
recdiscardcatchatage2023_num <- recdiscardcatchatage2023_num[recdiscardcatchatage2023_num$Length %in% 29:59,]

recdiscardcatchatage2024_prop <- left_join(discard_LF_propnj[,c(1,5)], alk2024propnj) %>% rename(Number = `2024`, Total.Count = rowsum) %>% mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0)
recdiscardcatchatage2024_num <- recdiscardcatchatage2024_prop[,c(1,3:14)]
recdiscardcatchatage2024_num[,c(2:13)]<- recdiscardcatchatage2024_num[,c(2:13)]*totalcatchnj$Discard.Mortality[which(totalcatchnj$Year==2024)]
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
  a <- LW.pars[y,2]
  b <- LW.pars[y,3]
  
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
  
  print(paste("total weight", 2000+y))
  print((sum(weights) + comm.in)/1000000)
    
  savehere <- function(x) {
    # Calculate the sample weight frequency by multiplying the numbers at length
    # by the weight at length
  WF <- W_LBin * LF.vec
  # Calculate what proportion of the sample weight is in each length bin
  WF <- WF/sum(WF, na.rm=T)
  
  # Calculate what proportion of the harvest weight is in each length bin
  HWL <- replace_na(WF * comm.in, 0)
  
  # Convert the harvest in weight by length bin to harvest in numbers by dividing
  # by the weight of one fish in that length bin
  HNL <- HWL/W_LBin
  
  # Calculate the CAA as above
  CAAL <- HNL * alk.in[,-1]
  CAA.tmp <- colSums(CAAL)
  c.flag <- ifelse(abs(sum(CAA.tmp) - sum(HNL)) < 1, 0, 1)
  
  
  # Calculate the WAA by converting the catch-at-age-and-length in numbers to
  # weight, using the average weight of each length bin, then dividing total
  # weight-at-age by total catch-at-age to get the average weight-at-age.
  WAAL <- CAAL *W_LBin
  WAA.tmp <- colSums(WAAL)
  WAA.tmp <- WAA.tmp/CAA.tmp
  WAA.tmp[which(is.na(WAA.tmp), arr.ind = TRUE)]<-0
  
  CAA.tmp <- cbind.data.frame(t(CAA.tmp), "Year"=y+2020, "CHECKSUM"=c.flag)
  CAA.comm <- rbind(CAA.comm, CAA.tmp)
  
  WAA.tmp <- cbind.data.frame(t(WAA.tmp), "Year"=y+2020, "CHECKSUM"=c.flag)
  WAA.comm <- rbind(WAA.comm, WAA.tmp)

  CAAL.comm[[y]]<-CAAL
  WAAL.comm[[y]]<-WAAL}
  
}
caa <- as.data.frame(caa)
mysum <- apply(caa, 1, sum)
prop <- apply(caa[,1:12], 2, function(x) x/mysum)
write.csv(caa, "CAA.csv")


write.csv(cbind(X1=c(0,0,0,0), bind_rows(waas))/1000, "waas.csv", row.names=F)

#####
write.csv(recharvestcatchatage2021nj, "./output/CAArecharvestnum2021.csv", row.names = FALSE)
write.csv(recharvestcatchatage2022nj, "./output/CAArecharvestnum2022.csv", row.names = FALSE)
write.csv(recharvestcatchatage2023nj, "./output/CAArecharvestnum2023.csv", row.names = FALSE)
write.csv(recharvestcatchatage2024nj, "./output/CAArecharvestnum2024.csv", row.names = FALSE)

write.csv(recdiscardcatchatage2021_num, "./output/CAArecdiscardnum2021.csv", row.names = FALSE)
write.csv(recdiscardcatchatage2022_num, "./output/CAArecdiscardnum2022.csv", row.names = FALSE)
write.csv(recdiscardcatchatage2023_num, "./output/CAArecdiscardnum2023.csv", row.names = FALSE)
write.csv(recdiscardcatchatage2024_num, "./output/CAArecdiscardnum2024.csv", row.names = FALSE)

write.csv(oldcomharvestcatchatage2021_numnj, "./output/oldCAAcomharvestnum2021.csv", row.names = FALSE)
write.csv(oldcomharvestcatchatage2022_numnj, "./output/oldCAAcomharvestnum2022.csv", row.names = FALSE)
write.csv(oldcomharvestcatchatage2023_numnj, "./output/oldCAAcomharvestnum2023.csv", row.names = FALSE)
#write.csv(oldcomharvestcatchatage2024_numnj, "./output/oldCAAcomharvestnum2024.csv", row.names = FALSE)

write.csv(CAAL.comm[[1]], "./output/CAAcomharvestnum2021.csv", row.names = FALSE)
write.csv(CAAL.comm[[2]], "./output/CAAcomharvestnum2022.csv", row.names = FALSE)
write.csv(CAAL.comm[[3]], "./output/CAAcomharvestnum2023.csv", row.names = FALSE)
write.csv(CAAL.comm[[4]], "./output/CAAcomharvestnum2024.csv", row.names = FALSE)

write.csv(WAAL.comm[[1]], "./output/WAAcomharvest2021.csv", row.names = FALSE)
write.csv(WAAL.comm[[2]], "./output/WAAcomharvest2022.csv", row.names = FALSE)
write.csv(WAAL.comm[[3]], "./output/WAAcomharvest2023.csv", row.names = FALSE)
write.csv(WAAL.comm[[4]], "./output/WAAcomharvest2024.csv", row.names = FALSE)

write.csv(CAA.comm, "./output/CAAcomharvestnum.csv", row.names = FALSE)
write.csv(WAA.comm, "./output/WAAcomharvestnum.csv", row.names = FALSE)

#How different is my old version from the new version based on Katie's results?
#In the old version my numbers of CAAL is about 25% lower than in Katie's version in 2021 and 2022, about 50% lower in 2023 and 2024
#So she's squeezing more individual fish in because she's thinking about the full size range whereas I was just using a mean, does that make sense?
#(CAAL.comm[[1]][-c(64,65),]-oldcomharvestcatchatage2021_numnj[,-1])/oldcomharvestcatchatage2021_numnj[,-1]
#(CAAL.comm[[2]][-c(64,65),]-oldcomharvestcatchatage2022_numnj[,-1])/oldcomharvestcatchatage2022_numnj[,-1]
#(CAAL.comm[[3]][-c(64,65),]-oldcomharvestcatchatage2023_numnj[,-1])/oldcomharvestcatchatage2023_numnj[,-1]
#(CAAL.comm[[4]][-c(64,65),]-oldcomharvestcatchatage2024_numnj[,-1])/oldcomharvestcatchatage2024_numnj[,-1]
