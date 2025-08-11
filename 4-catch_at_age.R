#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## This script calculates total catch in numbers of fish
# the length frequencies of different components of catch
# and combines those with the filled-in ALKs to calculate catch-at-age...
# then weight-at-age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
root <- "C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents"
#root <- "/media/jess/9CE61C02E61BDB7A/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents"
#on Ubuntu, you have to have this open in files/ssd to have it mounted...
library(tidyverse)
library(readxl)
library(ggplot2)

#INPUTS USED#############################
life <- read_xlsx(file.path(root, "data/tog/Tautog Data Template 2025_NJDEP.xlsx"), sheet = "LifeHistory", skip = 6)  
life24 <- read_xlsx(file.path(root, "data/tog/Tautog Data Template 2025_NJDEP_2024-update.xlsx"), sheet = "LifeHistory", skip = 6) 

#Load ALKs
indir <- "output/tog/alk/filled/opercboth"
indir <- file.path(root, indir)
alk2021numnj <- read.csv(file.path(indir, "NJNYB-ALK_2021_filled.csv"))
alk2022numnj <- read.csv(file.path(indir, "NJNYB-ALK_2022_filled.csv"))
alk2023numnj <- read.csv(file.path(indir, "NJNYB-ALK_2023_filled.csv"))
alk2024numnj <- read.csv(file.path(indir, "NJNYB-ALK_2024_filled.csv"))

#Load recreational catch and discard
totalcatchnj <- read.csv(file.path(root, "data/tog/rec/Tautog_MRIP_TotalCatch_2021-2024_NJNYB.csv"), header = TRUE)

#Load commercial catch data
commcatchnj <- read_xlsx(file.path(root, "data/tog/Regional_Comm_landings_MT_07.18.25.xlsx"), sheet = "MT")[,c("Year","NYB-NJ")]

#Length Frequencies for Recreational Catch and Discard, modified from discard_LFnj.R from Samarah Nehemiah for LIS
harvest_LFnj <- read.csv(file.path(root, "output/tog/harvest_LF.csv"))
discard_LFnj <- read.csv(file.path(root, "output/tog/discard_LF.csv"))[,2:6]
#########################################
#Find weight of recreationally harvested fish.
#There are small sample sizes for commercially harvested, so we'll use rec weights to infer comm weights.

names(life)[names(life)=="Year"] <- "Date"
life$Year <- as.integer(format(life$Date, '%Y'))
life24$`Weight (g)` <- life24$`Weight (g)` * 1000
#life24$Total.Length..cm. <- life24$Total.Length..cm. / 10
life24$`Total Length (cm)` <- life24$`Total Length (cm)` / 10

lifehistory <- rbind(life[,names(life24)], life24) %>%
  #rename("Weight" = "Weight..g.") %>% 
  rename("Weight" = "Weight (g)") %>% 
  #mutate(Length = floor(Total.Length..cm.)) %>% 
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

names(commcatchnj)[names(commcatchnj)=="NYB-NJ"] <- "Comm" #metric tons
commcatchnj$Comm <- commcatchnj$Comm*1000000
commcatchyrsumnj <- commcatchnj %>% left_join(weights_meansnj) %>% mutate(CommCatchNumFish = Comm/MeanWeight)

totalcatchnj <- totalcatchnj %>% 
  mutate(DiscardMortality = Released.B2*0.025) %>% 
  mutate(TotalRecCatch = Harvest.A.B1+DiscardMortality)
totalcatchnj <- left_join(totalcatchnj, commcatchyrsumnj) %>% mutate(TotalCatch = TotalRecCatch + CommCatchNumFish)

#convert to proportions
alks <- list(alk2021numnj, alk2022numnj, alk2023numnj, alk2024numnj)
#alks <- lapply(alks, function(y) {
#  y <- y %>% select(2:13) 
#})
alkprops <- lapply(alks, function(y) {
  y <- y %>% mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) %>% #add row sum
    mutate(across(2:12, .fns= function(x){x/rowsum})) %>% replace(is.na(.), 0) %>%
    rename("Length" = "length")
})

names(harvest_LFnj)<- c("Length", "2021", "2022", "2023", "2024")
names(discard_LFnj)<- c("Length", "2021", "2022", "2023", "2024")
discard_LF_propnj <- discard_LFnj %>% mutate(across(`2021`:`2024`, .fns = function(x){x/sum(x,na.rm = TRUE)}))
harvest_LF_propnj <- harvest_LFnj %>% mutate(across(`2021`:`2024`, .fns = function(x){x/sum(x, na.rm = TRUE)}))

recharvestCAA <- Map(function(x, y) {
  rechar_year <- left_join(harvest_LFnj[,c(1, x+1)], y) %>% 
    rename(Number = as.character(2020+x), Total.Count = rowsum) %>%
    mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% 
    select(-c("Number", "Total.Count"))
}, 1:4, alkprops)

alklist <- list(alkprops[[1]][,-13], alkprops[[2]][,-13], alkprops[[3]][,-13], alkprops[[4]][,-13]) 

recharvestCAA <- Map(function(x, y) {
  rechar_year <- left_join(harvest_LFnj[,c(1, x+1)], y) %>% 
    rename(Number = as.character(2020+x), Total.Count = rowsum) %>%
    mutate(across(paste0("X", 2:12), .fns = function(x){x*Number})) %>% replace(is.na(.), 0) %>% 
    select(-c("Number", "Total.Count"))
}, 1:4, alkprops)

##Recreational Discard Catch-at-Age
recdiscardCAA <- lapply(1:4, function(a) {
  discardprops <- apply(alkprops[[a]][,2:12], 2, function(x) x * discard_LF_propnj[, a+1])
  yr <- 2020+a
  discardnums <- as.data.frame(discardprops * totalcatchnj$DiscardMortality[totalcatchnj$Year==yr])
  discardnums$Length <-  discard_LF_propnj[, 1]
  discardnums <- discardnums[discardnums$Length %in% 29:60,]
  discardnums <- discardnums[,c(12,1:11)]
})

caa2 <- list(
  recdiscardCAA[[1]][,2:12] + recharvestCAA[[1]][,2:12], #/1000,
  recdiscardCAA[[2]][,2:12] + recharvestCAA[[2]][,2:12], #/1000,
  recdiscardCAA[[3]][,2:12] + recharvestCAA[[3]][,2:12], #/1000,
  recdiscardCAA[[4]][,2:12] + recharvestCAA[[4]][,2:12] #/1000
)

caa <- cbind(X1=c(0,0,0,0), rbind(
  apply(recdiscardCAA[[1]][,2:12] + recharvestCAA[[1]][,2:12], 2, sum) /1000,
  apply(recdiscardCAA[[2]][,2:12] + recharvestCAA[[2]][,2:12], 2, sum) /1000,
  apply(recdiscardCAA[[3]][,2:12] + recharvestCAA[[3]][,2:12], 2, sum) /1000,
  apply(recdiscardCAA[[4]][,2:12] + recharvestCAA[[4]][,2:12], 2, sum) /1000
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
write.csv(caa, file.path(root, "caa.csv"))
write.csv(waa, file.path(root, "waa.csv"))
