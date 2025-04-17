library(readxl)
library(FSA)
library(AquaticLifeHistory)
library(dplyr)
library(ggplot2)

#We are focused on the ageing structure question first
#(i.e., how well do paired samples agree across structures within a state,
#how similar is length-at-age for different structures within a state, 
#and how similar is length-at-age across states for the same structure), 
#but you should also be compiling the age-length data by year in order to do the age-length keys 
#once the ageing structure issue has been resolved.

setwd("C:\\Users\\jgorzo\\OneDrive - New Jersey Office of Information Technology\\Documents\\data\\")
#lines below won't work if file is open

#init <- getInitial(Length ~ SSasymp(Age, Asym, resp0, lrc), data = ny)
#Asym <- init[1]
#resp0 <- init[2]
#lrc <- init[3]
#fm1 <- nls(Length ~ SSasymp(Age, Asym, resp0, lrc), data = ny) 

ny_comm <- read_excel("2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="CommBioSamples", range = cell_rows(6:901))
ny_comm$state <- "NY"
ny_comm$mode <- "comm"
ny_rec <- read_excel("2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="RecBioSamples", range = cell_rows(6:110))
ny_rec$state <- "NY"
ny_rec$mode <- "rec"
nj_comm <- read_excel("Tautog Data Template 2025_NJDEP.xlsx", sheet="CommBioSamples", range = cell_rows(6:682))
nj_comm$Year <- format(nj_comm$Year, '%Y')
nj_comm$state <- "NJ"
nj_comm$mode <- "comm"
njny <- rbind(ny_comm, nj_comm[,names(ny_comm)], ny_rec)
njny$structure <- njny$`Ageing Structure`
njny$structure[njny$structure %in% c("opercular", "opec", "Operculum")] <- "operc"
njny$structure[njny$structure == "Otolith"] <- "oto"
njny$Length <- njny$`Total Length (cm)` #need this exact col name for AquaticLifeHistory pkg
njny$`Total Length (cm)` <- NULL
njny$`Ageing Structure` <- NULL
#write.csv(njny, "njny.csv", row.names=F) this would include LIS
njny <- njny[njny$Region!="LIS",]
njny$subregion <- "B"

njny <- njny[!is.na(njny$Age),]
tg <- njny[,c("Age", "Region", "state", "subregion", "structure", "Length")]

#dt <- tg %>%
#  dplyr::group_by(Age) %>%
#  dplyr::summarise(
#    mean = mean(Length),
#    lci = t.test(Length, conf.level = 0.95)$conf.int[1], not enough observations in each age...
#    uci = t.test(Length, conf.level = 0.95)$conf.int[2])
#dt

nj <- njny[njny$state=="NY" & njny$structure=="operc",] #& njny$structure=="oto"
p <- ggplot(nj, aes(x=as.factor(Age), y=Length)) + 
  geom_boxplot() + theme_classic(base_size = 20)
p

#pick back up here, try models with means 
summarized <- njny[!is.na(njny$structure) & !is.na(njny$Age),] %>% group_by(structure) %>% summarize(nrec = n())
summarized <- njny[!is.na(njny$structure) & !is.na(njny$Age),] %>% group_by(structure, Age) %>% summarize(m = mean(Length), n = n())
#ny <- summarized[!is.na(summarized$structure) &  summarized$structure=="oto" & summarized$subregion=="LIS",] #& summarized$subregion!="LIS" 
#plot(ny$Age, ny$m)
#f.starts <- vbStarts(m~Age,data=ny, methLinf="oldAge") 
#f.starts$K <- 0.3
#vbmod <- m ~ Linf * (1 - exp(-K * (Age - t0)))
#mymod <- nls(vbmod, data = ny, start = f.starts)
#plot(njny$Age, njny$Length)
#plot(njny[njny$structure=="both",]$Age, njny[njny$structure=="both",]$Length)
ny <- njny[njny$structure=="both" & !is.na(njny$structure),] # this is the data subset that performs best, only NY
ny <- njny[!is.na(njny$structure) &  njny$structure=="oto" & njny$state=="NJ",] # & njny$state=="NJ"
N.AgeLen <- nrow(ny)
Age <- ny$Age
Len <- ny$Length
f.starts <- vbStarts(Length~Age,data=ny, methLinf="oldAge") 
f.starts$K <- 0.3
f.starts$t0 <- 0
vbmod <- Length ~ Linf * (1 - exp(-K * (Age - t0)))
mymod <- nls(vbmod, data = ny, start = f.starts)

#converges for LIS, both
#for both across entire region, produces bad start value but fixed by t0 = 0
#separating out to NJ-NYB both (i.e. all of NY) doesn't converge

#&  njny$structure=="operc" njny$structure=="oto"
Estimate_Growth(data = njny[!is.na(njny$structure) &  njny$state=="NJ" & njny$structure=="oto",], models = "VB", Birth.Len = 0) #doesn't converge for all models
#Windows trick for writing to clipboard in table form: write.table(summarized, "clipboard-16384", sep = "\t", row.names = FALSE, quote = FALSE)
nj <- njny[njny$state=="NJ",] 
Estimate_Growth(data = nj[!is.na(nj$structure) &  nj$structure=="operc",], models = "VB", Birth.Len = 0) #doesn't converge for all models
Estimate_Growth(data = njny[!is.na(njny$structure) &  njny$structure=="oto" & njny$state=="NY",], models = "VB", Birth.Len = 0) #doesn't converge for all models
