library(readxl)
library(dplyr)
library(ggplot2)

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
njny <- njny[!is.na(njny$Age) & !is.na(njny$Length),]
tg <- njny[,c("Age", "Region", "state", "subregion", "structure", "Length", "Year")]
tg$TL_cm <- floor(tg$Length)
tg <- tg[tg$TL_cm >= 15 & tg$TL_cm <= 60,]
tg$TL_cm <- factor(tg$TL_cm, levels=15:60) #should this be 22:78 like DMV past update?
aldat <- mutate(tg, Age_plus = ifelse(Age>=12, 12, Age))
aldat$Age <- factor(aldat$Age, levels=2:22)
aldat$Age_plus <- factor(aldat$Age_plus, levels = 2:12)

operc <- aldat[aldat$structure=="operc",]
oto <- aldat[aldat$structure=="oto",]

#We need to do yearly keys
tt21 <- filter(aldat, Year == "2021")
tt22 <- filter(aldat, Year == "2022")
tt23 <- filter(aldat, Year == "2023")

#nj <- njny[njny$state=="NY" & njny$structure=="operc",] #& njny$structure=="oto"

tabyr <- function(dat) {
  tab <- table(dat$`TL_cm`, dat$Age_plus) #if you switch to full age you'll need to change Age_plus to Age
  tab1 <- matrix(tab, ncol = 11, dimnames= dimnames(tab)) #if you switch to full age you'll need to change the 11
  tab2 <- data.frame(tab1)
  df <- data.frame(names = row.names(tab2), tab2)
  colnames(df) <- c("", as.character(2:12)) #If you switch to full age you'll need to change the 12
  return(df)
}

tab21 <- tabyr(tt21)
tab22 <- tabyr(tt22)
tab23 <- tabyr(tt23)
#write_xlsx(list(ALK_2021_unfilled = tab21, ALK_2022_unfilled = tab22, ALK_2023_unfilled=tab23), "./Output/DMV_ALK_unfilled.xlsx")

#Age as a proportion of all individuals of that size in a year
proptab21 <- tab21[,-1]/rowSums(tab21[,-1])
proptab22 <- tab22[,-1]/rowSums(tab22[,-1])
proptab23 <- tab23[,-1]/rowSums(tab23[,-1])

# We have several rows (length bins) that have no age samples.
# These are gaps that need to be filled.
#Here they are for each year
apply(array(as.numeric(sapply(list(tab21, tab22, tab23), FUN = function(x) cbind(x[,1],rowSums(x[,-1])), simplify = "array")),c(57,2,3), dimnames = list(NULL, NULL, c("2021", "2022", "2023"))), MARGIN = 3, FUN = function(x) x[,1][which(x[,2]==0)])
#You can see that there are a usually one or a few missing at the small extreme, many at the large extreme, and then one or two that are "internal." In 2022 the internal is 27cm, in 2023 the internal is 55cm
