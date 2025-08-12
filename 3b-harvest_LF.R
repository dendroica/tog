# Tautock Stock Assessment
# Length Freq for MRIP harvest

# packages
library(ggplot2)
options(scipen=999)
library(tidyverse)
library(readxl)

#INPUT####################################
MRIP_har <- read.csv("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/data/tog/rec/Tautog_MRIP_AB1_LFs_2021-2024_NJNYB.csv", header=TRUE)
###################
# check it
head(MRIP_har)
str(MRIP_har)
MRIP_har$Year <- as.factor(MRIP_har$Year)
MRIP_har$Length <- as.numeric(MRIP_har$Length)

# take a look at it
ggplot(MRIP_har, aes(x=Length, y=Number)) +
  geom_bar(stat="identity") +
  #geom_errorbar(aes(ymin=Number-(Number*(PSE/100)), ymax=Number+(Number*(PSE/100)))) +
  theme_classic() +
  facet_wrap(~Year, ncol=2) +
  scale_x_continuous(breaks=c(15,20,25,30,35,40,45,50,55,60,65)) +
  ylab("Count of Fish") +
  xlab("Length (cm)") +
  ggtitle("MARI Recreational Harvest Length Frequencies") +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5))
ggsave("MARI_Recreational_Harvest_LengthFreq.png")


#format into table.  Length in one column, data for each year in other columns.  
max(MRIP_har$Length)
# max length of 64, ALK goes to 60 so group all the 60+s together


# make dataframe with all lengths represented, easier for putting into Excel sheet (if used)
lengths <- seq(29, 60)
MARI_har_lfs <- data.frame(lengths) 
names(MARI_har_lfs) <- "Length"

Yr_List <- unique(MRIP_har$Year)

 for (i in Yr_List) {
   Yr_All <- MRIP_har[MRIP_har$Year == i,] #c(1,8,9)
   Yr_L60 <- Yr_All[Yr_All$Length < 60,]
   names(Yr_L60)[5] <- i
   Yr_i <- Yr_L60[,c(4:5)]
   MARI_har_lfs <- merge(MARI_har_lfs, Yr_i, all.x=TRUE)
   
   Yr_max <- max(Yr_All$Length)
   if (Yr_max >= 60) {
     Yr_G60 <- Yr_All[Yr_All$Length >= 60,]
     Yr_60 <- sum(Yr_G60$Number)
     MARI_har_lfs[MARI_har_lfs$Length==60,i] <- Yr_60
   }
 }

write.csv(MARI_har_lfs, "C:/Users/galax/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/harvest_LF.csv", row.names=FALSE)


#####################################################################################
#####################################################################################