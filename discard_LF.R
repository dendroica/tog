#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Length Frequencies --------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(dplyr)
library(openxlsx)
library(readxl)
library(ggplot2)

#clear data
rm(list=ls()) 


#read in data for discard LF

#### american littoral society #####
mrip9 <- read.csv("C:\\Users\\jgorzo\\OneDrive - New Jersey Office of Information Technology\\Documents/data/tog/rec/RecData\\Tautog_MRIP_Type9_lengths_2021-24.csv", header=TRUE)
#mrip9<-read.csv("Data/MRIP/Tautog_MRIP_Type9_lengths_2021-24.csv",header=TRUE)
head(mrip9)
mrip9<-subset(mrip9, REGION=="NJNYB")

als <- read_excel("C:\\Users\\jgorzo\\OneDrive - New Jersey Office of Information Technology\\Documents/data/tog/rec\\ALS_Tautog_2021-2024.xlsx", col_names = TRUE)
#als<- read_excel("Data/MRIP/ALS_Tautog_2021-2024.xlsx", sheet ="tautog tags",col_names = TRUE)#american littoral society
head(als)  

str(als)

als<-subset(als,Region=="NJNYB")
#View(als)

#length is reported by each 0.25 inch
#look at distribution of 0.25 and 0.75
ggplot()+
  geom_histogram(aes(x=(als$Length_IN)),col="black",binwidth=0.25,position="identity")+
  theme_bw()+
  labs(x="Length IN")

## length bins seem even distributed across 0.25,0.5,0.75,inch bins so will just convert straight to CM

als$Length_cm<-als$Length_IN*2.54 #conver inches to cm
als$Length_cm<-floor(als$Length_cm)


lenfreqfun <- function(lendat,year) {
  
  lenrange <- data.frame(Length_cm = seq(trunc(min(na.omit(als$Length_cm))), trunc(max(na.omit(als$Length_cm))), by=1))
  
  lenfreq <- lendat %>% filter(year== year) %>%
    group_by(Length_cm) %>%
    dplyr::summarise(freq = n()) %>%
    left_join(lenrange, .) %>%
    replace_na(., list(freq = 0)) %>%
    dplyr::select(Length_cm,freq) %>%
    arrange(Length_cm)
}

years<-as.numeric(unique(als$Year))
years<-sort(years) #put in ascending order
lengths <- seq(min(na.omit(als$Length_cm)), max(na.omit(als$Length_cm)), by=1) #CM


#create matrix for each year and length bin
ALS_LF<-matrix(NA, nrow=length(lengths),ncol = (length(years)+1)) %>% data.frame()
names(ALS_LF)[1]<- "Length.CM"
ALS_LF$Length.CM <-lengths

#fill in matrix with observed length frequences
for(i in 1:length(years)){
  ALS_LF[,i+1] <- lenfreqfun(lendat= subset(als, Year==years[i]), year=years[i]) %>% .$freq
  names(ALS_LF)[i+1]<- paste0(years[i])
}

#do the same for MRIP discard (Type 9)

mrip9<-rename(mrip9,c('Length_cm'='LENGTH.ROUNDED.DOWN.TO.NEAREST.CM'))
colnames(mrip9)

lenfreqfun <- function(lendat,year) {
  
  lenrange <- data.frame(Length_cm = seq(trunc(min(na.omit(mrip9$Length_cm))), trunc(max(na.omit(mrip9$Length_cm))), by=1))
  
  lenfreq <- lendat %>% filter(year== year) %>%
    group_by(Length_cm) %>%
    dplyr::summarise(freq = n()) %>%
    left_join(lenrange, .) %>%
    replace_na(., list(freq = 0)) %>%
    dplyr::select(Length_cm,freq) %>%
    arrange(Length_cm)
}

years<-as.numeric(unique(mrip9$YEAR))
years<-sort(years) #put in ascending order
lengths <- seq(min(na.omit(mrip9$Length_cm)), max(na.omit(mrip9$Length_cm)), by=1) #CM


#create matrix for each year and length bin
MRIP9_LF<-matrix(NA, nrow=length(lengths),ncol = (length(years)+1)) %>% data.frame()
names(MRIP9_LF)[1]<- "Length.CM"
MRIP9_LF$Length.CM <-lengths

#fill in matrix with observed length frequences
for(i in 1:length(years)){
  MRIP9_LF[,i+1] <- lenfreqfun(lendat= subset(mrip9, YEAR==years[i]), year=years[i]) %>% .$freq
  names(MRIP9_LF)[i+1]<- paste0(years[i])
}

discard<- bind_rows(ALS_LF, MRIP9_LF) %>% 
  # evaluate following calls for each value in the rowname column
  group_by(Length.CM) %>% 
  # add all non-grouping variables
  summarise_all(sum)

#ALS_LF; MRIP9_LF; VAS_LF


#save LF
write.csv(ALS_LF,"ALS_LF.csv")
write.csv(MRIP9_LF,"MRIP9_LF.csv")
#write.csv(VAS_LF,"VAS_LF.csv")
write.csv(discard,"ALS+T9+VAS_LF.csv")




#plot out frequencies

discardexpand<- pivot_longer(discard, cols = c('2021','2022','2023','2024'), names_to = "Year", values_to = "Count")
discardexpand<-as.data.frame(discardexpand)

jpeg("LF_ALST9VAS.jpeg",res=800, height = 6,width=8,units="in")
ggplot(discardexpand)+
  geom_col(aes(x=Length.CM,y=Count),col="black",fill="darkgrey")+
  facet_wrap(~Year)+theme_bw()
dev.off()
