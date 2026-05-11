#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Length Frequencies --------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(dplyr)
library(readxl)
library(ggplot2)
root <- Sys.getenv("FILEPATH")
#INPUTS: read in data for discard LF#########
mrip9 <- read.csv(file.path(root, "data/tog/rec/Tautog_MRIP_Type9_lengths_2021-24.csv"), header=TRUE)
####################
mrip9<-subset(mrip9, REGION=="NJNYB") %>% rename("Length_cm" = "LENGTH.ROUNDED.DOWN.TO.NEAREST.CM")
discards <- mrip9[,c("YEAR", "Length_cm")]

lenfreqfun <- function(lendat) {
  yr <- lendat$YEAR[1]
  lenfreq <- lendat %>%
    group_by(Length_cm) %>%
    dplyr::summarise(freq = n()) %>%
    dplyr::select(Length_cm,freq) %>%
    arrange(Length_cm) %>%
    rename(!!(str_c(yr)) := "freq")
}

discard <- discards %>% group_by(YEAR) %>% group_split %>% 
  map(~lenfreqfun(.x)) %>% 
  reduce(full_join) %>% complete(Length_cm=full_seq(Length_cm, 1)) %>%
  replace(is.na(.), 0) %>% group_by(Length_cm) %>% summarise_all(sum)

write.csv(discard,file.path(root, "output/tog/discard_mripLF.csv"))

#plot out frequencies
discardexpand<- pivot_longer(discard, cols = c('2021','2022','2023','2024'), names_to = "Year", values_to = "Count")
discardexpand<-as.data.frame(discardexpand)

#jpeg("LF_ALST9VAS.jpeg",res=800, height = 6,width=8,units="in")
ggplot(discardexpand)+
  geom_col(aes(x=Length.CM,y=Count),col="black",fill="darkgrey")+
  facet_wrap(~Year)+theme_bw()
#dev.off()
