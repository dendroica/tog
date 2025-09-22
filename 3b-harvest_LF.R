# Tautock Stock Assessment
# Length Freq for MRIP harvest
# discard script does all lengths- mod here to match?
# packages
library(ggplot2)
library(tidyverse)
library(readxl)
options(scipen=999)

root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)

#INPUT####################################
MRIP_har <- read.csv(file.path(root, "/data/tog/rec/Tautog_MRIP_AB1_LFs_2021-2024_NJNYB.csv"), header=TRUE)
###################
# check it
head(MRIP_har)
str(MRIP_har)
MRIP_har$Length <- as.numeric(MRIP_har$Length)

min_len <- 17

# take a look at it
ggplot(MRIP_har, aes(x=Length, y=Number)) +
  geom_bar(stat="identity") +
  #geom_errorbar(aes(ymin=Number-(Number*(PSE/100)), ymax=Number+(Number*(PSE/100)))) +
  theme_classic() +
  facet_wrap(~as.factor(Year), ncol=2) +
  scale_x_continuous(breaks=c(15,20,25,30,35,40,45,50,55,60,65)) +
  ylab("Count of Fish") +
  xlab("Length (cm)") +
  ggtitle("MARI Recreational Harvest Length Frequencies") +
  theme(plot.title = element_text(size=16, face="bold", hjust=0.5))
ggsave("MARI_Recreational_Harvest_LengthFreq.png")

MRIP_har <- MRIP_har %>% select(Year, Length, Number)
MRIPG60 <- MRIP_har %>% filter(Length >= 60) %>% #should I do this with smaller length bins??
  group_by(Year) %>% summarise(Number = sum(Number)) %>% mutate(Length = 60) %>%
  bind_rows(data.frame(Year = 2022, Number = 0, Length =60))
MRIP_har <- MRIP_har %>% #bind_rows(MRIP_har, MRIPG60) %>% 
  filter(Length >= min_len & Length <= 60) %>% pivot_wider(names_from=Year, values_from=Number) %>%
  complete(Length=full_seq(Length, 1)) %>% replace(is.na(.), 0)

lastrow <- data.frame(c(min_len,60), c(0,0), c(0,0), c(0,0), c(0,0))
names(lastrow) <- names(MRIP_har)
MRIP_har <- bind_rows(MRIP_har, lastrow)
MRIP_har <- MRIP_har[order(MRIP_har$Length),]
write.csv(MRIP_har, file.path(root, "output/tog/all/harvest_LF.csv"), row.names=FALSE)
