#code experiment: not used in results and DO NOT USE

library(readxl)
library(dplyr)
library(ggplot2)

setwd("C:\\Users\\galax\\OneDrive - New Jersey Office of Information Technology\\Documents") #jgorzo galax
#setwd("/media/jess/9CE61C02E61BDB7A/Users/galax/OneDrive - New Jersey Office of Information Technology/Documents")
#on Ubuntu, you have to have this open in files/ssd to have it mounted...

#Using CommBioSamples and RecBioSamples to compute age-length key (ALK)
ny_comm <- read_excel("./data/tog/2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="CommBioSamples", range = cell_rows(6:901))
ny_comm24 <- read_excel("./data/tog/2025SA_NY_Tautog Data 2024-1.xlsx", sheet="CommBioSamples", range = cell_rows(6:797)) 
ny_comm <- bind_rows(ny_comm, ny_comm24) %>% mutate(mode="comm", Year = as.character(Year))
ny_rec <- read_excel("./data/tog/2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="RecBioSamples", range = cell_rows(6:110)) %>% 
  mutate(mode="rec", Year = as.character(Year))
ny <- bind_rows(ny_comm, ny_rec) %>% mutate(state="NY")

nj_comm <- read_excel("./data/tog/Tautog Data Template 2025_NJDEP.xlsx", sheet="CommBioSamples", range = cell_rows(6:682)) %>% mutate(Year = format(Year, '%Y'))
nj_comm24 <- read_excel("./data/tog/Tautog Data Template 2025_NJDEP_2024data.xlsx", sheet="CommBioSamples", range = cell_rows(6:239)) %>% mutate(Year = as.character(Year))
nj_comm <- bind_rows(nj_comm[,which(names(nj_comm) %in% names(nj_comm24))], nj_comm24[,which(names(nj_comm24) %in% names(nj_comm))]) %>%
  mutate(state="NJ", mode="comm")

njny <- bind_rows(ny, nj_comm[,names(ny)])
njny$structure <- njny$`Ageing Structure`
njny$structure[njny$structure %in% c("opercular", "opec", "Operculum")] <- "operc"
njny$structure[njny$structure == "Otolith"] <- "oto"
njny$Length <- njny$`Total Length (cm)` #need this exact col name for AquaticLifeHistory pkg
njny$`Total Length (cm)` <- NULL
njny$`Ageing Structure` <- NULL
#write.csv(njny, "njny.csv", row.names=F) this would include LIS
njny <- njny[!is.na(njny$Age) & !is.na(njny$Length) & njny$Age < 23,] #cutoff from looking at data, getting rid of 9999 and 99999 vals

njny$subregion <- "B"
njny[njny$Region=="LIS",]$subregion <- "LIS"
tg <- njny[,c("Age", "Region", "state", "subregion", "structure", "Length", "Year")]
tg$tl_cm <- floor(tg$Length)
##min(tg[tg$subregion=="B" & tg$structure=="operc",]$tl_cm) = 4
##max(tg[tg$subregion=="B" & tg$structure=="operc",]$tl_cm) = 83
##sort(unique(tg[tg$subregion=="B" & tg$structure=="operc",]$tl_cm)) 29 is next largest length incl rec data

#The previous assessment bounds for NJ-NYB were 15-60cm and MA-RI uses 14-60cm
#alk <- tg[tg$tl_cm >= 29 & tg$tl_cm < 60,]

#This final bounding was ad-hoc from filling the ALK. The gaps left to fill...
#...by the end of this script were 63cm onward, and 63cm does not occur...
#...in any annual raw (unfilled) data for NJ-NYB. 
#That and the larger gaps that were left after filling from all regions...
#...and at least MA-RI doesn't even go that high. So that felt like a reasonable cutoff...
#...and the highest the rec data goes below 63cm is 60cm
alk <- tg[tg$tl_cm >= 29 & tg$tl_cm < 61,] 

#alk$TL_cm <- factor(alk$tl_cm, levels=29:59) #should this be 29 - 83 now to match the rec data?
alk$TL_cm <- factor(alk$tl_cm, levels=29:60)
aldat <- mutate(alk, Age_plus = ifelse(Age>=12, 12, Age))
#aldat$age <- factor(aldat$Age, levels=2:15)
aldat$age <- factor(aldat$Age, levels=2:17)
aldat$Age_plus <- factor(aldat$Age_plus, levels = 2:12)
#aldat$Age_plus <- factor(aldat$Age_plus, levels = 1:12)

#operc <- aldat[aldat$structure=="operc" | aldat$structure=="both",] #| aldat$structure=="both"
#oto <- aldat[aldat$structure=="oto",]
#aldata <- operc[operc$subregion=="B",] #how to...
aldata <- aldat[aldat$subregion=="B",] #...revisit with new lengths/ages...

#We need to do yearly keys
tt21 <- filter(aldata, Year == "2021")
tt22 <- filter(aldata, Year == "2022")
tt23 <- filter(aldata, Year == "2023")
tt24 <- filter(aldata, Year == "2024")

#default_palette = scales::hue_pal()(14)
#names(default_palette) <- 2:15

#p <- ggplot(operc, aes(x=tl_cm, color=age, fill=age)) + 
#  labs(title="Age distribution per length bin (2cm)",x="Length", y = "Count") +
#  geom_histogram(alpha=0.5, binwidth=2) + scale_fill_manual(values = default_palette, drop=FALSE) +
#  facet_grid(cols=vars(Year))

tabyr <- function(dat) {
  tab <- table(dat$`TL_cm`, dat$Age_plus) #if you switch to full age you'll need to change Age_plus to Age
  tab1 <- matrix(tab, ncol = 11, dimnames= dimnames(tab)) #if you switch to full age you'll need to change the 11
  #tab1 <- matrix(tab, ncol = 12, dimnames= dimnames(tab))
  tab2 <- data.frame(tab1)
  df <- data.frame(names = row.names(tab2), tab2)
  colnames(df) <- c("length", as.character(2:12)) #If you switch to full age you'll need to change the 12
  return(df)
}

tab21 <- tabyr(tt21)
tab22 <- tabyr(tt22)
tab23 <- tabyr(tt23)
tab24 <- tabyr(tt24)
#write_xlsx(list(ALK_2021_unfilled = tab21, ALK_2022_unfilled = tab22, ALK_2023_unfilled=tab23), "./Output/DMV_ALK_unfilled.xlsx")

#Age as a proportion of all individuals of that size in a year
proptab21 <- tab21[,-1]/rowSums(tab21[,-1])
proptab22 <- tab22[,-1]/rowSums(tab22[,-1])
proptab23 <- tab23[,-1]/rowSums(tab23[,-1])
proptab24 <- tab24[,-1]/rowSums(tab24[,-1])
# We have several rows (length bins) that have no age samples.
# These are gaps that need to be filled.
#Here they are for each year


#write.csv(tab21[,-1], "NJNYB-ALK_2021_unfilled.csv")
#write.csv(tab22[,-1], "NJNYB-ALK_2022_unfilled.csv")
#write.csv(tab23[,-1], "NJNYB-ALK_2023_unfilled.csv")
#write.csv(tab24[,-1], "NJNYB-ALK_2024_unfilled.csv")

#write.csv(proptab21, "NJNYB-ALK_2021_unfilled-prop.csv")
#write.csv(proptab22, "NJNYB-ALK_2022_unfilled-prop.csv")
#write.csv(proptab23, "NJNYB-ALK_2023_unfilled-prop.csv")

###########STEP 1: fill with otolith data
alks <- list(tab21, tab22, tab23, tab24)

mrip9 <- read.csv("./data/tog/rec/Tautog_MRIP_Type9_lengths_2021-24.csv")
mrip9 <- mrip9[mrip9$REGION=="NJNYB",]
als <- read_excel("./data/tog/rec/ALS_Tautog_2021-2024.xlsx")
als <- als[als$Region=="NJNYB",]
als$Length_cm<-als$Length_IN*2.54 #convert inches to cm
als$Length_cm<-floor(als$Length_cm)
MRIP_har <- read.csv("./data/tog/rec/Tautog_MRIP_AB1_LFs_2021-2024_NJNYB.csv", header=TRUE)

#min(unique(c(mrip9$LENGTH.ROUNDED.DOWN.TO.NEAREST.CM, als$Length_cm, MRIP_har$Length))) = 12
#max(unique(c(mrip9$LENGTH.ROUNDED.DOWN.TO.NEAREST.CM, als$Length_cm, MRIP_har$Length))) = 83
#sort(unique(c(mrip9$LENGTH.ROUNDED.DOWN.TO.NEAREST.CM, als$Length_cm, MRIP_har$Length)))

checkgaps <- function(alk) {
  gaps <- lapply(alk, FUN = function(x) as.integer(names(rowSums(x[,2:12]))[which(rowSums(x[,2:12])==0)]))
  names(gaps) <- c(2021:2024)
  catchmatch <- Map(function(x,y) {
    x %in% unique(c(mrip9$LENGTH.ROUNDED.DOWN.TO.NEAREST.CM[mrip9$YEAR==as.integer(y)], als$Length_cm[als$Year==as.integer(y)], MRIP_har$Length[MRIP_har$Year==as.integer(y)]))
  }, gaps, names(gaps))
  tofill <- Map(function(x,y) {x[y]},
                gaps, catchmatch)
return(list(gaps, tofill))}

gapstofill <- checkgaps(alks)

#You can see that there are a usually one or a few missing at the small extreme, many at the large extreme, and then one or two that are "internal." In 2022 the internal is 27cm, in 2023 the internal is 55cm

#otofill <- Map(function(x,y) {
#  check <- oto[oto$Year==y & oto$Length %in% x & oto$subregion=="B",]
#  if (nrow(check) > 0) {
#    otofill <- tabyr(check)
#    otofill <- otofill[otofill$length %in% check$Length,]
    #otofill$year <- y
#  } else {otofill <- data.frame()}
#  return(otofill)
#}, gapstofill[[2]], names(gapstofill[[2]]))

#otofilled <- Map(function(x,y) {
#  if(nrow(y) > 0) {
#    x[x$length %in% y$length,] <- y
#    }
#  return(x)
#}, alks, otofill)

otofilled <- Map(
  function(x,y) {
    x$year <- y
    return(x)},
  alks, c(2021:2024))

otofilled <- lapply(otofilled, function(y) {
  y$length <- as.integer(y$length)
  y <- y %>% mutate(rowsum = rowSums(.[2:12], na.rm = TRUE)) #add row sum
  datao <- max(y$length[y$rowsum > 0]) #last row with values
  if (y[y$length==datao,12] == y[y$length==datao,"rowsum"] & datao < max(y$length)) {
    y[y$length %in% (datao+1):max(y$length),12] <- y[y$length==datao,12]
  }
return(y)})
########################STEP 2: fill 
lis_opercboth <- operc[operc$subregion=="LIS",]
lis_oto <- oto[oto$subregion=="LIS",]

gapstofill <- checkgaps(otofilled)

lis21 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet="LIS_2021")
lis22 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet="LIS_2022")
lis23 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet="LIS_2023")
lis24 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet="LIS_2024")
LIS_unfill <- list(lis21, lis22, lis23, lis24)
names(LIS_unfill) <- c("2021", "2022", "2023", "2024")

dmv21 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet="ALK_2021_unfilled")
dmv22 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet="ALK_2022_unfilled")
dmv23 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet="ALK_2023_unfilled")
dmv24 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet="ALK_2024_unfilled")
dmv_unfill <- list(dmv21, dmv22, dmv23, dmv24)
names(dmv_unfill) <- c("2021", "2022", "2023", "2024")

gapstofill <- checkgaps(otofilled)[[2]]

nearfill <- Map(function(x,y) {
  dmv <- dmv_unfill[[as.character(y$year[1])]]
  lis <- LIS_unfill[[as.character(y$year[1])]][,c(1,3:13)]
  gaps <- which(diff(x) > 1) + 1 # Identify the index of the element after the gap
  missing_numbers <- x[gaps] - 1 # Find the missing number(s)
  lastrowfilled <- missing_numbers[length(missing_numbers)]
  
  #y$length <- as.integer(y$length)
  #print(str(x))
  #print(str(y))
  
  for(i in 1:length(x)) {
    if(x[i] == min(y$length)) { #if the gap to fill is the smallest bin in the ALK...
      if(x[i] + 1 != x[i+1]) { #if the next greater length bin isn't empty...
        y[y$length==x[i],c(2:12)] <- y[y$length==x[i]+1,c(2:12)] #...fill from below
      } else if (sum(dmv[dmv[,1]==x[i],c(2:12)]) > 0) { #e.g. need to fill 29 for 2021, 2023
          y[y$length==x[i],c(2:12)] <- dmv[dmv[,1]==x[i],c(2:12)] #if the next greatest length bin from the smallest is empty, fill from DMV
        } else {
          y[y$length==x[i],c(2:12)] <- lis[lis[,1]==x[i],c(2:12)]
        }
    } else if(x[i] == max(y$length)) { #if the gap to fill is the largest bin in the ALK...
      if(x[i] - 1 != x[i-1] & sum(y[y$length==x[i]-1, c(2:12)]) > 0) { #if the next smallest length bin doesn't need filling and has values...
        y[y$length==x[i],c(2:12)] <- y[y$length==x[i]-1,c(2:12)] #...fill from above
      } else if (y[y$length==max(y$length[y$rowsum > 0]),12] == y[y$length==max(y$length[y$rowsum > 0]),"rowsum"]) { #if the closest length bin with values are all in 12, fill down
        y[y$length==x[i],c(2:12)] <- y[y$length==max(y$length[y$rowsum > 0]),c(2:12)]
      } else {
        if (sum(dmv[dmv[,1]==x[i],c(2:12)]) > 0) {
          y[y$length==x[i],c(2:12)] <- dmv[dmv[,1]==x[i],c(2:12)]
        } else {
          y[y$length==x[i],c(2:12)] <- lis[lis[,1]==x[i],c(2:12)]
        }
      }
    #} else if (all(x[i]:max(y$length) %in% x)) { #look for a run of 0 values at the end of the key
    #  if (sum(y[y$length==lastrowfilled,c(2:11)]) == 0) {
    #    y[y$length==x[i],c(2:12)] <- y[y$length==lastrowfilled,c(2:12)]
    #  } else {
    #    if(sum(lis[lis[,1]==x[i],c(2:12)]) == 0) {
    #      y[y$length==x[i],c(2:12)] <- dmv[dmv[,1]==x[i],c(2:12)]
    #    } else {
    #      y[y$length==x[i],c(2:12)] <- lis[lis[,1]==x[i],c(2:12)]
    #    }
    #  }
    #} else if (all(x[i]:(max(y$length)-1) %in% x) & sum(y[y$length==max(y$length),2:12]) == 0) { #handle the same as above if the last row is empty...
    #  if (sum(y[y$length==lastrowfilled,c(2:11)]) == 0) {
    #    y[y$length==x[i],c(2:12)] <- y[y$length==lastrowfilled,c(2:12)]
    #    if(i == length(x)) { #if you're done filling the key after this iteration, fill last row with 12+
    #      y[y$length==max(y$length),c(2:12)] <- y[y$length==lastrowfilled,c(2:12)]
    #    }
    #  } else {
    #    if(sum(lis[lis[,1]==x[i],c(2:12)]) == 0) {
    #      y[y$length==x[i],c(2:12)] <- dmv[dmv[,1]==x[i],c(2:12)]
    #    } else {
    #      y[y$length==x[i],c(2:12)] <- lis[lis[,1]==x[i],c(2:12)]
    #    }
    #  }
    } else if (i == length(x)) { #if it's the last bin to be filled and it's not the largest length bin in the key...
      if(x[i] - 1 == x[i-1]) { #if the bin above it is empty...
        y[y$length==x[i],c(2:12)] <- y[y$length==x[i]+1,c(2:12)] #...fill with the bin below (this logic may have been specific to this data...)
      } else {
        y[y$length==x[i],c(2:12)] <- y[y$length==x[i]+1,c(2:12)] + y[y$length==x[i]-1,c(2:12)]
      }
    } else if (!(x[i] + 1) %in% x & !(x[i] - 1) %in% x) { #if there are values on either side...
      y[y$length==x[i],c(2:12)] <- y[y$length==x[i]+1,c(2:12)] + y[y$length==x[i]-1,c(2:12)]
    } else if ((x[i] + 1) %in% x & !(x[i] - 1) %in% x) {
        y[y$length==x[i],c(2:12)] <- y[y$length==x[i]-1,c(2:12)]
    } else if (!(x[i] + 1) %in% x & (x[i] - 1) %in% x & sum(y[y$length==x[i]+1,c(2:12)]) > 0) {
      y[y$length==x[i],c(2:12)] <- y[y$length==x[i]+1,c(2:12)]
    } else {
      if(sum(dmv[dmv[,1]==x[i],c(2:12)]) > 0) {
        y[y$length==x[i],c(2:12)] <- dmv[dmv[,1]==x[i],c(2:12)]
      } else {
        y[y$length==x[i],c(2:12)] <- lis[lis[,1]==x[i],c(2:12)] 
      }
    }
  } #need 2024 
  y$rowsum <- NULL
  return(y)
}, gapstofill, otofilled)

checkgaps(nearfill)[[2]]

#fill from other years
fill21 <- nearfill[[1]]
fill23 <- nearfill[[3]]
fill23[fill23$length==59, ] <- fill21[fill21$length==59,]

#by this point, from the filling steps above, the closest length bins on either side...
#...that had values were just 1 in age 12, so used that to fill 
fill24 <- nearfill[[4]]
fill24[fill24$length %in% 57,12] <- 1
#############

nearfill <- list(fill21, nearfill[[2]], fill23, fill24)

#write.csv(nearfill[[1]], "./output/tog/alk/filled/opercboth/NJNYB-ALK_2021_filled.csv")
#write.csv(nearfill[[2]], "./output/tog/alk/filled/opercboth/NJNYB-ALK_2022_filled.csv")
#write.csv(nearfill[[3]], "./output/tog/alk/filled/opercboth/NJNYB-ALK_2023_filled.csv")
#write.csv(nearfill[[4]], "./output/tog/alk/filled/opercboth/NJNYB-ALK_2024_filled.csv")
