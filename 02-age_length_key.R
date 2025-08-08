library(readxl)
library(dplyr)
library(ggplot2)

setwd("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents") # jgorzo jgorzo
# setwd("/media/jess/9CE61C02E61BDB7A/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents")
# on Ubuntu, you have to have this open in files/ssd to have it mounted...

# INPUTS USED: UNFILLED ALK###################################
# Using CommBioSamples and RecBioSamples to compute age-length key (ALK)
ny_comm <- read_excel("./data/tog/2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet = "CommBioSamples", range = cell_rows(6:901))
ny_comm24 <- read_excel("./data/tog/2025SA_NY_Tautog Data 2024-1.xlsx", sheet = "CommBioSamples", range = cell_rows(6:797))
ny_rec <- read_excel("./data/tog/2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet = "RecBioSamples", range = cell_rows(6:110))
nj_comm <- read_excel("./data/tog/Tautog Data Template 2025_NJDEP.xlsx", sheet = "CommBioSamples", range = cell_rows(6:682))
nj_comm24 <- read_excel("./data/tog/Tautog Data Template 2025_NJDEP_2024data.xlsx", sheet = "CommBioSamples", range = cell_rows(6:239))

# Using rec harvest and live releases to see what gaps need to be filled...
als <- read_excel("./data/tog/rec/ALS_Tautog_2021-2024.xlsx")
MRIP_har <- read.csv("./data/tog/rec/Tautog_MRIP_AB1_LFs_2021-2024_NJNYB.csv", header = TRUE)

# INPUTS USED: FILLING THE ALK##############################
# Used adjacent states to fill the gaps where needed...
lis21 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet = "LIS_2021")
lis22 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet = "LIS_2022")
lis23 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet = "LIS_2023")
lis24 <- read_excel("./data/tog/other/LIS_ALK_unfilled060325.xlsx", sheet = "LIS_2024")

dmv21 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet = "ALK_2021_unfilled")
dmv22 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet = "ALK_2022_unfilled")
dmv23 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet = "ALK_2023_unfilled")
dmv24 <- read_excel("./data/tog/other/dmv/DMV_ALK_unfilled.xlsx", sheet = "ALK_2024_unfilled")
###############################################################

########### UNFILLED ALK####################
# data shaping
ny <- bind_rows(ny_comm, ny_comm24, ny_rec) %>% mutate(Year = as.character(Year))

nj_comm <- nj_comm %>% mutate(Year = format(Year, "%Y"))
nj_comm24 <- nj_comm24 %>% mutate(Year = as.character(Year))
nj_comm <- bind_rows(nj_comm[, which(names(nj_comm) %in% names(nj_comm24))], nj_comm24[, which(names(nj_comm24) %in% names(nj_comm))])

njny <- bind_rows(ny, nj_comm[, names(ny)])
njny$structure <- njny$`Ageing Structure`
njny$`Ageing Structure` <- NULL
njny$structure[njny$structure %in% c("opercular", "opec", "Operculum")] <- "operc"
njny$structure[njny$structure == "Otolith"] <- "oto"

njny$Length <- njny$`Total Length (cm)` # need this exact col name for AquaticLifeHistory pkg
njny$`Total Length (cm)` <- NULL
njny <- njny[!is.na(njny$Age) & !is.na(njny$Length) & njny$Age < 23, ] # cutoff from looking at data, getting rid of 9999 and 99999 vals

njny$Region[njny$Region != "LIS"] <- "B"
njny <- njny[, c("Age", "Region", "structure", "Length", "Year")]
njny$tl_cm <- floor(njny$Length)
## min(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm) = 4
## max(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm) = 83
## sort(unique(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm)) 29 is next largest length incl in rec data

for_alk <- njny[njny$tl_cm >= 29 & njny$tl_cm < 61, ] %>%
  mutate(alk, Age_plus = ifelse(Age >= 12, 12, Age)) %>%
  filter(Region == "B")
# The previous assessment bounds for NJ-NYB were 15-60cm and MA-RI uses 14-60cm
# This final bounding was ad-hoc from filling the ALK. The gaps left to fill...
# ...by the end of this script when trying to use the full range of data (to 83cm)
# ...were 63cm onward, and 63cm does not occur in any annual raw (unfilled) data for NJ-NYB.
# Given that the region's last limit was 60cm and MA-RI also only goes to 60cm...
# ...that felt like a reasonable cutoff, and the highest the rec data goes below 63cm is 60cm

for_alk$TL_cm <- factor(for_alk$tl_cm, levels = 29:60)
for_alk$Age_plus <- factor(for_alk$Age_plus, levels = 2:12)

operc <- for_alk[for_alk$structure == "operc" | for_alk$structure == "both", ]
oto <- for_alk[for_alk$structure == "oto", ]
#alk_data <- operc

#Originally, our region elected to use only operculum data...
#but both after looking at the gaps left in the data and what was available for filling
#and what the consequences were of filling various ways, it made for a more cohesive key
#to use all data for the region. From a 6/13 email with Katie, she commented
#that there does not appear to be strong evidence of bias between aging structures for NJ
alk_data <- for_alk
annual_al <- split(alk_data, alk_data$Year) #could try across all structures...

pivot_alk <- function(dat) {
  tab <- table(dat$`TL_cm`, dat$Age_plus) # if you switch to full age you'll need to change Age_plus to Age
  tab1 <- matrix(tab, ncol = 11, dimnames = dimnames(tab)) # if you switch to full age you'll need to change the 11
  tab2 <- data.frame(tab1)
  df <- data.frame(names = row.names(tab2), tab2)
  colnames(df) <- c("length", as.character(2:12)) # If you switch to full age you'll need to change the 12
  return(df)
}

alks <- lapply(annual_al, pivot_alk)
# write.csv(alks[[1]][,-1], "NJNYB-ALK_2021_unfilled.csv")
# write.csv(alks[[2]][,-1], "NJNYB-ALK_2022_unfilled.csv")
# write.csv(alks[[3]][,-1], "NJNYB-ALK_2023_unfilled.csv")
# write.csv(alks[[4]][,-1], "NJNYB-ALK_2024_unfilled.csv")
##########################################################

############# FILLING THE ALK
# We have several rows (length bins) that have no age samples.
# These are gaps that need to be filled.
# using rec data to see what gaps need to be filled, I don't think type 9 gets included?
# mrip9 <- read.csv("./data/tog/rec/Tautog_MRIP_Type9_lengths_2021-24.csv")
# mrip9 <- mrip9[mrip9$REGION=="NJNYB",]
als <- als[als$Region == "NJNYB", ]
als$Length_cm <- als$Length_IN * 2.54 # convert inches to cm
als$Length_cm <- floor(als$Length_cm)
# min(unique(c(als$Length_cm, MRIP_har$Length))) = 17
# max(unique(c(als$Length_cm, MRIP_har$Length))) = 83
# sort(unique(c(als$Length_cm, MRIP_har$Length)))
check_gaps <- function(alks) {
  gaps <- lapply(alks, FUN = function(alk) as.integer(names(rowSums(alk[, 2:12]))[which(rowSums(alk[, 2:12]) == 0)])) # these are all of the gaps that exist
  names(gaps) <- c(2021:2024)

  catch_match <- Map(
    function(gap, yr) {
      gap %in% unique(c(als$Length_cm[als$Year == as.integer(yr)], MRIP_har$Length[MRIP_har$Year == as.integer(yr)])) # mrip9$LENGTH.ROUNDED.DOWN.TO.NEAREST.CM[mrip9$YEAR==as.integer(y)],
    },
    gaps, names(gaps)
  )

  gaps_to_fill <- Map(
    function(alk_gap, len_catch) {
      alk_gap[len_catch]
    },
    gaps, catch_match
  )
  return(gaps_to_fill)
}
# gaps_to_fill <- check_gaps(alks)

#### STEP 1: if last age bin with values is all in 12+ fill down to the end
age12_filled_alks <- lapply(alks, function(alk) {
  alk$length <- as.integer(alk$length)
  alk <- alk %>% mutate(rowsum = rowSums(.[2:12], na.rm = TRUE)) # add row sum
  max_len_aged <- max(alk$length[alk$rowsum > 0]) # last row with values
  if (alk[alk$length == max_len_aged, 12] == alk[alk$length == max_len_aged, "rowsum"] & max_len_aged < max(alk$length)) {
    alk[alk$length %in% (max_len_aged + 1):max(alk$length), 12] <- alk[alk$length == max_len_aged, 12]
  }
  return(alk)
})
gaps_to_fill <- check_gaps(age12_filled_alks)

#### STEP 2: fill with otolith data from my own region (NJNYB)

#From Katie's 6/13 email:
#Given that (1) there is no clear evidence of bias between the structures from the data we have and 
#(2) this is probably for length bins with overall small sample sizes whether you are borrowing from NJ otoliths or other years/regions, 
#I think either option is justifiable. There’s going to be uncertainty either way – 
#does the benefit of using the NJ otoliths outweigh the fact that we’re creating a different gap-filling protocol for NJ-NYB than for other regions? 
#We let the regions determine their own gap-filling procedures last time, 
#so I don’t think it’s a big deal if there are differences from region to region this time, 
#especially if it’s well-documented, but I don’t know how the full SAS feels.
#If, after working with the data, you feel using the NJ otoliths before looking to another region or year, 
#is the way to go, I’d support that. 

#...here is my attempt at well-documenting! :)
oto_fill_rows <- Map(function(gaps, yr) {
  check <- oto[oto$Year == yr & oto$tl_cm %in% gaps, ]
  if (nrow(check) > 0) {
    oto_for_fill <- pivot_alk(check)
    oto_for_fill <- oto_for_fill[oto_for_fill$length %in% check$tl_cm, ]
  } else {
    oto_for_fill <- data.frame()
  }
  return(oto_for_fill)
}, gaps_to_fill, names(gaps_to_fill))

otofilled <- Map(function(alk, otofill) {
  if (nrow(otofill) > 0) {
    alk[alk$length %in% otofill$length, ] <- otofill
  }
  return(alk)
}, age12_filled_alks, oto_fill_rows)

otofilled <- Map(
  function(alk, yr) {
    alk$year <- yr
    return(alk)
  },
  otofilled, c(2021:2024)
)
# filled 31, 48 in 2024...

#### STEP 3: fill with adjacent rows, or where you can't, neighboring state ALKs
LIS_unfill <- list(lis21, lis22, lis23, lis24)
names(LIS_unfill) <- c("2021", "2022", "2023", "2024")

dmv_unfill <- list(dmv21, dmv22, dmv23, dmv24)
names(dmv_unfill) <- c("2021", "2022", "2023", "2024")

gaps_to_fill <- check_gaps(otofilled)

nearfill <- Map(function(alk, gaps_to_fill) {
  dmv <- dmv_unfill[[as.character(alk$year[1])]]
  lis <- LIS_unfill[[as.character(alk$year[1])]][, c(1, 3:13)]
  gaps <- which(diff(gaps_to_fill) > 1) + 1 # Identifalk the indegaps_to_fill of the element after the gap
  missing_numbers <- gaps_to_fill[gaps] - 1 # Find the missing number(s)
  lastrowfilled <- missing_numbers[length(missing_numbers)]

  for (i in 1:length(gaps_to_fill)) {
    if (gaps_to_fill[i] == min(alk$length)) { ### if the gap to fill is the smallest bin in the ALK...
      if (gaps_to_fill[i] + 1 != gaps_to_fill[i + 1]) { # if the negaps_to_fillt greater length bin isn't emptalk...
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] + 1, c(2:12)] # ...fill from below
      } else if (sum(lis[lis[, 1] == gaps_to_fill[i], c(2:12)]) == 0) { # e.g. need to fill 29 for 2021, 2023
        # sum(dmv[dmv[,1]==gaps_to_fill[i],c(2:12)]) > 0 shows preference for DMV when it has values
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- dmv[dmv[, 1] == gaps_to_fill[i], c(2:12)] # if the negaps_to_fillt greatest length bin from the smallest is emptalk, and LIS is emptalk, fill from DMV
      } else {
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- lis[lis[, 1] == gaps_to_fill[i], c(2:12)] # prefer to fill with LIS
      }
    } else if (gaps_to_fill[i] == max(alk$length)) { ### if the gap to fill is the largest bin in the ALK...
      if (gaps_to_fill[i] - 1 != gaps_to_fill[i - 1] & sum(alk[alk$length == gaps_to_fill[i] - 1, c(2:12)]) > 0) { # if the negaps_to_fillt smallest length bin doesn't need filling and has values...
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] - 1, c(2:12)] # ...fill from above
      } else {
        if (sum(lis[lis[, 1] == gaps_to_fill[i], c(2:12)]) == 0) { # preference for filling from LIS when it has data
          alk[alk$length == gaps_to_fill[i], c(2:12)] <- dmv[dmv[, 1] == gaps_to_fill[i], c(2:12)]
        } else {
          alk[alk$length == gaps_to_fill[i], c(2:12)] <- lis[lis[, 1] == gaps_to_fill[i], c(2:12)]
        }
      }
    } else if (i == length(gaps_to_fill)) { ### if it's the last bin to be filled (and it's not the largest length bin in the kealk...)
      if (gaps_to_fill[i] - 1 == gaps_to_fill[i - 1]) { # if the bin above it is emptalk...
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] + 1, c(2:12)] # ...fill with the bin below
      } else {
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] + 1, c(2:12)] + alk[alk$length == gaps_to_fill[i] - 1, c(2:12)]
      }
    } else if (!(gaps_to_fill[i] + 1) %in% gaps_to_fill & !(gaps_to_fill[i] - 1) %in% gaps_to_fill) { ### analk row "in the middle": if there are values on either side...
      alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] + 1, c(2:12)] + alk[alk$length == gaps_to_fill[i] - 1, c(2:12)]
    } else if ((gaps_to_fill[i] + 1) %in% gaps_to_fill & !(gaps_to_fill[i] - 1) %in% gaps_to_fill) { # if the bin below is emptalk...
      alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] - 1, c(2:12)]
    } else if (!(gaps_to_fill[i] + 1) %in% gaps_to_fill & (gaps_to_fill[i] - 1) %in% gaps_to_fill & sum(alk[alk$length == gaps_to_fill[i] + 1, c(2:12)]) > 0) { # if the bin above is emptalk and the bin below has non-0 values
      alk[alk$length == gaps_to_fill[i], c(2:12)] <- alk[alk$length == gaps_to_fill[i] + 1, c(2:12)]
    } else {
      if (sum(lis[lis[, 1] == gaps_to_fill[i], c(2:12)]) == 0) { # if 0s on both sides, fill from adjacent region (LIS first)
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- dmv[dmv[, 1] == gaps_to_fill[i], c(2:12)]
      } else {
        alk[alk$length == gaps_to_fill[i], c(2:12)] <- lis[lis[, 1] == gaps_to_fill[i], c(2:12)]
      }
    }
  } # need 2024
  alk$rowsum <- NULL
  return(alk)
}, otofilled, gaps_to_fill)

check_gaps(nearfill)

# by this point, from the filling steps above, the next closest length bin to the end...
# ...that had values was just 1 in age 12, so used that to fill down
fill24 <- nearfill[[4]]
fill24[fill24$length %in% 57:60, 12] <- 1
#############

nearfill <- list(nearfill[[1]], nearfill[[2]], nearfill[[3]], fill24)

write.csv(nearfill[[1]], "./output/tog/alk/filled/allstr/NJNYB-ALK_2021_filled.csv")
write.csv(nearfill[[2]], "./output/tog/alk/filled/allstr/NJNYB-ALK_2022_filled.csv")
write.csv(nearfill[[3]], "./output/tog/alk/filled/allstr/NJNYB-ALK_2023_filled.csv")
write.csv(nearfill[[4]], "./output/tog/alk/filled/allstr/NJNYB-ALK_2024_filled.csv")
