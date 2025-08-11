library(readxl)
library(dplyr)
library(ggplot2)

root <- "C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents" # jgorzo
# setwd("/media/jess/9CE61C02E61BDB7A/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents")
# On Ubuntu, you have to have this open in files/ssd to have it mounted

# INPUTS USED: UNFILLED ALK###################################
# Using CommBioSamples and RecBioSamples to compute age-length key (ALK)
ny_comm <- read_xlsx(
  file.path(root, "data/tog/2025SA_NY_Tautog Data 2021-2023_corrected.xlsx"),
  sheet = "CommBioSamples",
  range = cell_rows(6:901)
)
ny_comm24 <- read_xlsx(
  file.path(root, "data/tog/2025SA_NY_Tautog Data 2024-1.xlsx"),
  sheet = "CommBioSamples",
  range = cell_rows(6:797)
)
ny_rec <- read_xlsx(
  file.path(root, "data/tog/2025SA_NY_Tautog Data 2021-2023_corrected.xlsx"),
  sheet = "RecBioSamples",
  range = cell_rows(6:110)
)
nj_comm <- read_xlsx(
  file.path(root, "data/tog/Tautog Data Template 2025_NJDEP.xlsx"),
  sheet = "CommBioSamples",
  range = cell_rows(6:682)
)
nj_comm24 <- read_xlsx(
  file.path(root, "data/tog/Tautog Data Template 2025_NJDEP_2024data.xlsx"),
  sheet = "CommBioSamples",
  range = cell_rows(6:239)
)

# Using rec harvest and live releases to see what gaps need to be filled...
als <- read_xlsx(file.path(root, "data/tog/rec/ALS_Tautog_2021-2024.xlsx"))
mrip_har <- read.csv(
  file.path(root, "data/tog/rec/Tautog_MRIP_AB1_LFs_2021-2024_NJNYB.csv"),
  header = TRUE
)

# INPUTS USED: FILLING THE ALK##############################
# Used adjacent states to fill the gaps where needed...
lis21 <- read_xlsx(
  file.path(root, "data/tog/other/LIS_ALK_unfilled060325.xlsx"),
  sheet = "LIS_2021"
)
lis22 <- read_xlsx(
  file.path(root, "data/tog/other/LIS_ALK_unfilled060325.xlsx"),
  sheet = "LIS_2022"
)
lis23 <- read_xlsx(
  file.path(root, "data/tog/other/LIS_ALK_unfilled060325.xlsx"),
  sheet = "LIS_2023"
)
lis24 <- read_xlsx(
  file.path(root, "data/tog/other/LIS_ALK_unfilled060325.xlsx"),
  sheet = "LIS_2024"
)

dmv21 <- read_xlsx(
  file.path(root, "data/tog/other/dmv/DMV_ALK_unfilled.xlsx"),
  sheet = "ALK_2021_unfilled"
)
dmv22 <- read_xlsx(
  file.path(root, "data/tog/other/dmv/DMV_ALK_unfilled.xlsx"),
  sheet = "ALK_2022_unfilled"
)
dmv23 <- read_xlsx(
  file.path(root, "data/tog/other/dmv/DMV_ALK_unfilled.xlsx"),
  sheet = "ALK_2023_unfilled"
)
dmv24 <- read_xlsx(
  file.path(root, "data/tog/other/dmv/DMV_ALK_unfilled.xlsx"),
  sheet = "ALK_2024_unfilled"
)
###############################################################

########### UNFILLED ALK####################
# data shaping
ny <- bind_rows(ny_comm, ny_comm24, ny_rec) %>%
  mutate(Year = as.character(Year))

nj_comm <- nj_comm %>% mutate(Year = format(Year, "%Y"))
nj_comm24 <- nj_comm24 %>% mutate(Year = as.character(Year))
nj_comm <- bind_rows(
  nj_comm[, which(names(nj_comm) %in% names(nj_comm24))],
  nj_comm24[, which(names(nj_comm24) %in% names(nj_comm))]
)

njny <- bind_rows(ny, nj_comm[, names(ny)])
njny$structure <- njny$`Ageing Structure`
njny$`Ageing Structure` <- NULL
njny$structure[njny$structure %in% c("opercular", "opec", "Operculum")] <- "operc"
njny$structure[njny$structure == "Otolith"] <- "oto"

# need this exact col name for AquaticLifeHistory pkg
njny$Length <- njny$`Total Length (cm)` 
njny$`Total Length (cm)` <- NULL
njny <- njny[!is.na(njny$Age) & !is.na(njny$Length) & njny$Age < 23, ] # cutoff from looking at data, getting rid of 9999 and 99999 vals

njny$Region[njny$Region != "LIS"] <- "B"
njny <- njny[, c("Age", "Region", "structure", "Length", "Year")]
njny$tl_cm <- floor(njny$Length)
## min(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm) = 4
## max(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm) = 83
## sort(unique(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm)) 
# 29 is next largest length incl in rec data
for_alk <- njny[njny$tl_cm >= 29 & njny$tl_cm < 61, ] %>%
  mutate(Age_plus = ifelse(Age >= 12, 12, Age)) %>%
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
alk_data <- operc

# Our region elected to use only operculum data...
# From a 6/13 email with Katie, she commented that there does not appear to be 
# strong evidence of bias between aging structures for NJ. So I tested doing 
# with the full data too.
# alk_data <- for_alk
annual_al <- split(alk_data, alk_data$Year)

alks <- lapply(annual_al, function(dat) {
  tab <- table(dat$`TL_cm`, dat$Age_plus) # if you switch to full age you'll need to change Age_plus to Age
  tab1 <- matrix(tab, ncol = 11, dimnames = dimnames(tab)) # if you switch to full age you'll need to change the 11
  tab2 <- data.frame(tab1)
  df <- data.frame(names = row.names(tab2), tab2)
  colnames(df) <- c("length", as.character(2:12)) # If you switch to full age you'll need to change the 12
  return(df)
})
##########################################################

############# FILLING THE ALK
# We have several rows (length bins) that have no age samples.
# These are gaps that need to be filled because they're in rec data
# I don't think type 9 gets included?
# mrip9 <- read.csv("./data/tog/rec/Tautog_MRIP_Type9_lengths_2021-24.csv")
# mrip9 <- mrip9[mrip9$REGION=="NJNYB",]
als <- als[als$Region == "NJNYB", ]
als$Length_cm <- als$Length_IN * 2.54 # convert inches to cm
als$Length_cm <- floor(als$Length_cm)
# min(unique(c(als$Length_cm, mrip_har$Length))) = 17
# max(unique(c(als$Length_cm, mrip_har$Length))) = 83
# sort(unique(c(als$Length_cm, mrip_har$Length)))
check_gaps <- function(alks) {
  gaps <- lapply(alks,
    FUN = function(alk) {
      as.integer(names(rowSums(alk[, 2:12]))[which(rowSums(alk[, 2:12]) == 0)])
    }
  ) # these are all of the gaps that exist
  names(gaps) <- c(2021:2024)
  catch_match <- Map(
    function(gap, yr) {
      gap %in% unique(c(
        als$Length_cm[als$Year == as.integer(yr)],
        mrip_har$Length[mrip_har$Year == as.integer(yr)]
      )) # mrip9$LENGTH.ROUNDED.DOWN.TO.NEAREST.CM[mrip9$YEAR==as.integer(y)],
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
  row_i <- which(alk$length == max_len_aged)
  if (alk[row_i, 12] == alk[row_i, "rowsum"] && max_len_aged < max(alk$length)) {
    last_rows <- (max_len_aged + 1):max(alk$length)
    alk[alk$length %in% last_rows, 12] <- alk[row_i, 12]
  }
  alk$rowsum <- NULL
  return(alk)
})
gaps_to_fill <- check_gaps(age12_filled_alks)

#### STEP 2: fill with otolith data from my own region (NJNYB)

# From Katie's 6/13 email:
# "Given that (1) there is no clear evidence of bias between the structures from 
# the data we have and (2) this is probably for length bins with overall small 
# sample sizes whether you are borrowing from NJ otoliths or other 
# years/regions, I think either option is justifiable. There’s going to be
# uncertainty either way – does the benefit of using the NJ otoliths outweigh
# the fact that we’re creating a different gap-filling protocol for NJ-NYB than
# for other regions? We let the regions determine their own gap-filling
# procedures last time, so I don’t think it’s a big deal if there are
# differences from region to region this time, especially if it’s
# well-documented, but I don’t know how the full SAS feels. If, after working
# with the data, you feel using the NJ otoliths before looking to another
# region or year,is the way to go, I’d support that."
oto_filled_alks <- Map(function(gaps, yr, alk) {
  check <- oto[oto$Year == yr & oto$tl_cm %in% gaps, ]
  if (nrow(check) > 0) {
    oto_for_fill <- pivot_alk(check)
    oto_for_fill <- oto_for_fill[oto_for_fill$length %in% check$tl_cm, ]
  } else {
    oto_for_fill <- data.frame()
  }

  if (nrow(oto_for_fill) > 0) {
    alk[alk$length %in% oto_for_fill$length, ] <- oto_for_fill
  }
  return(alk)
}, gaps_to_fill, names(gaps_to_fill), age12_filled_alks)

#### STEP 3: fill with adjacent rows, or where you can't, neighboring state ALKs
lis_unfilled_alks <- list(lis21, lis22, lis23, lis24)
names(lis_unfilled_alks) <- c("2021", "2022", "2023", "2024")

dmv_unfilled_alks <- list(dmv21, dmv22, dmv23, dmv24)
names(dmv_unfilled_alks) <- c("2021", "2022", "2023", "2024")

gaps_to_fill <- check_gaps(oto_filled_alks)

fill_gaps <- function(i) {
  next_len <- gaps[i] + 1
  prev_len <- gaps[i] - 1
  next_row <- alk[alk$length == next_len, 2:12]
  prev_row <- alk[alk$length == prev_len, 2:12]
  num_fish_smaller <- sum(prev_row)
  num_fish_bigger <- sum(next_row)
  if (gaps[i] == min(alk$length)) { ### if the gap to fill is the smallest bin in the ALK...
    if (next_len != gaps[i + 1]) { # if the next greater length bin isn't empty...
      filler <- next_row # ...fill from below
    } else if (sum(lis[lis[, 1] == gaps[i], 2:12]) == 0) { # if not, use LIS if it has values for that bin
      # sum(dmv[dmv[,1]==gaps[i],2:12]) > 0 shows preference for DMV when it has values
      filler <- dmv[dmv[, 1] == gaps[i], 2:12] # else fill from DMV
    } else {
      filler <- lis[lis[, 1] == gaps[i], 2:12]
    }
  } else if (gaps[i] == max(alk$length)) { ### if the gap to fill is the largest bin in the ALK...
    if (prev_len != gaps[i - 1] && num_fish_smaller > 0) { # if the next smallest length bin doesn't need filling and has values...
      filler <- prev_row # ...fill from above
    } else {
      if (sum(lis[lis[, 1] == gaps[i], 2:12]) == 0) { # preference for filling from LIS when it has data
        filler <- dmv[dmv[, 1] == gaps[i], 2:12]
      } else {
        filler <- lis[lis[, 1] == gaps[i], 2:12]
      }
    }
  } else if (i == length(gaps)) { ### if it's the last bin to be filled (and it's not the largest length bin in the ALK...)
    filler <- ifelse(prev_len == gaps[i - 1], next_row, next_row + prev_row) # if the bin above it is empty...
  } else if (!(next_len) %in% gaps && !(prev_len) %in% gaps) { ### any row "in the middle": if there are values on either side...
    filler <- next_row + prev_row
  } else if ((next_len) %in% gaps && !(prev_len) %in% gaps) { # if the bin below is empty...
    filler <- prev_row
  } else if (!(next_len) %in% gaps && (prev_len) %in% gaps && num_fish_bigger > 0) { # if the bin above is empty and the bin below has non-0 values
    filler <- next_row
  } else {
    if (sum(lis[lis[, 1] == gaps[i], 2:12]) == 0) { # if 0s on both sides, fill from adjacent region (LIS first)
      filler <- dmv[dmv[, 1] == gaps[i], 2:12]
    } else {
      filler <- lis[lis[, 1] == gaps[i], 2:12]
    }
  }
  return(filler)
}

near_filled_alks <- Map(function(alk, gaps, yr) {
  dmv <- dmv_unfilled_alks[[as.character(yr)]]
  lis <- lis_unfilled_alks[[as.character(yr)]][, c(1, 3:13)]
  filled_alk <- lapply(1:length(gaps), fill_gaps)
  alk[alk$length %in% gaps, 2:12] <- bind_rows(filled_alk)
  return(alk) 
}, oto_filled_alks, gaps_to_fill, c(2021:2024))

check_gaps(near_filled_alks)

# By this point, from the filling steps above, the next closest length bin to 
# the end that had values was just 1 in age 12, so used that to fill down
near_filled_alks[[4]][near_filled_alks[[4]]$length %in% 57:60, 12] <- 1
#############

########### OUTPUTS
# write.csv(alks[[1]][,-1], "NJNYB-ALK_2021_unfilled.csv")
# write.csv(alks[[2]][,-1], "NJNYB-ALK_2022_unfilled.csv")
# write.csv(alks[[3]][,-1], "NJNYB-ALK_2023_unfilled.csv")
# write.csv(alks[[4]][,-1], "NJNYB-ALK_2024_unfilled.csv")

# write.csv(near_filled_alks[[1]], file.path(root, "output/tog/alk/filled/opercboth/NJNYB-ALK_2021_filled.csv"), row.names = F)
# write.csv(near_filled_alks[[2]], file.path(root, "output/tog/alk/filled/opercboth/NJNYB-ALK_2022_filled.csv"), row.names = F)
# write.csv(near_filled_alks[[3]], file.path(root, "output/tog/alk/filled/opercboth/NJNYB-ALK_2023_filled.csv"), row.names = F)
# write.csv(near_filled_alks[[4]], file.path(root, "output/tog/alk/filled/opercboth/NJNYB-ALK_2024_filled.csv"), row.names = F)
