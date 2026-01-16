library(readxl)
library(dplyr)
library(ggplot2)
# can use origin::originize_file to overwrite in place qualifying namespaces

root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"

root <- file.path(root, usr, loc)
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

# Using rec harvest and live releases to see what gaps need to be filled
als <- read_xlsx(file.path(root, "data/tog/rec/ALS_Tautog_2021-2024.xlsx"))
mrip_har <- read.csv(
  file.path(root, "data/tog/rec/Tautog_MRIP_AB1_LFs_2021-2024_NJNYB.csv"),
  header = TRUE
)

# INPUTS USED: FILLING THE ALK##############################
# Used adjacent states to fill the gaps where needed
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
# Data shaping
ny <- bind_rows(ny_comm, ny_comm24, ny_rec) |>
  mutate(Year = as.character(Year))

nj_comm <- nj_comm |> mutate(Year = format(Year, "%Y"))
nj_comm24 <- nj_comm24 |> mutate(Year = as.character(Year))
nj_comm <- bind_rows(
  nj_comm[, which(names(nj_comm) %in% names(nj_comm24))],
  nj_comm24[, which(names(nj_comm24) %in% names(nj_comm))]
)

njny <- bind_rows(ny, nj_comm[, names(ny)])
njny <- njny |> mutate(
  structure = case_when(
    `Ageing Structure` %in% c("opercular", "opec", "Operculum") ~ "operc",
    `Ageing Structure` %in% c("Otolith", "otolith") ~ "oto",
    .default = as.character(`Ageing Structure`) # Catches any remaining values
  )
)
njny$`Ageing Structure` <- NULL

# Need this exact col name for AquaticLifeHistory pkg
njny$Length <- njny$`Total Length (cm)`
njny$`Total Length (cm)` <- NULL
njny <- njny[!is.na(njny$Age) & !is.na(njny$Length) & njny$Age < 23, ] # cutoff from looking at data, getting rid of 9999 and 99999 vals

njny$Region[njny$Region != "LIS"] <- "B"
njny <- njny[, c("Age", "Region", "structure", "Length", "Year")]
njny$tl_cm <- floor(njny$Length)
## min(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm) = 4
## max(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm) = 83
## sort(unique(njny[njny$Region=="B" & njny$structure=="operc",]$tl_cm))
als <- als[als$Region == "NJNYB", ]
als$Length_cm <- als$Length_IN * 2.54 # convert inches to cm
als$Length_cm <- floor(als$Length_cm)
# min(unique(c(als$Length_cm, mrip_har$Length))) = 17
# max(unique(c(als$Length_cm, mrip_har$Length))) = 83
# sort(unique(c(als$Length_cm, mrip_har$Length)))

# Since we are only using operc/both data, the next smallest bin is 29cm
lengths <- c(29:60)
min_age <- 2
max_age <- 12
for_alk <- njny[njny$tl_cm %in% lengths, ] |>
  mutate(Age_plus = ifelse(Age >= max_age, max_age, Age)) |>
  filter(Region == "B")

# The previous assessment bounds for NJ-NYB were 15-60cm and MA-RI uses 14-60cm.
# The final bounding chosen here was post hoc from filling the ALK. The gaps
# that would have been left to fill by the end of this script when trying to use
# the full range of rec data (to 83cm) were 63cm onward, and 63cm does not occur
# in any annual raw (unfilled) data for NJ-NYB. Given that the region's last
# limit was 60cm and MA-RI also only goes to 60cm, that felt like a reasonable
# cutoff, and the highest the rec data goes below 63cm is 60cm
# in hindsight...should I be incorporating VAS here too...?
for_alk$TL_cm <- factor(for_alk$tl_cm, levels = lengths)
for_alk$Age_plus <- factor(for_alk$Age_plus, levels = min_age:max_age)

operc <- for_alk[for_alk$structure == "operc" | for_alk$structure == "both", ]
oto <- for_alk[for_alk$structure == "oto", ]
# alk_data <- operc
alk_data <- operc

# Our region elected to use only operculum data
# From a 6/13 email with Katie, she commented that there does not appear to be
# strong evidence of bias between aging structures for NJ. So I tested doing
# with the full data too.
# alk_data <- for_alk
annual_al <- split(alk_data, alk_data$Year)

PivotALK <- function(dat) {
  tab <- table(dat$`TL_cm`, dat$Age_plus) # if you switch to full age you'll need to change Age_plus to Age
  tab1 <- matrix(tab, ncol = length(min_age:max_age), dimnames = dimnames(tab))
  tab2 <- data.frame(tab1)
  df <- data.frame(names = row.names(tab2), tab2)
  colnames(df) <- c("length", as.character(min_age:max_age)) # if you switch to full age you'll need to change the 12
  return(df)
}

alks <- lapply(annual_al, PivotALK)
##########################################################

############# FILLING THE ALK
# We have several rows (length bins) that have no age samples.
# These are gaps that need to be filled because they're in rec data
# I don't think type 9 gets included?
# mrip9 <- read.csv("./data/tog/rec/Tautog_MRIP_Type9_lengths_2021-24.csv")
# mrip9 <- mrip9[mrip9$REGION=="NJNYB",]

CheckGaps <- function(alks) {
  gaps <- lapply(alks,
    FUN = function(alk) {
      as.integer(alk$length[which(rowSums(alk[, 2:ncol(alk)]) == 0)])
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
# gaps_to_fill <- CheckGaps(alks)

#### STEP 1: if last age bin with values is all in 12+ fill down to the end
age12_filled_alks <- lapply(alks, function(alk) {
  alk$length <- as.integer(alk$length)
  alk <- alk |>
    rowwise() |>
    mutate(rowsum = sum(c_across(matches("\\d")), na.rm = TRUE)) |>
    ungroup()
  max_len_aged <- max(alk$length[alk$rowsum > 0]) # last row with values
  row_i <- which(alk$length == max_len_aged)
  if (alk[row_i, as.character(max_age)] == alk[row_i, "rowsum"] && max_len_aged < max(alk$length)) {
    last_rows <- (max_len_aged + 1):max(alk$length)
    alk[alk$length %in% last_rows, 2:ncol(alk)] <- alk[row_i, 2:ncol(alk)]
  }
  alk$rowsum <- NULL
  return(alk)
})
gaps_to_fill <- CheckGaps(age12_filled_alks)

tabyr <- function(dat) {
  tab <- table(dat$`TL_cm`, dat$Age_plus) #if you switch to full age you'll need to change Age_plus to Age
  tab1 <- matrix(tab, ncol = 11, dimnames= dimnames(tab)) #if you switch to full age you'll need to change the 11
  #tab1 <- matrix(tab, ncol = 12, dimnames= dimnames(tab))
  tab2 <- data.frame(tab1)
  df <- data.frame(names = row.names(tab2), tab2)
  colnames(df) <- c("length", as.character(2:12)) #If you switch to full age you'll need to change the 12
  return(df)
}

otofill <- Map(function(x,y) {
  check <- oto[oto$Year==y & oto$Length %in% x,]
  if (nrow(check) > 0) {
    otofill <- tabyr(check)
    otofill <- otofill[otofill$length %in% check$Length,]
    #otofill$year <- y
  } else {otofill <- data.frame()}
  return(otofill)
}, gaps_to_fill, names(gaps_to_fill))

age12_filled_alks <- Map(function(x,y) {
  if(nrow(y) > 0) {
    y$length <- as.integer(y$length)
    x[x$length %in% y$length,] <- y
  }
  return(x)
}, age12_filled_alks, otofill)
gaps_to_fill <- CheckGaps(age12_filled_alks)

#### STEP 3: fill with adjacent rows, or where you can't, neighboring state ALKs
lis_unfilled_alks <- list(lis21, lis22, lis23, lis24)
names(lis_unfilled_alks) <- c("2021", "2022", "2023", "2024")

dmv_unfilled_alks <- list(dmv21, dmv22, dmv23, dmv24)
names(dmv_unfilled_alks) <- c("2021", "2022", "2023", "2024")

FillGaps <- function(i, gaps, alk, dmv, lis) {
  next_len <- gaps[i] + 1
  prev_len <- gaps[i] - 1
  next_row <- alk[alk$length == next_len, 2:ncol(alk)]
  prev_row <- alk[alk$length == prev_len, 2:ncol(alk)]
  num_fish_smaller <- sum(prev_row)
  num_fish_bigger <- sum(next_row)
  if (gaps[i] == min(alk$length)) { ### if the gap to fill is the smallest bin in the ALK...
    if (next_len != gaps[i + 1]) { # if the next greater length bin isn't empty...
      filler <- next_row # ...fill from below
    } else if (sum(lis[lis[, 1] == gaps[i], names(alk)[2:ncol(alk)]]) == 0) { # if not, use LIS if it has values for that bin
      # sum(dmv[dmv[,1]==gaps[i],min_age:max_age]) > 0 shows preference for DMV when it has values
      filler <- dmv[dmv[, 1] == gaps[i], names(alk)[2:ncol(alk)]] # else fill from DMV
    } else {
      filler <- lis[lis[, 1] == gaps[i], names(alk)[2:ncol(alk)]]
    }
  } else if (gaps[i] == max(alk$length)) { ### if the gap to fill is the largest bin in the ALK...
    if (prev_len != gaps[i - 1] && num_fish_smaller > 0) { # if the next smallest length bin doesn't need filling and has values...
      filler <- prev_row # ...fill from above
    } else {
      if (sum(lis[lis[, 1] == gaps[i], names(alk)[2:ncol(alk)]]) == 0) { # preference for filling from LIS when it has data
        filler <- dmv[dmv[, 1] == gaps[i], names(alk)[2:ncol(alk)]]
      } else {
        filler <- lis[lis[, 1] == gaps[i], names(alk)[2:ncol(alk)]]
      }
    }
  } else if (i == length(gaps)) { ### if it's the last bin to be filled (and it's not the largest length bin in the ALK...)
    filler <- if (prev_len == gaps[i - 1]) {
      next_row
    } else {
      next_row + prev_row
    } # if the bin above it is empty...
  } else if (!(next_len) %in% gaps && !(prev_len) %in% gaps) { ### any row "in the middle": if there are values on either side...
    filler <- next_row + prev_row
  } else if ((next_len) %in% gaps && !(prev_len) %in% gaps) { # if the bin below is empty...
    filler <- prev_row
  } else if (!(next_len) %in% gaps && (prev_len) %in% gaps && num_fish_bigger > 0) { # if the bin above is empty and the bin below has non-0 values
    filler <- next_row
  } else {
    if (sum(lis[lis[, 1] == gaps[i], names(alk)[2:ncol(alk)]]) == 0) { # if 0s on both sides, fill from adjacent region (LIS first)
      filler <- dmv[dmv[, 1] == gaps[i], names(alk)[2:ncol(alk)]]
    } else {
      filler <- lis[lis[, 1] == gaps[i], names(alk)[2:ncol(alk)]]
    }
  }
  ages <- names(alk)[2:ncol(alk)]
  if (any(!ages %in% names(filler))) {
    missing_age <- ages[!ages %in% names(filler)]
    zero_matrix <- matrix(0, nrow = 1, ncol = length(missing_age),
                          dimnames = list(NULL, missing_age))
    zero_df <- as.data.frame(zero_matrix)
    filler <- cbind(filler, zero_df)
    filler <- filler[,ages]
  }
  return(filler)
}

near_filled_alks <- Map(function(alk, gaps, yr) {
  dmv <- dmv_unfilled_alks[[as.character(yr)]]
  lis <- lis_unfilled_alks[[as.character(yr)]]
  names(lis)[names(lis) == "LengthCM"] <- "length"
  lis <- lis[, names(lis)[names(alk) %in% names(lis)]]
  filled_alk <- lapply(1:length(gaps),
                       FillGaps, gaps = gaps, alk = alk, dmv = dmv, lis = lis)
  alk[alk$length %in% gaps, 2:ncol(alk)] <- bind_rows(filled_alk)
  return(alk)
}, age12_filled_alks, gaps_to_fill, c(2021:2024))

CheckGaps(near_filled_alks)

# By this point, from the filling steps above, the next closest length bin to
# the end that had values was just 1 in age 12, so used that to fill down
near_filled_alks[[4]][near_filled_alks[[4]]$length %in% 57:60, max_age] <- 1
#############

########### OUTPUTS
# write.csv(alks[[1]][,-1], "NJNYB-ALK_2021_unfilled.csv")
# write.csv(alks[[2]][,-1], "NJNYB-ALK_2022_unfilled.csv")
# write.csv(alks[[3]][,-1], "NJNYB-ALK_2023_unfilled.csv")
# write.csv(alks[[4]][,-1], "NJNYB-ALK_2024_unfilled.csv")

write.csv(near_filled_alks[[1]], file.path(root, "output/tog/alk/filled/test/NJNYB-ALK_2021_filled.csv"), row.names = F)
write.csv(near_filled_alks[[2]], file.path(root, "output/tog/alk/filled/test/NJNYB-ALK_2022_filled.csv"), row.names = F)
write.csv(near_filled_alks[[3]], file.path(root, "output/tog/alk/filled/test/NJNYB-ALK_2023_filled.csv"), row.names = F)
write.csv(near_filled_alks[[4]], file.path(root, "output/tog/alk/filled/test/NJNYB-ALK_2024_filled.csv"), row.names = F)
