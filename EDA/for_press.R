library(readxl)
library(dplyr)
library(ggplot2)
library(FSA)
library(AquaticLifeHistory)

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
#als <- als[als$Region == "NJNYB", ]
#als$Length_cm <- als$Length_IN * 2.54 # convert inches to cm
#als$Length_cm <- floor(als$Length_cm)
# min(unique(c(als$Length_cm, mrip_har$Length))) = 17
# max(unique(c(als$Length_cm, mrip_har$Length))) = 83
# sort(unique(c(als$Length_cm, mrip_har$Length)))
# There is data for otoliths 17-27cm, also the only age 1 data
lengths <- c(min(njny$tl_cm, na.rm=T):max(njny$tl_cm, na.rm=T))
min_age <- 1

# Since we are only using operc/both data, the next smallest bin is 29cm
#lengths <- c(29:60)
#min_age <- 2
max_age <- 22
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
alk_data <- for_alk

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

getlengths <- lapply(alks, function(x) {
  apply(x, 2, function(y) {which(y>0)})
})

minmex <- bind_rows(lapply(getlengths, function(x) {
  bind_rows(Map(function(y, z) {
    lens <- as.integer(names(y))
    c(as.integer(z), min(lens, na.rm=T), max(lens, na.rm=T))
  }, x[2:23], names(x)[2:23]))[-1,]
  }))

minmex[minmex == -Inf] <- NA
minmex[minmex == Inf] <- NA

apply(minmex, 2, min, na.rm=T)
apply(minmex, 2, max, na.rm=T)

theme_set(theme_bw())
vb <- makeGrowthFun()
nj_comm$Length <- nj_comm$`Total Length (cm)`
nj_comm$`Total Length (cm)` <- NULL
nj_comm <- nj_comm[!is.na(nj_comm$Age) & !is.na(nj_comm$Length) & nj_comm$Age < 23, ]
sv0 <- vbStarts(Length~Age,data=nj_comm) 

nj_comm$region <- "S"
nj_comm[nj_comm$Region %in% c("394 - NJ (0-3 nm) NJ/NY Boundary to 40 N Lat",
                              "395 - NJ ( 0 - 3 nm ) 40 N to 39 30 8.43 N)",
                              "612 - Cholera Bank",
                              "615 - Barnegat Ridge",
                              "178 - Sandy Hook Bay and Raritan Bay"),]$region <- "N"
nj_comm <- nj_comm[nj_comm$Region != "614 - New Jersey Shore East of NJ Ocean Waters",]

ggplot(data=nj_comm,aes(x=Age,y=Length,color=region)) +
  geom_point(size=2,alpha=0.3) +
  scale_y_continuous(name="Total Length (cm)") + #,limits=c(0,700)
  scale_x_continuous(name="Age (years)") + #,breaks=0:1
  #scale_color_manual(values=c("male"="darkblue","female"="darkred")) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=sv0),
              linewidth=1) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position=c(0.8,0.2),
        legend.title=element_blank())

in_dir <- file.path(root, "data/tog")
life_history <- lapply(
  c(
    file.path(in_dir, "Tautog Data Template 2025_NJDEP.xlsx"),
    file.path(in_dir, "Tautog Data Template 2025_NJDEP_2024-update.xlsx")
  ),
  read_xlsx,
  sheet = "LifeHistory", skip = 6
)

names(life_history[[1]])[names(life_history[[1]]) == "Year"] <- "Date"
life_history[[1]]$Year <- as.integer(format(life_history[[1]]$Date, "%Y"))
life_history[[2]] <- life_history[[2]] |>
  rename("Total Length (cm)" = "cm", "Weight (g)" = "grams") |>
  select(-"Total Length (mm)", -"Weight (kg)")
life_history[[1]] <- life_history[[1]][, names(life_history[[2]])]
life_history <- bind_rows(life_history) |>
  rename("Weight" = "Weight (g)") |>
  mutate(Length = floor(`Total Length (cm)`)) |>
  select(Year, Age, Weight, Region, Maturity, Length)
life_history <- life_history[!is.na(life_history$Weight), ]
table(life_history$Year)

vb <- makeGrowthFun()
life_history <- life_history[!is.na(life_history$Age) & !is.na(life_history$Length) & life_history$Age < 23, ]
sv0 <- vbStarts(Length~Age,data=life_history) 

ggplot(data=life_history,aes(x=Age,y=Length,color=Maturity)) +
  geom_point(size=2,alpha=0.3) +
  scale_y_continuous(name="Total Length (cm)") + #,limits=c(0,700)
  scale_x_continuous(name="Age (years)") + #,breaks=0:1
  #scale_color_manual(values=c("male"="darkblue","female"="darkred")) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=sv0),
              linewidth=1) +
  theme(panel.grid.minor.x=element_blank(),
        legend.position=c(0.8,0.2),
        legend.title=element_blank())

lengths <- c(min(life_history$Length, na.rm=T):max(life_history$Length, na.rm=T))
min_age <- 0

# Since we are only using operc/both data, the next smallest bin is 29cm
#lengths <- c(29:60)
#min_age <- 2
max_age <- 15
for_alk <- life_history |>
  mutate(Age_plus = ifelse(Age >= max_age, max_age, Age))

for_alk$TL_cm <- factor(for_alk$Length, levels = lengths)
for_alk$Age_plus <- factor(for_alk$Age_plus, levels = min_age:max_age)
# alk_data <- operc
alk_data <- for_alk

# Our region elected to use only operculum data
# From a 6/13 email with Katie, she commented that there does not appear to be
# strong evidence of bias between aging structures for NJ. So I tested doing
# with the full data too.
# alk_data <- for_alk
annual_al <- split(alk_data, alk_data$Year)

alks <- lapply(annual_al, PivotALK)
getlengths <- lapply(alks, function(x) {
  apply(x, 2, function(y) {which(y>0)})
})

minmex <- bind_rows(lapply(getlengths, function(x) {
  bind_rows(Map(function(y, z) {
    lens <- as.integer(names(y))
    c(as.integer(z), min(lens, na.rm=T), max(lens, na.rm=T))
  }, x[2:16], names(x)[2:16]))[-1,]
}))

minmex[minmex == -Inf] <- NA
minmex[minmex == Inf] <- NA

apply(minmex, 2, min, na.rm=T)
apply(minmex, 2, max, na.rm=T)

min_age <- 1
for_alk <- life_history |>
  mutate(Age_plus = ifelse(Age >= max_age, max_age, Age))

for_alk$TL_cm <- factor(for_alk$Length, levels = lengths)
for_alk$Age_plus <- factor(for_alk$Age_plus, levels = min_age:max_age)
# alk_data <- operc
alk_data <- for_alk

alk_data <- alk_data[alk_data$Maturity %in% c("M", "F"),]
annual_al <- split(alk_data, alk_data$Maturity)
alks <- lapply(annual_al, PivotALK)
getlengths <- lapply(alks, function(x) {
  apply(x, 2, function(y) {which(y>0)})
})

minmex <- lapply(getlengths, function(x) {
  bind_rows(Map(function(y, z) {
    lens <- as.integer(names(y))
    c(as.integer(z), min(lens, na.rm=T), max(lens, na.rm=T))
  }, x[2:16], names(x)[2:16]))[-1,]
})
