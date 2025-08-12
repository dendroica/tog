# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script calculates total catch in numbers of fish
# the length frequencies of different components of catch
# and combines those with the filled-in ALKs to calculate catch-at-age...
# ...then weight-at-age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"

root <- file.path(root, usr, loc)
# root <- "/media/jess/9CE61C02E61BDB7A/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents"
# On Ubuntu, you have to have this open in files/ssd to have it mounted...
library(tidyverse)
library(readxl)
library(ggplot2)

# INPUTS USED#############################
# Load ALKs
alks <- list.files(file.path(root, "output/tog/alk/filled/opercboth"),
  full.names = TRUE
) %>% map(read.csv)
# Folder contains...
# NJNYB-ALK_2021_filled.csv
# NJNYB-ALK_2022_filled.csv
# NJNYB-ALK_2023_filled.csv
# NJNYB-ALK_2024_filled.csv

# Load recreational catch and discard
total_catch <- read.csv(
  file.path(root, "data/tog/rec/Tautog_MRIP_TotalCatch_2021-2024_NJNYB.csv"),
  header = TRUE
)

# Load commercial catch data
comm_catch <- read_xlsx(
  file.path(root, "data/tog/Regional_Comm_landings_MT_07.18.25.xlsx"),
  sheet = "MT"
)[, c("Year", "NYB-NJ")]

# Length Frequencies for Recreational Catch and Discard
# Modified from discard_LF.R from Samarah Nehemiah for LIS
harvest_lf <- read.csv(file.path(root, "output/tog/harvest_LF.csv"))
discard_lf <- read.csv(file.path(root, "output/tog/discard_LF.csv"))[, 2:6]

in_dir <- file.path(root, "data/tog")
life_history <- lapply(
  c(
    file.path(in_dir, "Tautog Data Template 2025_NJDEP.xlsx"),
    file.path(in_dir, "Tautog Data Template 2025_NJDEP_2024-update.xlsx")
  ),
  read_xlsx,
  sheet = "LifeHistory", skip = 6
)
#########################################
# convert to proportions
# alks <- lapply(alks, function(y) {
#  y <- y %>% select(2:13)
# })
alkprops <- lapply(alks, function(y) {
  y <- y %>%
    mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) %>% # add row sum
    mutate(across(2:12, .fns = function(x) {
      x / rowsum
    })) %>%
    replace(is.na(.), 0) %>%
    rename("Length" = "length")
})

names(harvest_lf) <- c("Length", "2021", "2022", "2023", "2024")
#harvest_lf_propnj <- harvest_lf %>%
#  mutate(across(`2021`:`2024`, .fns = function(x) {
#    x / sum(x, na.rm = TRUE)
#  }))
recharvestCAA <- Map(function(x, y) {
  rechar_year <- left_join(harvest_lf[, c(1, x + 1)], y) %>%
    rename(Number = as.character(2020 + x), Total.Count = rowsum) %>%
    mutate(across(paste0("X", 2:12), .fns = function(x) {
      x * Number
    })) %>%
    replace(is.na(.), 0) %>%
    select(-c("Number", "Total.Count"))
}, 1:4, alkprops)

total_catch <- total_catch %>%
  mutate(DiscardMortality = Released.B2 * 0.025)

names(discard_lf) <- c("Length", "2021", "2022", "2023", "2024")
discard_lf_propnj <- discard_lf %>%
  mutate(across(`2021`:`2024`, .fns = function(x) {
    x / sum(x, na.rm = TRUE)
  }))

## Recreational Discard Catch-at-Age
recdiscardCAA <- lapply(1:4, function(a) {
  discardprops <- apply(
    alkprops[[a]][, 2:12], 2,
    function(x) x * discard_lf_propnj[, a + 1]
  )
  yr <- 2020 + a
  totalcatch <- total_catch$DiscardMortality[total_catch$Year == yr]
  discardnums <- as.data.frame(discardprops * totalcatch)
  discardnums$Length <- discard_lf_propnj[, 1]
  discardnums <- discardnums[discardnums$Length %in% 29:60, ]
  discardnums <- discardnums[, c(12, 1:11)]
})

caa2 <- list(
  recdiscardCAA[[1]][, 2:12] + recharvestCAA[[1]][, 2:12], # /1000,
  recdiscardCAA[[2]][, 2:12] + recharvestCAA[[2]][, 2:12], # /1000,
  recdiscardCAA[[3]][, 2:12] + recharvestCAA[[3]][, 2:12], # /1000,
  recdiscardCAA[[4]][, 2:12] + recharvestCAA[[4]][, 2:12] # /1000
)

caa <- cbind(X1 = c(0, 0, 0, 0), rbind(
  apply(recdiscardCAA[[1]][, 2:12] + recharvestCAA[[1]][, 2:12], 2, sum) / 1000,
  apply(recdiscardCAA[[2]][, 2:12] + recharvestCAA[[2]][, 2:12], 2, sum) / 1000,
  apply(recdiscardCAA[[3]][, 2:12] + recharvestCAA[[3]][, 2:12], 2, sum) / 1000,
  apply(recdiscardCAA[[4]][, 2:12] + recharvestCAA[[4]][, 2:12], 2, sum) / 1000
))

waas <- list()

# Length-weight relationships for recreational harvested fish
ggplot(
  data = life_history,
  aes(x = Length, y = Weight, group = as.factor(Year), colour = as.factor(Year))
) +
  geom_point()

lw_relationship_all <- nls(
  Weight ~ alpha * (Length^beta),
  data = life_history, start = c(alpha = 5e6, beta = 3)
)

lw_pars <- bind_rows(lapply(c(1:4), function(i) {
  # if (i < 4) {
  nlstmpnj <- nls(
    Weight ~ alpha * (Length^beta),
    data = filter(life_history, Year == 2020 + i),
    start = c(alpha = 5e6, beta = 3)
  )
  # } else {nls.tmpj <- lw_relationship_all}
  LWdftmpnj <- data.frame(
    Year = 2020 + i,
    a = summary(nlstmpnj)$parameters[1, 1],
    b = summary(nlstmpnj)$parameters[2, 1]
  )
  #preds.tmpnj <- data.frame(Length = min(life_history$Length, na.rm = TRUE):max(life_history$Length, na.rm = TRUE))
  #preds.tmpnj$Pred <- LWdftmpnj$a*((preds.tmpnj$Length)^LWdftmpnj$b)
  #ggplot(data = preds.tmpnj, aes(x = Length, y = Pred)) + geom_point() + ggtitle(paste(2020+i))
}))

alklist <- list(
  alkprops[[1]][, -13],
  alkprops[[2]][, -13],
  alkprops[[3]][, -13],
  alkprops[[4]][, -13]
)

# Find weight of recreational harvested fish.
# There are small sample sizes for commercially harvested,
# so we'll use rec weights to infer comm weights.
names(life_history[[1]])[names(life_history[[1]]) == "Year"] <- "Date"
life_history[[1]]$Year <- as.integer(format(life_history[[1]]$Date, "%Y"))
# life_history24$`Weight (g)` <- life_history24$`Weight (g)` * 1000 #I was given data in the wrong format...
# life_history24$Total.Length..cm. <- life_history24$Total.Length..cm. / 10
# life_history24$`Total Length (cm)` <- life_history24$`Total Length (cm)` / 10
life_history[[2]] <- life_history[[2]] %>%
  rename("Total Length (cm)" = "cm", "Weight (g)" = "grams") %>%
  select(-"Total Length (mm)", -"Weight (kg)")
life_history[[1]] <- life_history[[1]][, names(life_history[[2]])]
life_history <- bind_rows(life_history) %>%
  # rename("Weight" = "Weight..g.") %>%
  rename("Weight" = "Weight (g)") %>%
  # mutate(Length = floor(Total.Length..cm.)) %>%
  mutate(Length = floor(`Total Length (cm)`)) %>%
  select(Year, Age, Weight, Region, Length)
life_history <- life_history[!is.na(life_history$Weight), ]
table(life_history$Year)

ggplot(data = life_history, aes(x = Weight)) +
  geom_histogram() +
  facet_wrap(~Year) +
  xlab("Recreational Fish Weight (g)")
# We only need to use the recreational weights
weights_means <- life_history %>%
  group_by(Year) %>%
  summarize(
    MeanWeight = mean(Weight, na.rm = TRUE),
    sdWeight = sd(Weight, na.rm = TRUE), .groups = "keep"
  )

names(comm_catch)[names(comm_catch) == "NYB-NJ"] <- "Comm" # metric tons
comm_catch$Comm <- comm_catch$Comm * 1000000
commcatchyrsumnj <- comm_catch %>%
  left_join(weights_means) %>%
  mutate(CommCatchNumFish = Comm / MeanWeight)

total_catch <- total_catch %>%
  mutate(TotalRecCatch = Harvest.A.B1 + DiscardMortality)
total_catch <- left_join(total_catch, commcatchyrsumnj) %>%
  mutate(TotalCatch = TotalRecCatch + CommCatchNumFish)

for (y in 1:4) {
  alk.in <- as.data.frame(alklist[[y]])

  # subset to the seasonal comm harvest
  comm.in <- total_catch$Comm[y] # this is in metric tons converted to g

  # Get your yearly L-W pars
  a <- lw_pars[y, 2]
  b <- lw_pars[y, 3]

  # subset your LF
  LF.vec <- harvest_lf[, 1 + y]

  # Convert length bins to weights. Our length bins are floored, so we'll
  # add 0.5 to get to the center of the bin for the weight calculations.
  # If you rounded your lengths, you don't need to add anything.
  W_LBin <- a * ((alk.in[, 1] + 0.5)^b)
  WAAL <- apply(caa2[[y]], 2, function(x) x * W_LBin)
  nums <- apply(caa2[[y]], 2, sum)
  weights <- apply(WAAL, 2, sum)
  waa <- weights / nums
  waas[[y]] <- waa

  print(paste("total weight", 2020 + y))
  print((sum(weights) + comm.in) / 1000000)
}

caa <- as.data.frame(caa) # final output: catch-at-age
mysum <- apply(caa, 1, sum)
prop <- apply(caa[, 1:12], 2, function(x) x / mysum)

# final output: weight-at-age
waa <- cbind(X1 = c(0, 0, 0, 0), bind_rows(waas)) / 1000
# for the ASAP inputs, these appear to be scaled?
write.csv(caa, file.path(root, "caa.csv"))
write.csv(waa, file.path(root, "waa.csv"))
