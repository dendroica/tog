# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This script calculates total catch in numbers of fish
# the length frequencies of different components of catch
# and combines those with the filled-in ALKs to calculate catch-at-age...
# ...then weight-at-age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)
library(ggplot2)

# INPUTS USED#############################
root <- "C:/Users"
usr <- "jgorzo" #"galax" #"jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)
# root <- "/media/jess/9CE61C02E61BDB7A/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents"
# On Ubuntu, you have to have this open in files/ssd to have it mounted...
# Load ALKs
alks <- list.files(file.path(root, "output/tog/alk/filled/opercboth"),
  full.names = TRUE
) |> map(read.csv)
# Folder contains...
# NJNYB-ALK_2021_filled.csv
# NJNYB-ALK_2022_filled.csv
# NJNYB-ALK_2023_filled.csv
# NJNYB-ALK_2024_filled.csv

# Load recreational catch and discard
total_rec_catch <- read.csv(
  file.path(root, "data/tog/rec/Tautog_MRIP_TotalCatch_2021-2024_NJNYB.csv"),
  header = TRUE
)

# Load commercial catch data
comm_catch <- read_xlsx(
  file.path(root, "data/tog/Regional_Comm_landings_MT_07.18.25.xlsx"),
  sheet = "MT"
)[, c("Year", "NYB-NJ")]

# Length Frequencies for Recreational Catch and Discard
harvest_lf <- read.csv(file.path(root, "output/tog/harvest_LF.csv"))
# Modified from discard_LF.R from Samarah Nehemiah for LIS
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
names(harvest_lf) <- c("Length", "2021", "2022", "2023", "2024")
min_age <- 1
max_age <- 12
min_len <- 17
# Convert to proportions
alk_props <- lapply(alks, function(y) {
  y <- y |> rowwise() |>
    mutate(rowsum = sum(c_across(matches("\\d")), na.rm = TRUE)) |> ungroup() |>
    mutate(across(2:ncol(y), .fns = function(x) {
      x / rowsum
    })) |> mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    rename("Length" = "length") |>
    select(-rowsum)
})
names(alk_props) <- c(2021:2024)

rec_harvest_caa <- Map(function(x, y) {
  annual_rec_har <- left_join(harvest_lf[, c("Length", as.character(x))], y) |>
    rename(Number = as.character(x)) |>
    mutate(across(paste0("X", min_age:max_age), .fns = function(x) {
      x * Number
    })) |>
    mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    select(-c("Number")) |> complete(Length=full_seq(Length, period=1)) %>%
    replace(is.na(.), 0)
  return(annual_rec_har)
}, 2021:2024, alk_props)
rec_harvest_caa_annual <- bind_rows(lapply(rec_harvest_caa, function(x) {
  apply(x, 2, sum)
})) %>% select(-Length)
rec_harvest_caa_annual <- cbind(X1=c(0,0,0,0), rec_harvest_caa_annual)

names(discard_lf) <- c("Length", "2021", "2022", "2023", "2024")
discard_prop <- discard_lf |>
  mutate(across(`2021`:`2024`, .fns = function(x) {
    x / sum(x, na.rm = TRUE)
  })) |>
  filter(Length >= min_len & Length <= 60)

total_rec_catch <- total_rec_catch |> # number of fish
  mutate(discard_mortality = Released.B2 * 0.025)
## Recreational Discard Catch-at-Age
rec_discard_caa <- Map(function(yr, alk_prop) {
  aged_discard_props <- apply(
    alk_prop[, 2:ncol(alk_prop)], 2, function(x) {
      x * discard_prop[, as.character(yr)]
    }
  )
  discards <- total_rec_catch$discard_mortality[total_rec_catch$Year == yr]
  aged_discards <- as.data.frame(aged_discard_props * discards)
  aged_discards$Length <- min_len:60
  aged_discards <- aged_discards[, c("Length", paste0("X", min_age:max_age))]
}, 2021:2024, alk_props)
rec_discard_caa_annual <- bind_rows(lapply(rec_discard_caa, function(x) {
  apply(x, 2, sum)
})) %>% select(-Length)
rec_discard_caa_annual <- cbind(X1=c(0,0,0,0), rec_discard_caa_annual)
# Length-weight relationships for recreational harvested fish
# ggplot(
#  data = life_history,
#  aes(x = Length, y = Weight, group = as.factor(Year), colour = as.factor(Year))
# ) +
#  geom_point()

names(life_history[[1]])[names(life_history[[1]]) == "Year"] <- "Date"
life_history[[1]]$Year <- as.integer(format(life_history[[1]]$Date, "%Y"))
life_history[[2]] <- life_history[[2]] |>
  rename("Total Length (cm)" = "cm", "Weight (g)" = "grams") |>
  select(-"Total Length (mm)", -"Weight (kg)")
life_history[[1]] <- life_history[[1]][, names(life_history[[2]])]
life_history <- bind_rows(life_history) |>
  rename("Weight" = "Weight (g)") |>
  mutate(Length = floor(`Total Length (cm)`)) |>
  select(Year, Age, Weight, Region, Length)
life_history <- life_history[!is.na(life_history$Weight), ]
table(life_history$Year)

lw_relationship_all <- nls(
  Weight ~ alpha * (Length^beta),
  data = life_history, start = c(alpha = 5e6, beta = 3)
)

lw_pars <- bind_rows(lapply(2021:2024, function(i) {
  # if (i < 2024) {
  annual_lw <- nls(
    Weight ~ alpha * (Length^beta),
    data = filter(life_history, Year == i),
    start = c(alpha = 5e6, beta = 3)
  )
  # } else {nls.tmpj <- lw_relationship_all}
  params <- data.frame(
    Year = i,
    a = summary(annual_lw)$parameters[1, 1],
    b = summary(annual_lw)$parameters[2, 1]
  )
  # preds.tmpnj <- data.frame(Length = min(life_history$Length, na.rm = TRUE):max(life_history$Length, na.rm = TRUE))
  # preds.tmpnj$Pred <- params$a*((preds.tmpnj$Length)^params$b)
  # ggplot(data = preds.tmpnj, aes(x = Length, y = Pred)) + geom_point() + ggtitle(paste(2020+i))
  return(params)
}))

ggplot(data = life_history, aes(x = Weight)) +
  geom_histogram() +
  facet_wrap(~Year) +
  xlab("Recreational Fish Weight (g)")
# We only need to use the recreational weights
weights_means <- life_history |>
  group_by(Year) |>
  summarize(
    MeanWeight = mean(Weight, na.rm = TRUE),
    sdWeight = sd(Weight, na.rm = TRUE), .groups = "keep"
  )

total_rec_catch <- total_rec_catch |> # numbers of fish
  mutate(TotalRecCatch = Harvest.A.B1 + discard_mortality)

names(comm_catch)[names(comm_catch) == "NYB-NJ"] <- "comm" # metric tons
comm_catch$comm <- comm_catch$comm * 1000000
comm_catch <- comm_catch |>
  left_join(weights_means) |>
  mutate(commCatchNumFish = comm / MeanWeight)

total_catch <- left_join(total_rec_catch, comm_catch) |>
  mutate(Total.Catch = TotalRecCatch + commCatchNumFish)

caals <- Map(function(x, y) {
  x[, 2:12] + y[, 2:12]
}, rec_discard_caa, rec_harvest_caa)

# Find weight of recreational harvested fish.
# There are small sample sizes for commercially harvested,
# so we'll use rec weights to infer comm weights.
waa <- Map(function(alk, yr, caal) {
  # Get your yearly L-W pars
  b <- lw_pars[lw_pars$Year == yr, "b"]
  # Convert length bins to weights. Our length bins are floored, so we'll
  # add 0.5 to get to the center of the bin for the weight calculations.
  # If you rounded your lengths, you don't need to add anything.
  wl <- lw_pars[lw_pars$Year == yr, "a"] * ((alk[, 1] + 0.5)^b)
  waal <- do.call(cbind, apply(caal, 2, function(caal_annual) caal_annual * wl)) # how much weight is in each AL bin
  names(waal) <- paste0("X",2:12)
  weights <- apply(waal, 2, sum) # total weight in each age bin
  caa <- apply(caal, 2, sum) # catch-at-age
  waa <- weights / caa # average weight per fish

  print(paste("total weight", yr))
  comm <- total_catch[total_catch$Year == yr, ]$comm # this is in metric tons converted to g
  print((sum(weights) + comm) / 1000000)
  return(waa)
}, alk_props, 2021:2024, caals)

caa_out <- as.data.frame(cbind(X1 = c(0, 0, 0, 0), rbind(
  t(sapply(caals, function(x) {
    apply(x, 2, sum) / 1000
  }))
))) # Final output: catch-at-age
annual_sum <- apply(caa_out, 1, sum)
caa_prop <- apply(caa_out[, 1:12], 2, function(x) x / annual_sum)

# final output: weight-at-age
waa <- cbind(X1 = c(0, 0, 0, 0), bind_rows(waa)) / 1000 # for the ASAP inputs, these appear to be scaled?
# OUTPUTS#############
# write.csv(caa_out, file.path(root, "output/tog/caa.csv"))
# write.csv(waa, file.path(root, "output/tog/waa.csv"))
waa0 <- waa
waa0[is.na(waa0)] <- 0
discard_weights <- apply(waa0 * rec_discard_caa_annual, 1, sum) * .001
harvest_weights <- apply(waa0 * rec_harvest_caa_annual, 1, sum) * .001
