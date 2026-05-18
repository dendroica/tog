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
root <- Sys.getenv("FILEPATH")
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

# Length Frequencies for Recreational Catch and Discard
harvest_lf <- read.csv(file.path(root, "output/tog/harvest_LF.csv"))
# Modified from discard_LF.R from Samarah Nehemiah for LIS
discard_lf <- read.csv(file.path(root, "output/tog/discard_mripLF.csv"))[, 2:6]
#########################################
names(harvest_lf) <- c("Length", "2021", "2022", "2023", "2024")
min_age <- 2
max_age <- 12
min_len <- 29
# Convert to proportions
alk_props <- lapply(alks, function(y) {
  y <- y |>
    rowwise() |>
    mutate(rowsum = sum(c_across(matches("\\d")), na.rm = TRUE)) |>
    ungroup() |>
    mutate(across(2:ncol(y), .fns = function(x) {
      x / rowsum
    })) |>
    mutate(across(where(is.numeric), ~ replace_na(., 0))) |>
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
    mutate(across(where(is.numeric), ~ replace_na(., 0))) |>
    select(-c("Number")) |>
    complete(Length = full_seq(Length, period = 1)) %>%
    replace(is.na(.), 0)
  return(annual_rec_har)
}, 2021:2024, alk_props)
rec_harvest_caa_annual <- bind_rows(lapply(rec_harvest_caa, function(x) {
  apply(x, 2, sum)
})) %>% select(-Length)
rec_harvest_caa_annual <- cbind(X1 = c(0, 0, 0, 0), rec_harvest_caa_annual)

names(discard_lf) <- c("Length", "2021", "2022", "2023", "2024")
discard_prop <- discard_lf |>
  mutate(across(`2021`:`2024`, .fns = function(x) {
    x / sum(x, na.rm = TRUE)
  })) |>
  filter(Length >= min_len & Length <= 60)
if (max(discard_prop$Length) < 60) {
  gap <- 60 - max(discard_prop$Length)
  fill <- data.frame(
    (max(discard_prop$Length) + 1):60, rep(0, gap), rep(0, gap),
    rep(0, gap), rep(0, gap)
  )
  names(fill) <- names(discard_prop)
  discard_prop <- rbind(discard_prop, fill)
}
## Recreational Discard Catch-at-Age
rec_discard_caa <- Map(function(yr, alk_prop) {
  aged_discard_props <- apply(
    alk_prop[, 2:ncol(alk_prop)], 2, function(x) {
      x * discard_prop[, as.character(yr)]
    }
  )
  discards <- total_rec_catch$Released.B2[total_rec_catch$Year == yr]
  aged_discards <- as.data.frame(aged_discard_props * discards)
  aged_discards$Length <- min_len:60
  aged_discards <- aged_discards[, c("Length", paste0("X", min_age:max_age))]
}, 2021:2024, alk_props)
rec_discard_caa_annual <- bind_rows(lapply(rec_discard_caa, function(x) {
  apply(x, 2, sum)
})) %>% select(-Length)
rec_discard_caa_annual <- cbind(X1 = c(0, 0, 0, 0), rec_discard_caa_annual)

total <- rec_discard_caa_annual + rec_harvest_caa_annual
annual_sum <- apply(total, 1, sum)
mrip_prop <- apply(total[, 1:12], 2, function(x) x / annual_sum)
