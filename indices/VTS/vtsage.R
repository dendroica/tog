library(ggplot2, quietly = TRUE, verbose = FALSE)
library(readxl, quietly = TRUE, verbose = FALSE)
library(rmarkdown, quietly = TRUE, verbose = FALSE)
library(knitr, quietly = TRUE, verbose = FALSE)
library(dplyr)
library(tidyr)
root <- Sys.getenv("FILEPATH")

vts16 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_1_2016.xlsx"), sheet = "trap_survey_yr_1")
names(vts16) <- tolower(names(vts16))
#vts16$video <- NULL
names(vts16)[names(vts16)=="length_mm"] <- "total_length_mm"
names(vts16)[names(vts16)=="soak_time days"] <- "soak_time" 

vts17 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_2_2017.xlsx"), sheet = "trap_survey_yr_2")
names(vts17) <- tolower(names(vts17))
#vts17$video <- NULL
names(vts17)[names(vts17)=="length_mm"] <- "total_length_mm"
names(vts17)[names(vts17)=="soak_time_days"] <- "soak_time" 

vts18 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_3_2018.xlsx"), sheet = "NJDEP TrapSurvey_yr_3")
names(vts18) <- tolower(names(vts18))
names(vts18)[names(vts18)=="length_mm"] <- "total_length_mm"
vts18 <- vts18[,which(names(vts18) %in% names(vts17))]

vts19 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_4_2019.xlsx"), sheet = "AllCombined")
names(vts19) <- tolower(names(vts19))
vts19 <- vts19[,which(names(vts19) %in% names(vts17))]

vts20 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_5_2020.xlsx"), sheet = "AllCombined")
names(vts20) <- tolower(names(vts20))
vts20 <- vts20[,which(names(vts20) %in% names(vts17))]

vts21 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_6_2021.xlsx"), sheet = "AllCombined")
names(vts21) <- tolower(names(vts21))
vts21 <- vts21[,which(names(vts21) %in% names(vts17))]

vts22 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_7_2022.xlsx"), sheet = "AllCombined")
names(vts22) <- tolower(names(vts22))
vts22 <- vts22[,which(names(vts22) %in% names(vts17))]

vts23 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_8_2023.xlsx"), sheet = "AllCombined")
names(vts23) <- tolower(names(vts23))
vts23 <- vts23[,which(names(vts23) %in% names(vts17))]

vts24 <- read_excel(file.path(root, "data/tog/RE_ ventless trap survey data",
                              "NJDEP Trap_survey_yr_9_2024.xlsx"), sheet = "AllCombined")
names(vts24) <- tolower(names(vts24))
vts24 <- vts24[,which(names(vts24) %in% names(vts17))]
vts <- rbind(vts16, vts17, vts18, vts19, vts20, vts21, vts22, vts23) %>%
  arrange(haul_date) %>% rename(onoff=`on/off reef`)
vts <- vts[,which(names(vts) %in% names(vts24))]
vts <- rbind(vts, vts24)
vts[,c("season", "species")] <- apply(vts[,c("season", "species")], 2, tolower)
vts[vts$vessel=="reef bound",]$vessel <- "reefbound"
vts[vts$vessel=="resiliance",]$vessel <- "resilience"
vts$reef <- toupper(vts$reef)
vts[grep("u", vts$total_length_mm),]$total_length_mm <- "245"
vts[which(vts$total_length_mm=="NA"),]$total_length_mm <- "1"
vts$total_length_mm <- as.integer(vts$total_length_mm)
vts <- vts %>% filter(total_length_mm > 0 & total_length_mm < 2000)
vts$season <- factor(vts$season, levels=c("spring", "summer", "fall", "winter"))
samples <- vts %>% group_by(year) %>% summarise(sampes = n_distinct(haul_date, reef))

tog <- vts[vts$species=="tautog",]
ocean_len <- tog %>%
  group_by(year, set_date, haul_date, season, reef, soak_time, trap_id, vessel, species, total_length_mm) %>%
  summarise(FREQUENCY = n()) %>% ungroup()
ocean_len[which(ocean_len$total_length_mm==1), "FREQUENCY"] <- 0
ocean_len$LENGTH <- floor(ocean_len$total_length_mm / 10)
### Step 1: Data processing
yrs <- c(2021:2024) # unique(ocean_len$YEAR)
# use min/max below [vs length(yrs)] to account for missing years
out <- matrix(NA, nrow = 100, ncol = length(min(yrs):max(yrs))) # nrow = lenghts = 1:100 cm         # output storage matrix
colnames(out) <- paste0("x", min(yrs):max(yrs))

tes <- ocean_len %>% group_by(reef, LENGTH, year) %>% summarise(freq = sum(FREQUENCY))
test <- ocean_len %>% group_by(reef, year) %>% summarise(nsta = n_distinct(trap_id))
t2 <- merge(tes, test)
t2$meanFREQUENCY <- t2$freq / t2$nsta
out <- t2 %>%
  complete(year, LENGTH=full_seq(29:60, 1), reef, fill=list(meanFREQUENCY=0)) %>% 
  pivot_wider(names_from = "year", values_from = "meanFREQUENCY", values_fill=0)
################################################################################
# Age the index (specify directory where ALKs are):
indir <- "output/tog/alk/filled/opercboth"
alk2021numnj <- read.csv(file.path(root, indir, "NJNYB-ALK_2021_filled.csv"))
alk2022numnj <- read.csv(file.path(root, indir, "NJNYB-ALK_2022_filled.csv"))
alk2023numnj <- read.csv(file.path(root, indir, "NJNYB-ALK_2023_filled.csv"))
alk2024numnj <- read.csv(file.path(root, indir, "NJNYB-ALK_2024_filled.csv"))

alks <- list(alk2021numnj, alk2022numnj, alk2023numnj, alk2024numnj)

alkprops <- lapply(alks, function(y) {
  y <- y %>%
    mutate(rowsum = rowSums(.[grep("X", names(.))], na.rm = TRUE)) %>% # add row sum
    mutate(across(2:12, .fns = function(x) {
      x / rowsum
    })) %>%
    replace(is.na(.), 0) %>%
    rename("FL.cm" = "length") %>%
    select(-rowsum)
})

alkprops <- bind_rows(Map(function(x, y) {
  y <- y %>% mutate("Year" = x)
}, 2021:2024, alkprops))

# alks<- rbind(alk2021numnj, alk2022numnj, alk2023numnj, alk2024numnj) %>%

alks <- alkprops[, c(paste0("X", 2:12), "FL.cm", "Year")]
################################################################################
# Multinomial, seasonal ALK:
################################################################################
wtdLF <- out
wtdLF <- wtdLF[wtdLF$LENGTH > 28 & wtdLF$LENGTH < 61,]
# get length freq proportions
# scaledWtdLF <- t(t(out)/colSums(out))

# age composition of index (storage matrix):
ACs <- matrix(NA, nrow = length(yrs), ncol = 12) # 8 cols = year, plus ages 0:6+
NAAs <- matrix(NA, nrow = length(yrs), ncol = 12)

for (i in 1:length(yrs)) {
  .alk <- alks[alks$Year == yrs[i], 1:11] # & alk$Geo=="North"
  print(nrow(.alk))
  
  wtdLF_annual <- wtdLF[, which(colnames(wtdLF)==as.character(yrs[i]))]
  wtd <- do.call(cbind, apply(.alk, 2, function(x) x * wtdLF_annual))
  names(wtd) <- 2:12
  NAA <- colSums(wtd) # numbers at age
  NAAs[i, ] <- c(yrs[i], NAA)
  PAA <- NAA / sum(NAA) # proportions at age
  ACs[i, 1] <- yrs[i]
  ACs[i, 2:12] <- PAA
}

yrs <- 2016:2020
oldACs <- matrix(NA, nrow = length(yrs), ncol = 12)
NAAs <- matrix(NA, nrow = length(yrs), ncol = 12)
.alk <- alks[alks$Year == 2022, 1:11]

for (i in 1:length(yrs)) {
  print(nrow(.alk))
  wtdLF_annual <- wtdLF[, which(colnames(wtdLF)==as.character(yrs[i]))]
  wtd <- do.call(cbind, apply(.alk, 2, function(x) x * wtdLF_annual))
  names(wtd) <- 2:12
  NAA <- colSums(wtd) # numbers at age
  NAAs[i, ] <- c(yrs[i], NAA)
  PAA <- NAA / sum(NAA) # proportions at age
  oldACs[i, 1] <- yrs[i]
  oldACs[i, 2:12] <- PAA
}
ACs <- cbind(rbind(oldACs,ACs), samples[,2])