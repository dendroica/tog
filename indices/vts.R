library(readxl)
library(dplyr)
base_path <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(base_path, usr, loc)

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
  arrange(haul_date) 

counts <- vts %>%
  group_by(year, haul_date, reef, soak_time, trap_id, material, species) %>%
  summarise(count = n()) %>% ungroup()
filled <- counts %>%
  complete(species, nesting(year, haul_date, trap_id, reef, soak_time, material),
           fill=list(count=0)) %>% filter(species == "Tautog")

mod <- as.formula("Tautog ~ Year + SurfaceTemp + Salinity + DO")
bmc <- buildmerControl(include= ~ Year) #+ (1|site)
nb <- buildglmmTMB(mod, dat, family = nbinom2, 
                   buildmerControl = bmc)