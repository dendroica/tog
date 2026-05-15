#This is for an assessment update, updating the ASAP file with new years' data
#For the new years: need CAA, WAA, total weights, MRIP ages, ages for your indices
#For all years: update indices and CV, ESS
library(readxl)
source("./WHAM/asapwrite.R")
source("./WHAM/asap_update.R")

source("4-catch_at_age.R") #caa
#INDEX DATA
mrip <- read_xlsx(
  file.path(root, "data/tog/MRIP indices tautog 1981-2024.xlsx"),
  sheet = "NJNYB"
)[, c("Year", "CPUE", "CV")]
ess <- read_xlsx(
  file.path(root, "data/tog/rec/Tautog_Regional_ESS_1982-2023.xlsx")
)
source("./indices/MRIP.R") #mrip_prop
source("./indices/NJOT/4-age.R") #agecomp
#these would require time-intensive model reruns
load(file.path(Sys.getenv("FILEPATH"), "output/tog/index/NYWLI_index.RData")) #index.out_NY
source("./indices/NJOT/3a-analysis_nogam.R") #index.out_nj
index1 <- index.out_NY
index2 <- index.out_nj
names(index2)[1:3] <- c("Year", "Index", "CV")
index2age <- agecomp

index <- list(list(1, index1),
              list(2, index2, index2age, c(0, 35, 35, 0)))
#the last update ASAP file
asap <- read_asap3_dat(file.path(Sys.getenv("FILEPATH"), "data/tog/NJ-NYB_BASE_RUN_2021.DAT"))

waa0 <- caa[[1]]
caa_out <- caa[[2]]
total_weight <- caa[[3]]

endyr <- 2024
fileout <- file.path(Sys.getenv("FILEPATH"), "output/tog/asap/writetest/2024update-test.dat")
########
AssessUpdate(asap, endyr, caa_out, waa0, total_weight, index, mrip, mrip_prop, ess, fileout)
source("./indices/VTS/vtsupdate.R") #could fold this into this script