library(readxl)

root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)

ssb <- read_xlsx(
  file.path(root, "output/tog/asap/asapinputs_for_R.xlsx"),
  sheet = "SSB WAA"
)

pmat <- read_xlsx(
  file.path(root, "output/tog/asap/asapinputs_for_R.xlsx"),
  sheet = "maturity"
)

P_MAT <- c(0, 0, 0.8, rep(1, 9))

sbpr(
  age = 1:12,
  ssbwgt = unname(unlist(as.vector(as.data.frame(ssb[1, 2:13])))),
  partial = c(
    0.002414456, 0.010225583, 0.04223707, 0.158422558, 0.445541409,
    0.774292042, 0.93610729, 0.984298204, 0.996313169, 0.999170831,
    0.999842733, 1
  ),
  pmat = P_MAT,
  M = 0.15,
  pF = 0.2, # change - Lindy's AGEPRO FILE
  pM = 0.1667, # change (same)
  plus = TRUE,
  maxF = 2,
  incrF = 0.001,
  MSP = 40,
  options = c(1, 3),
  oldest = 100
)
