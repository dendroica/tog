library(wham)
library(readxl)
source("4-catch_at_age.R") #caa
load(file.path(Sys.getenv("FILEPATH"), "output/tog/index/NYWLI_index.RData")) #index.out_NY
source("./indices/NJOT/3a-analysis_nogam.R") #index.out_nj
source("./indices/NJOT/4-age.R") #agecomp
mrip <- read_xlsx(
  file.path(root, "data/tog/MRIP indices tautog 1981-2024.xlsx"),
  sheet = "NJNYB"
)[, c("Year", "CPUE", "CV")]
asap <- read_asap3_dat(file.path(Sys.getenv("FILEPATH"), "output/tog/asap/FINAL/ORIG.DAT"))

waa0 <- caa[[1]]
caa_out <- caa[[2]]
total_weight <- caa[[3]]
#load model workspaces for IAA

#GETS UPDATED##############
endyr <- 2024
n <- endyr - asap[[1]]$dat$R_avg_end
asap[[1]]$dat$R_avg_end <- endyr
asap[[1]]$dat$n_years <- asap[[1]]$dat$n_years + n
asap[[1]]$dat$WAA_mats[[1]] <- rbind(asap[[1]]$dat$WAA_mats[[1]], waa0)
asap[[1]]$dat$CAA_mats[[1]] <- rbind(asap[[1]]$dat$CAA_mats[[1]],
                                     cbind(caa_out, total_weight))
#STAYS THE SAME############
asap[[1]]$dat$M <- rbind(asap[[1]]$dat$M,
                         do.call("rbind", 
                                 replicate(n, 
                                           asap[[1]]$dat$M[1,],
                                           simplify = FALSE)))
asap[[1]]$dat$maturity <- rbind(asap[[1]]$dat$maturity,
                                do.call("rbind",
                                        replicate(n,
                                                  asap[[1]]$dat$maturity[1,],
                                                  simplify = FALSE)))
asap[[1]]$dat$WAA_mats[[2]] <- rbind(asap[[1]]$dat$WAA_mats[[2]],
                                     do.call("rbind",
                                             replicate(n,
                                                       asap[[1]]$dat$WAA_mats[[2]][1,],
                                                       simplify = FALSE)))

asap[[1]]$dat$DAA_mats[[1]] <- rbind(asap[[1]]$dat$DAA_mats[[1]],
                                     do.call("rbind",
                                             replicate(n,
                                                       asap[[1]]$dat$DAA_mats[[1]][1,],
                                                       simplify = FALSE)))
asap[[1]]$dat$prop_rel_mats[[1]] <- rbind(asap[[1]]$dat$prop_rel_mats[[1]],
                                          do.call("rbind",
                                                  replicate(n,
                                                            asap[[1]]$dat$prop_rel_mats[[1]][1,],
                                                            simplify = FALSE)))
asap[[1]]$dat$recruit_cv <- c(asap[[1]]$dat$recruit_cv,
                              rep(asap[[1]]$dat$recruit_cv[1], n))
asap[[1]]$dat$discard_cv <- c(asap[[1]]$dat$discard_cv,
                              rep(asap[[1]]$dat$discard_cv[1], n))
asap[[1]]$dat$discard_Neff <- c(asap[[1]]$dat$discard_Neff,
                                rep(asap[[1]]$dat$discard_Neff[1], n))

#SELECTIVITY BLOCKS
#would need to change this if you are adding a selectivity block
#the line below assumes you're not and adds this new data to the last block
asap[[1]]$dat$sel_block_assign[[1]] <- c(asap[[1]]$dat$sel_block_assign[[1]],
                                         rep.int(asap[[1]]$dat$sel_block_assign[[1]][length(asap[[1]]$dat$sel_block_assign[[1]])],
                                                 n))

#need to add
asap[[1]]$dat$catch_cv #add values for new years
asap[[1]]$dat$catch_Neff #add values for new years

#index data
asap[[1]]$dat$IAA_mats[[1]] <- unname(as.matrix(index.out_NY[index.out_NY$Year <= endyr,c("Year", "Index", "CV")]))

olddata <- asap[[1]]$dat$IAA_mats[[2]][,c(1,4:ncol(asap[[1]]$dat$IAA_mats[[2]]))]
updatedata <- rbind(olddata, c(2021, rep(-1, ncol(olddata)-2), 0), agecomp)
asap[[1]]$dat$IAA_mats[[2]] <- unname(as.matrix(cbind(index.out_nj[,c("YEAR", "Index", "CV")], updatedata[,2:14])))
#MRIP asap[[1]]$dat$IAA_mats[[3]]
mrip <- mrip[mrip$Year >= asap[[1]]$dat$year1,]
