library(wham)
source("./WHAM/asapwrite.R")
#make agecomp optional for all indices: generalize index ingestion
#specify which index is MRIP instead of hard code
#put new index code here
AssessUpdate <- function(asap, endyr, caa_out, waa0, total_weight, index1, index2, index2age, mrip, mrip_prop, ess, fileout) {
n <- endyr - asap[[1]]$dat$R_avg_end
asap[[1]]$dat$R_avg_end <- endyr
asap[[1]]$dat$n_years <- as.integer(asap[[1]]$dat$n_years + n)
asap[[1]]$dat$WAA_mats[[1]] <- rbind(asap[[1]]$dat$WAA_mats[[1]], unname(as.matrix(waa0)))
asap[[1]]$dat$CAA_mats[[1]] <- rbind(asap[[1]]$dat$CAA_mats[[1]],
                                     unname(as.matrix(cbind(caa_out, total_weight))))
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
#index data
asap[[1]]$dat$IAA_mats[[1]] <- unname(as.matrix(index1[index1$Year <= endyr,c("Year", "Index", "CV")]))

olddata <- asap[[1]]$dat$IAA_mats[[2]][,c(1,4:ncol(asap[[1]]$dat$IAA_mats[[2]]))]
updatedata <- rbind(olddata, c(2021, rep(-1, ncol(olddata)-2), 0), index2age)
asap[[1]]$dat$IAA_mats[[2]] <- unname(as.matrix(cbind(index2[,c("YEAR", "Index", "CV")], updatedata[,2:14])))

mrip <- mrip[mrip$Year >= asap[[1]]$dat$year1,]
asap[[1]]$dat$catch_cv <- mrip$CV
ess <- ess[ess$YEAR >= asap[[1]]$dat$year1,]
asap[[1]]$dat$catch_Neff <- ess$`NJ-NYB`
olddata <- asap[[1]]$dat$IAA_mats[[3]][,4:(ncol(asap[[1]]$dat$IAA_mats[[2]])-1)]
updatedata <- rbind(olddata, mrip_prop)
asap[[1]]$dat$IAA_mats[[3]] <- unname(as.matrix(cbind(mrip[, c("Year", "CPUE", "CV")], updatedata, ess$`NJ-NYB`)))
writeoutasap(asap, fileout)
return(asap)}
