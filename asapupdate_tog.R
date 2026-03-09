library(wham)
source("4-catch_at_age.R") #waa0
endyr <- 2024
asap <- read_asap3_dat("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/FINAL/ORIG.DAT")
n <- endyr - asap[[1]]$dat$R_avg_end
asap[[1]]$dat$R_avg_end <- endyr
asap[[1]]$dat$n_years <- asap[[1]]$dat$n_years + n
asap[[1]]$dat$M <- rbind(asap[[1]]$dat$M, do.call("rbind", replicate(n, asap[[1]]$dat$M[1,], simplify = FALSE)))
asap[[1]]$dat$maturity <- rbind(asap[[1]]$dat$maturity,
                                do.call("rbind",
                                        replicate(n,
                                                  asap[[1]]$dat$maturity[1,],
                                                  simplify = FALSE)))
asap[[1]]$dat$WAA_mats[[1]] <- rbind(asap[[1]]$dat$WAA_mats[[1]], waa0)
asap[[1]]$dat$WAA_mats[[2]] <- rbind(asap[[1]]$dat$WAA_mats[[2]],
                                     do.call("rbind",
                                             replicate(n,
                                                       asap[[1]]$dat$WAA_mats[[2]][1,],
                                                       simplify = FALSE)))
asap[[1]]$dat$sel_block_assign[[1]] <- c(asap[[1]]$dat$sel_block_assign[[1]], rep.int(5, n))
asap[[1]]$dat$CAA_mats[[1]] <- rbind(asap[[1]]$dat$CAA_mats[[1]], cbind(caa_out, total_weight))
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