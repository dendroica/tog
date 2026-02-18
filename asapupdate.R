library(wham)
asap <- read_asap3_dat("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/FINAL/ORIG.DAT")
#will have to update for new indices
asap[[1]]$dat$n_years #needs to be updated to number of years
asap[[1]]$dat$n_fleet_sel_blocks #are you going to add sensitivity blocks?
asap[[1]]$dat$M #add rows for new years
asap[[1]]$dat$maturity #add rows for new years
asap[[1]]$dat$WAA_mats[[1]] #add rows for new years
asap[[1]]$dat$WAA_mats[[2]] #add rows for new years
asap[[1]]$dat$sel_block_assign[[1]] #add values for new years
asap[[1]]$dat$sel_block_option #if you add a selectivity block, update options
asap[[1]]$dat$CAA_mats[[1]] #add rows for new years
asap[[1]]$dat$DAA_mats[[1]] #add rows for new years
asap[[1]]$dat$prop_rel_mats[[1]] #add rows for new years

#index data
asap[[1]]$dat$IAA_mats[[1]] #add data for new years
asap[[1]]$dat$IAA_mats[[2]] #add data for new years
asap[[1]]$dat$IAA_mats[[3]] #add data for new years

asap[[1]]$dat$recruit_cv #add values for new years
asap[[1]]$dat$catch_cv #add values for new years
asap[[1]]$dat$discard_cv #add values for new years
asap[[1]]$dat$catch_Neff #add values for new years
asap[[1]]$dat$discard_Neff #add values for new years

asap[[1]]$dat$R_avg_end #update to be last year