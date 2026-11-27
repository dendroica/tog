library(wham)
asap <- read_asap3_dat(file.path(Sys.getenv("FILEPATH"),"/output/tog/asap/FINAL/ORIG.DAT"))
#will have to update for new indices
asap[[1]]$dat$n_fleet_sel_blocks #are you going to add sensitivity blocks?

#IF YOU ADD A SELECTIVITY BLOCK
asap[[1]]$dat$sel_block_option #if you add a selectivity block, update options