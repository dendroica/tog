library(wham)
n <- 4 #number of new years
asap <- read_asap3_dat("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/FINAL/ORIG.DAT")
asap[[1]]$dat$n_years <- asap[[1]]$dat$n_years + n
asap[[1]]$dat$M <- rbind(asap[[1]]$dat$M, do.call("rbind", replicate(n, asap[[1]]$dat$M[1,], simplify = FALSE)))
