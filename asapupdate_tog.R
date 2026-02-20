library(wham)
endyr <- 2024
asap <- read_asap3_dat("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/FINAL/ORIG.DAT")
n <- endyr - asap[[1]]$dat$R_avg_end
asap[[1]]$dat$R_avg_end <- endyr
asap[[1]]$dat$n_years <- asap[[1]]$dat$n_years + n
asap[[1]]$dat$M <- rbind(asap[[1]]$dat$M, do.call("rbind", replicate(n, asap[[1]]$dat$M[1,], simplify = FALSE)))
