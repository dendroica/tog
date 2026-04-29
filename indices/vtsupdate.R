library(wham)
source("indices/vtsage.R")
source("WHAM/asapwrite.R")
load("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/vtsindex.RData")
asap <- read_asap3_dat("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/FINAL/ORIG.DAT")
vts <- read_asap3_dat("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/vts.DAT")
asap[[1]]$dat$n_indices <- 4
#in my case, I used all the same options as the ocean trawl
#but look at these under Index Specification, Index Selectivity
asap[[1]]$dat$index_units <- c(asap[[1]]$dat$index_units, asap[[1]]$dat$index_units[length(asap[[1]]$dat$index_units)])
asap[[1]]$dat$index_acomp_units <- c(asap[[1]]$dat$index_acomp_units, asap[[1]]$dat$index_acomp_units[length(asap[[1]]$dat$index_acomp_units)])
asap[[1]]$dat$index_month <- c(asap[[1]]$dat$index_month, asap[[1]]$dat$index_month[length(asap[[1]]$dat$index_month)])
asap[[1]]$dat$index_sel_choice <- c(asap[[1]]$dat$index_sel_choice, asap[[1]]$dat$index_sel_choice[length(asap[[1]]$dat$index_sel_choice)])
asap[[1]]$dat$index_sel_start_age <- c(asap[[1]]$dat$index_sel_start_age, asap[[1]]$dat$index_sel_start_age[length(asap[[1]]$dat$index_sel_start_age)])
asap[[1]]$dat$index_sel_end_age <- c(asap[[1]]$dat$index_sel_end_age, asap[[1]]$dat$index_sel_end_age[length(asap[[1]]$dat$index_sel_end_age)])
asap[[1]]$dat$index_WAA_pointers <- c(asap[[1]]$dat$index_WAA_pointers, asap[[1]]$dat$index_WAA_pointers[length(asap[[1]]$dat$index_WAA_pointers)])
asap[[1]]$dat$use_index_acomp <- c(asap[[1]]$dat$use_index_acomp, asap[[1]]$dat$use_index_acomp[length(asap[[1]]$dat$use_index_acomp)])
asap[[1]]$dat$use_index <- c(asap[[1]]$dat$use_index, asap[[1]]$dat$use_index[length(asap[[1]]$dat$use_index)])

asap[[1]]$dat$index_sel_option <- c(asap[[1]]$dat$index_sel_option, asap[[1]]$dat$index_sel_option[length(asap[[1]]$dat$index_sel_option)])
asap[[1]]$dat$index_sel_ini[[4]] <- asap[[1]]$dat$index_sel_ini[[2]]

vtsindex <- unname(as.matrix(cbind(index.out[,c("Year", "Index", "CV")], age1=0,ACs[,2:13])))
filled <- 1989:2015
filled <- unname(cbind(filled, matrix(-1, 27, 15)))
asap[[1]]$dat$IAA_mats[[4]] <- rbind(filled,vtsindex)
asap[[1]]$dat$index.names <- c(asap[[1]]$dat$index.names, "VTS")
asap[[1]]$index.names <- c(asap[[1]]$index.names, "VTS")

asap[[1]]$dat$lambda_index <- c(asap[[1]]$dat$lambda_index, asap[[1]]$dat$lambda_index[length(asap[[1]]$dat$lambda_index)])
asap[[1]]$dat$lambda_q <- c(asap[[1]]$dat$lambda_q, asap[[1]]$dat$lambda_q[length(asap[[1]]$dat$lambda_q)])
asap[[1]]$dat$cv_q <- c(asap[[1]]$dat$cv_q, asap[[1]]$dat$cv_q[length(asap[[1]]$dat$cv_q)])
asap[[1]]$dat$lambda_q_devs <- c(asap[[1]]$dat$lambda_q_devs, asap[[1]]$dat$lambda_q_devs[length(asap[[1]]$dat$lambda_q_devs)])
asap[[1]]$dat$cv_q_devs <- c(asap[[1]]$dat$cv_q_devs, asap[[1]]$dat$cv_q_devs[length(asap[[1]]$dat$cv_q_devs)])

asap[[1]]$dat$q_ini <- c(asap[[1]]$dat$q_ini, asap[[1]]$dat$q_ini[length(asap[[1]]$dat$q_ini)])

asap[[1]]$comments <- append(asap[[1]]$comments, "# Index-4 Selectivity Data ", after = 43)
asap[[1]]$comments <- append(asap[[1]]$comments, "# Index-4 Data ", after = 47)

writeoutasap(asap, "C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/vts.dat")
