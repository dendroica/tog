library(wham)
asap <- read_asap3_dat("C:/Users/galax/OneDrive - New Jersey Office of Information Technology/Documents/output/tog/asap/FINAL/ORIG.DAT")
input_asap <- prepare_wham_input(asap)
nofit_asap <- fit_wham(input_asap, do.fit=F)
fit_asap <- fit_wham(input_asap, do.retro = FALSE, do.osa = FALSE, do.sdrep = FALSE, do.brps = FALSE)
fit_asap <- do_sdreport(fit_asap)
fit_asap$sdrep
fit_asap <- do_reference_points(fit_asap, do.sdrep = TRUE)
fit_asap <- do_retro_peels(fit_asap)
fit_asap <- make_osa_residuals(fit_asap)
tmp.dir <- tempdir(check=TRUE)
plot_wham_output(fit_asap, dir.main = tmp.dir)

#selectivity <- list(model = c("age-specific", "logistic", "logistic"))
selectivity$model <- rep("age-specific", 3)
selectivity$initial_pars <- list(
  c(0.5,0.5,0.5,1,0.5, 0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$fix_pars <- list(4,4,4)
input_3 <- set_selectivity(input, selectivity = selectivity)

F_opts <- list(
  F = cbind(rep(5,input$data$n_years_model)),
  map_F = cbind(rep(NA, input$data$n_years_model)))
selectivity$re <- c("iid", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars[[1]] <- rep(0.1,6)
selectivity$map_pars <- list(c(1,1,1,1,1,1),c(2:4,NA,5:6), c(7:9,NA,10:11))
input_4 <- set_selectivity(input, selectivity = selectivity)
input_4 <- set_F(input_4, F_opts)
# This one takes a while
fit_4 <- fit_wham(input_4, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_4, dir.main = tmp.dir)

selectivity <- list(model = rep("age-specific", 3))
selectivity$re <- c("iid", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
  c(rep(0.5,4),1,0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,NA,1),c(2:4,NA,5:6), c(7:9,NA,10:11))
input_5 <- set_selectivity(input, selectivity = selectivity)
fit_5 <- fit_wham(input_5, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_5, dir.main = tmp.dir)

selectivity <- list(model = rep("age-specific", 3))
selectivity$re <- c("ar1_y", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
  c(rep(0.5,4),1,0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1:4,NA,5),c(6:8,NA,9:10), c(11:13,NA,14:15))
input_6 <- set_selectivity(input, selectivity = selectivity)
fit_6 <- fit_wham(input_6, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_6, dir.main = tmp.dir)

selectivity$re <- c("ar1", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
  c(rep(0.5,4),1,0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,NA,1),c(2:4,NA,5:6), c(7:9,NA,10:11))
input_7 <- set_selectivity(input, selectivity = selectivity)
fit_7 <- fit_wham(input_7, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_7, dir.main = tmp.dir)

selectivity$re <- c("2dar1", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
  c(rep(0.5,4),1,0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5),
  c(0.5,0.5,0.5,1,0.5, 0.5))
selectivity$map_pars <- list(c(1,1,1,1,NA,1),c(2:4,NA,5:6), c(7:9,NA,10:11))
input_8 <- set_selectivity(input, selectivity = selectivity)
fit_8 <- fit_wham(input_8, do.sdrep = FALSE, do.osa = FALSE, do.retro = FALSE)
#Does not converge...
plot_wham_output(fit_8, dir.main = tmp.dir)

selectivity <- list(model = rep("logistic", 3))
selectivity$re <- c("iid", "none", "none")
selectivity$fix_pars <- NULL
selectivity$initial_pars <- list(
  c(3, 0.2), 
  c(3, 0.2), 
  c(3, 0.2)) 
input_9 <- set_selectivity(input, selectivity = selectivity)
fit_9 <- fit_wham(input_9, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_9, dir.main = tmp.dir)

input <- set_selectivity(input_asap, selectivity)
fit_0 <- fit_wham(input, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_0, dir.main = tmp.dir)
#2 fixed effects
input_1 <- set_NAA(input, NAA_re = list(N1_model = "equilibrium"))
fit_1 <- fit_wham(input_1, do.retro=FALSE, do.osa=FALSE)
#2 fixed effects, 6 random effects
input_2 <- set_NAA(input, NAA_re = list(N1_model = "iid-re"))
fit_2 <- fit_wham(input_2, do.retro=FALSE, do.osa=FALSE)
#3 fixed effects, 6 random effects
input_3 <- set_NAA(input, NAA_re = list(N1_model = "ar1-re"))
fit_3 <- fit_wham(input_3, do.retro=FALSE, do.osa=FALSE)
fit_3 <- fit_wham(input_3, do.sdrep = TRUE, do.osa = FALSE, do.retro = FALSE)
plot_wham_output(fit_3, dir.main = tmp.dir)

N1 <- sapply(0:3, function(x) get(paste0("fit_", x))$rep$NAA[1,1,1,])
matplot(N1, type = 'l', lwd = 2, lty = 1, ylab = "Initial Abundance", xlab = "Age")
legend(x = "topright", legend = paste0("fit_", 0:3), col = 1:4, lty = 1, lwd = 2)

SSB <- sapply(0:3, function(x) get(paste0("fit_", x))$rep$SSB)
matplot(SSB, type = 'l', lty = 1, ylab = "SSB", xlab = "Year", lwd = 2)
legend(x = "topright", legend = paste0("fit_", 0:3), col = 1:4, lty = 1, lwd = 2)

NAA_list <- list(
  sigma = "rec",
  cor = "iid")
input_4 <- set_NAA(input, NAA_list)
fit_4 <- fit_wham(input_4, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_4, dir.main = tmp.dir)

NAA_list <- list(
  sigma = "rec",
  cor = "ar1_y")
input_5 <- set_NAA(input, NAA_list)
fit_5 <- fit_wham(input_5, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_5, dir.main = tmp.dir)

NAA_list <- list(
  sigma = "rec+1",
  cor = "iid")
input_6 <- set_NAA(input, NAA_list)
fit_6 <- fit_wham(input_6, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_6, dir.main = tmp.dir)

NAA_list <- list(
  sigma = "rec+1",
  cor = "ar1_a")
input_7 <- set_NAA(input, NAA_list)
fit_7 <- fit_wham(input_7, do.retro=FALSE, do.osa=FALSE)
#doesn't converge well due to logistic selectivity parameters
plot_wham_output(fit_7, dir.main = tmp.dir)

NAA_list <- list(
  sigma = "rec+1",
  cor = "ar1_y")
input_8 <- set_NAA(input, NAA_list)
fit_8 <- fit_wham(input_8, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_8, dir.main = tmp.dir)

NAA_list <- list(
  sigma = "rec+1",
  cor = "2dar1")
input_9 <- set_NAA(input, NAA_list)
fit_9 <- fit_wham(input_9, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_9, dir.main = tmp.dir)

NAA_list <- list(recruit_model = 3,
                 sigma = "rec+1",
                 cor = "ar1_y")
input_10 <- set_NAA(input, NAA_list)
fit_10 <- fit_wham(input_10, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_10, dir.main = tmp.dir)

NAA_list <- list(recruit_model = 4,
                 sigma = "rec+1",
                 cor = "ar1_y")
input_11 <- set_NAA(input, NAA_list)
fit_11 <- fit_wham(input_11, do.retro=FALSE, do.osa=FALSE)
plot_wham_output(fit_11, dir.main = tmp.dir)

M <- list(
  mean_model = "estimate-M",
  means_map = array(1, dim = c(1,1,6)))
input_12 <- set_M(input, M)
fit_12 <- fit_wham(input_12, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_12, dir.main = tmp.dir)

M <- list(
  mean_model = "estimate-M",
  means_map = array(c(1,1,1,2,2,2), dim = c(1,1,6)))
input_13 <- set_M(input, M)
fit_13 <- fit_wham(input_13, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_13, dir.main = tmp.dir)

M <- list(
  mean_model = "weight-at-age",
  b_prior = TRUE
)
input_14 <- set_M(input, M)
input_14$data$waa[4,44,1] <- 0.1 #a 0 at age 1 for weight.
fit_14 <- fit_wham(input_14, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_14, dir.main = tmp.dir)

M <- list(
  initial_means = array(0.2, dim = c(1,1,6)), 
  means_map = array(NA, dim = c(1,1,6)),
  re_model = matrix("ar1_a",1,1)
)
input_15 <- set_M(input, M)
fit_15 <- fit_wham(input_15, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
#some bad selectivity pars
plot_wham_output(fit_15, dir.main = tmp.dir)

M <- list(
  initial_means = array(0.2, dim = c(1,1,6)), 
  means_map = array(NA, dim = c(1,1,6)),
  re_model = matrix("ar1_y",1,1)
)
input_16 <- set_M(input, M)
fit_16 <- fit_wham(input_16, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
#scale is bad for this fit
plot_wham_output(fit_16, dir.main = tmp.dir)

M <- list(
  initial_means = array(0.2, dim = c(1,1,6)), 
  means_map = array(NA, dim = c(1,1,6)),
  re_model = matrix("iid_ay",1,1)
)
input_17 <- set_M(input, M)
fit_17 <- fit_wham(input_17, do.sdrep = FALSE, do.retro = FALSE, do.osa = FALSE)
# saveRDS(fit_17, file.path("temp", "day_2_2_fit_17.RDS"))
plot_wham_output(fit_17, dir.main = tmp.dir)

catchability <- list(
  re = c("iid", "none")
)
input_18 <- set_q(input, catchability)
fit_18 <- fit_wham(input_18, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_18, dir.main = tmp.dir)

catchability <- list(
  re = c("ar1", "ar1")
)
input_19 <- set_q(input, catchability)
fit_19 <- fit_wham(input_19, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_19, dir.main = tmp.dir)

catchability <- list(
  re = c("ar1", "ar1"),
  q_upper = rep(1,2)
)
input_20 <- set_q(input, catchability)
fit_20 <- fit_wham(input_20, do.retro = FALSE, do.osa = FALSE)
plot_wham_output(fit_20, dir.main = tmp.dir)

####end of vignette 4
