#---------------------------------#
# Working with AGEPRO Output in R #
# Tautog SAS ASAP/AGEPRO Webinar  #
# July 1, 2025                    #
# K. Drew, ASMFC                  #
#---------------------------------#

library(tidyverse)

# We set the F reference points based on the ASAP output
F_thresh <- 0.324995790032184
F_target <- 0.197726475852373

#---- Long-term projections ----
#agepro <- dget("C:/Users/galax/OneDrive/Documents/AGEPRO/vts30_2026-04-30_14-53-48/vts30.rdat")
#str(agepro)
#plot(agepro$ssb$pct50, ylim=c(0, 7000))

# Where SSB stabilizes under the F threshold (F30%SPR) is the SSB threshold. 
# We use the median of the last 10 years of this projections as the SSB threshold
#SSB_thresh <- round(median(agepro$ssb$pct50[90:100]), 0)
SSB_thresh <- round(median(c(5.9948,
                             5.9963,
                             6.0025,
                             6.0035,
                             6.0024,
                             5.9973,
                             6.0041,
                             6.0074,
                             6.0130,
                             5.9977))*1000, 0)

# We'll do the same thing for the SSB target
#agepro <- dget("C:\\Users\\katie_d\\Documents\\AGEPRO\\MARI_SSB_Target_2021_2025-06-30_16-12-28\\MARI_SSB_Target_2021.rdat")
#SSB_target <- round(median(agepro$ssb$pct50[90:100]), 0)
SSB_target = round(median(c(7.9936, 7.9867, 7.9948, 7.9875, 7.9817, 7.9783, 7.9837, 7.9911, 7.9893, 7.9671))*1000,0)
# Now we can plot the short-term projections with the reference points

#---- Short-term projections
agepro_30 <-  dget("C:/Users/galax/OneDrive/Documents/AGEPRO/target2030_update_2026-03-12_11-51-43/target2030_update.rdat")
agepro_28 <-  dget("C:/Users/galax/OneDrive/Documents/AGEPRO/target2030_28redux_2026-03-12_11-47-18/target2030_28redux.rdat")
agepro_quo <-  dget("C:/Users/galax/OneDrive/Documents/AGEPRO/target2030_statusquo_2026-03-12_11-03-18/target2030_statusquo.rdat")

agepro <- agepro_28
str(agepro)

yrs <- seq(agepro$genparms$startyear, agepro$genparms$endyear, 1)

ssb.plot <- data.frame(Year=yrs, SSB=agepro$ssb$pct50, LCI=agepro$ssb$pct5, UCI=agepro$ssb$pct95)

ggplot(ssb.plot, aes(x=Year, y=SSB)) + 
  geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), alpha=0.4) +
  geom_line() + geom_point() +
  geom_hline(aes(yintercept=SSB_target, linetype="SSB Target", color="SSB Target")) +
  geom_hline(aes(yintercept=SSB_thresh, linetype="SSB Threshold", color="SSB Threshold")) +
  scale_color_manual(values=c("SSB Target"="black", "SSB Threshold" = "red"), name="Reference Points") + 
  scale_linetype_manual(values=c("SSB Target"=2, "SSB Threshold"=1), name="Reference Points") +
  scale_y_continuous(name="Spawning stock biomass (mt)", labels=scales::comma, limits=c(0,NA)) +
  theme_bw()

F.plot <- data.frame(Year=yrs, Fmult=agepro$fmult$pct50, LCI=agepro$fmult$pct5, UCI=agepro$fmult$pct95)

ggplot(F.plot, aes(x=Year, y=Fmult)) + 
  geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), alpha=0.4) +
  geom_line() + geom_point() +
  geom_hline(aes(yintercept=F_target, linetype="F Target", color="F Target")) +
  geom_hline(aes(yintercept=F_thresh, linetype="F Threshold", color="F Threshold")) +
  scale_color_manual(values=c("F Target"="black", "F Threshold" = "red"), name="Reference Points") + 
  scale_linetype_manual(values=c("F Target"=2, "F Threshold"=1), name="Reference Points") +
  scale_y_continuous(name="Full F", labels=scales::comma, limits=c(0,NA)) +
  theme_bw()

# Probability of exceeding reference points
agepro$threshprob

harvest <- data.frame(year = 1989:2030,
              quo = c(asap[[1]]$dat$CAA_mats[[1]][,13], rep(1833, 6)), 
              harvest30 = c(asap[[1]]$dat$CAA_mats[[1]][,13], rep(1094, 6)),
              harvest_28 = c(asap[[1]]$dat$CAA_mats[[1]][,13], rep(1028, 6)))
harvest2 <- pivot_longer(harvest, cols=-year, names_to="scenario", values_to="harvest")

ggplot(harvest2, aes(x=year, y=harvest, colour = scenario)) + 
  #geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), alpha=0.4) +
  geom_line() + geom_point() +
  #geom_hline(aes(yintercept=F_target, linetype="F Target", color="F Target")) +
  #geom_hline(aes(yintercept=F_thresh, linetype="F Threshold", color="F Threshold")) +
  #scale_color_manual(values=c("F Target"="black", "F Threshold" = "red"), name="Reference Points") + 
  #scale_linetype_manual(values=c("F Target"=2, "F Threshold"=1), name="Reference Points") +
  scale_y_continuous(name="Harvest", labels=scales::comma, limits=c(0,NA)) +
  theme_bw()
