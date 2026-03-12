#---------------------------------#
# Working with AGEPRO Output in R #
# Tautog SAS ASAP/AGEPRO Webinar  #
# July 1, 2025                    #
# K. Drew, ASMFC                  #
#---------------------------------#

library(tidyverse)

# We set the F reference points based on the ASAP output
F_thresh <- 0.33
F_target <- 0.2

#---- Long-term projections ----
#agepro <- dget("C:\\Users\\katie_d\\Documents\\AGEPRO\\MARI_SSB_Thresh_2021_2025-06-30_16-00-12\\MARI_SSB_Thresh_2021.rdat")

#str(agepro)

#plot(agepro$ssb$pct50, ylim=c(0, 7000))

# Where SSB stabilizes under the F threshold (F30%SPR) is the SSB threshold. 
# We use the median of the last 10 years of this projections as the SSB threshold

#SSB_thresh <- round(median(agepro$ssb$pct50[90:100]), 0)
SSB_thresh = 5929

# We'll do the same thing for the SSB target
#agepro <- dget("C:\\Users\\katie_d\\Documents\\AGEPRO\\MARI_SSB_Target_2021_2025-06-30_16-12-28\\MARI_SSB_Target_2021.rdat")
#SSB_target <- round(median(agepro$ssb$pct50[90:100]), 0)
SSB_target = 7910
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

harvest <- as.data.frame(year = 1989:2030,
              quo = c(asap[[1]]$dat$CAA_mats[[1]][,13], rep(1833, 6)), 
              harvest30 = c(asap[[1]]$dat$CAA_mats[[1]][,13], rep(1094, 6)),
              harvest_28 = c(asap[[1]]$dat$CAA_mats[[1]][,13], rep(1028, 6)))