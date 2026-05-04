#-------------------------------------------#
# Adjusting for a retrospective pattern     #
# when using a 3-year average F for stock   #
# status.                                   #
#-------------------------------------------#

# The tautog assessment uses a 3-year running average
# of F to evaluate overfishing status. The Mohn's rho
# calculated by ASAP uses the annual F. This script
# calculates the Mohn's rho for a 3-year running average 
# for F so that the terminal year F can be adjusted using the
# correct metric if necessary, and then walks through
# the retrospective flow chart plots with the 3-year avg. F

library(tidyverse)
root <- Sys.getenv("FILEPATH")

asap <- file.path(root, "output/tog/asap/FINAL")

#---- Reference points for stock status ----

# We are using the full F for the 3-year average, 
# so use the F30% and F40% from the SPR.Target.Table output (this is in ASAP plots output)
#30% = 0.3318533
#40% = 0.201155
F_reforig <- data.frame(BRP=c("Target", "Threshold"), value=c(0.20157032300054, 0.332662206371242))

# You could do this with ASAP's F report (which is an average F over
# ages, not years), as long as you use the equivalent full F in the 
# projections

# Use the SSB reference points from your long-term AgePro projections 
# that used the F values above
SSB_reforig <- data.frame(BRP=c("Target", "Threshold"), value=c(7306,5474 ))

#---- Read in retro runs and calculate the 3-year average F ----

# Folder where your retrospective runs are saved
rdorig <- file.path(asap, "retro")

# Name of your ASAP input file (no .dat extension)
fnorig <- "ORIG"

# Number of peels you ran in your retrospective analysis
# (usually 5-7)
npeels <- 6

# Base run results
asap.baseorig <- dget(paste0(file.path(rdorig, fnorig), "_000.rdat"))
nyears <- asap.baseorig$parms$nyears

F_fullorig <- apply(asap.baseorig$F.age, 1, max)

F_3yrorig <- matrix(nrow=nyears, ncol=npeels+1)
SSBorig <- matrix(nrow=nyears, ncol=npeels+1)

# SSB
SSBorig[,1] <- asap.baseorig$SSB

# 3-year average F
for(y in 3:length(F_fullorig)){
  F_3yrorig[y,1] <- mean(c(F_fullorig[y], F_fullorig[y-1], F_fullorig[y-2]))
}

# Comparison of annual and average full F

F.compsorig <- data.frame(Year=asap.baseorig$parms$styr:asap.baseorig$parms$endyr, 
                      FullF=F_fullorig, Metric="Annual")

F.compsorig <- rbind(F.compsorig, data.frame(Year=asap.baseorig$parms$styr:asap.baseorig$parms$endyr, 
                                     FullF=F_3yrorig[,1], Metric="3-year Average"))


ggplot(F.compsorig,aes(x=Year, y=FullF, color=Metric, shape=Metric)) + 
  geom_line() + geom_point() +
  scale_color_manual(values=c("#000000", "#E69F00")) +
  scale_y_continuous(name="Fishing mortality", limits=c(0,NA)) +
  theme_bw()

#ggsave("F_Annual_v_3yrAvg.png", height=4.5, width=6.5)

# Dataframe for plotting the box
F_SSBorig <- data.frame(F_rep=F_3yrorig[nyears,1], SSB=SSBorig[nyears,1], Metric="Unadjusted")

# Get SSB and 3-year average for F for each peel
for(p in 1:npeels){
  
  asap.in <- dget(paste0(file.path(rdorig, fnorig), "_00", p, ".rdat"))
  F_fullorig <- apply(asap.in$F.age, 1, max)
  for(y in 3:length(F_fullorig)){
    F_3yrorig[y,p+1] <- mean(c(F_fullorig[y], F_fullorig[y-1], F_fullorig[y-2]))
  }
  SSBorig[,p+1] <- c(asap.in$SSB, rep(NA, p))
}

# Mohn's rho for each peel (calculate using ASAP's method)
rel.mat.Forig <- matrix(nrow=nyears, ncol=npeels)

for (p in 1:npeels) {
  rel.mat.Forig[,p] <- (F_3yrorig[,p+1] - F_3yrorig[,1])/(F_3yrorig[,1])
}

rel.mat.SSB <- matrix(nrow=nyears, ncol=npeels)

for (p in 1:npeels) {
  rel.mat.SSB[,p] <- (SSBorig[,p+1] - SSBorig[,1])/(SSBorig[,1])
}

mm.f <- 0
for (p in 1:npeels) {
  mm.f <- mm.f + rel.mat.Forig[nyears - p, p]
}
rho.f <- mm.f/npeels

mm.ssb <- 0
for (p in 1:npeels) {
  mm.ssb <- mm.ssb + rel.mat.SSB[nyears - p, p]
}
rho.ssb <- mm.ssb/npeels

F_SSBorig <- rbind(F_SSBorig, data.frame(F_rep=F_3yrorig[nyears,1]*(1/(1+rho.f)), SSB=SSBorig[nyears,1]*(1/(1+rho.ssb)), Metric="Retro-adjusted"))

# Table for output
FSSB.outorig <- as.data.frame(t(F_SSBorig[,-3]))
names(FSSB.outorig) <- F_SSBorig$Metric                 
FSSB.outorig$Rho <- c(rho.f, rho.ssb)

#write.csv(FSSB.outorig, paste0(rdorig, "F_SSBorig_retro_adjusted.csv"))

#---- Retrospective Flow Chart ----
#---- Q1: Is rho outside of suggested bounds...----
FSSB.outorig
# Short-lived species (max age <= 15 years): -0.22 < Rho < 0.30
# Long-lived species (max age > 15 years): -0.15 < Rho < 0.2

#--- AND is rho-adjusted value outside the 90% CI of the unadjusted estimate? ----
# Where did you save your MCMC runs?
pwd <- file.path(asap, "mcmc")

# What did you call your MCMC base run input file?
#fnorig <- "AUG20_KD_MCMC"

# 90% confidence intervals from method used to seed projections
# from MCMC run 
MCMC.inorig <- read.table(paste0(file.path(pwd, fnorig), ".MCM"), header=T)

# SSB in terminal year
ssb.mcmcorig <- MCMC.inorig[,paste0("SSB", asap.baseorig$parms$endyr)]

SSB_min <-quantile(ssb.mcmcorig, probs=0.05)
SSB_max <- quantile(ssb.mcmcorig, probs=0.95)

# 3-year average from MCMC runs
Y3orig <- c(asap.baseorig$parms$endyr, asap.baseorig$parms$endyr-1, asap.baseorig$parms$endyr-2)
f.mcmcorig <-   MCMC.inorig[,paste0("Fmult_", Y3orig)]
f.mcmcorig$F_3yr <- apply(f.mcmcorig, 1, mean)

F_minorig <- quantile(f.mcmcorig$F_3yr, probs=0.05)
F_maxorig <- quantile(f.mcmcorig$F_3yr, probs=0.95)

# 90% CIs for plotting
CI_boxorig <- data.frame(SSB_CI1=c(SSB_min, SSB_min, SSB_min, SSB_max),
                     SSB_CI2=c(SSB_max, SSB_min, SSB_max, SSB_max),
                     F_CI1=c(F_minorig, F_minorig, F_maxorig, F_minorig),
                     F_CI2=c(F_minorig, F_maxorig, F_maxorig, F_maxorig))

## No BRPs, only the box
ggplot() +   
  geom_segment(data=CI_boxorig, aes(x=SSB_CI1, y=F_CI1, xend=SSB_CI2, yend=F_CI2)) +
  geom_segment(aes(x=SSBorig[nyears,1], xend=SSBorig[nyears,1], y=F_minorig, yend=F_maxorig, lty="90% CI")) +
  geom_segment(aes(x=SSB_min, xend=SSB_max, y=F_3yrorig[nyears,1], yend=F_3yrorig[nyears,1], lty="90% CI"))+
  # geom_hline(data=F_reforig, aes(yintercept=value, lty=BRP))+
  # geom_vline(data=SSB_reforig, aes(xintercept=value, lty=BRP))+
  geom_point(data=F_SSBorig, aes(x=SSB, y=F_rep, color=Metric, shape=Metric)) +
  scale_linetype_manual(name=NULL, values=c("90% CI"=1, "Target"=3, "Threshold"=4))+
  scale_color_manual(values=c("red", "black"))+
  scale_shape_manual(values=c(16, 15)) +
  scale_y_continuous(name="3-year Average F", limits=c(0,NA))+
  scale_x_continuous(name="SSB (mt)", limits=c(0,NA), labels=scales::comma)+
  theme_bw() + theme(panel.grid=element_blank(), legend.spacing.y=unit(0.01, "cm"))


#ggsave("Retro_adj_boxplot.png", height=4.5, width=6.5) 

## With BRPs
ggplot() +   
  geom_segment(data=CI_boxorig, aes(x=SSB_CI1, y=F_CI1, xend=SSB_CI2, yend=F_CI2)) +
  geom_segment(aes(x=SSB[nyears,1], xend=SSB[nyears,1], y=F_minorig, yend=F_maxorig, lty="90% CI")) +
  geom_segment(aes(x=SSB_minorig, xend=SSB_maxorig, y=F_3yrorig[nyears,1], yend=F_3yrorig[nyears,1], lty="90% CI"))+
  geom_hline(data=F_reforig, aes(yintercept=value, lty=BRP))+
  geom_vline(data=SSB_reforig, aes(xintercept=value, lty=BRP))+
  geom_point(data=F_SSBorig, aes(x=SSB, y=F_rep, color=Metric, shape=Metric)) +
  scale_linetype_manual(name=NULL, values=c("90% CI"=1, "Target"=3, "Threshold"=4))+
  scale_color_manual(values=c("red", "black"))+
  scale_shape_manual(values=c(16, 15)) +
  scale_y_continuous(name="3-year Average F", limits=c(0,NA))+
  scale_x_continuous(name="SSB (mt)", limits=c(0,NA), labels=scales::comma)+
  theme_bw() + theme(panel.grid=element_blank(), legend.spacing.y=unit(0.01, "cm"))


#ggsave("Retro_adj_boxplot_wBRPs.png", height=4.5, width=6.5) 

#---- Q2: Are the majority of most recent peels outside of confidence interval---- 
# of the base run...

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Pull the 90% CIs from the MCMC runs
yrs <- seq(asap.baseorig$parms$styr, asap.baseorig$parms$endyr, 1)
ssb.mcmcorig <- MCMC.inorig[,paste0("SSB", yrs)]
ssb.cis <- apply(ssb.mcmcorig, 2, quantile, probs=c(0.05, 0.95))

# Calculate the 90% CIs for the 3-year average F
f.mcmcorig <- MCMC.inorig[,paste0("Fmult_", yrs)]
f3.mcmc <- matrix(nrow=nrow(MCMC.inorig), ncol=nyears)
for(y in 3:nyears){
  f.3 <- f.mcmcorig[,paste0("Fmult_", c(yrs[y],yrs[y-1], yrs[y-2]))]
  f3.mcmc[,y] <- apply(f.3, 1, mean)
}

f.cis <- apply(f3.mcmc, 2, quantile, probs=c(0.05, 0.95), na.rm=T)

# Create the base run output data frame with CIs
baseout_orig <- data.frame(Year=yrs,SSB=asap.baseorig$SSB, F_rep=F_3yrorig[,1],
                       SSB_LCI=ssb.cis[1,], SSB_UCI=ssb.cis[2,],
                       F_LCI=f.cis[1,], F_UCI=f.cis[2,],
                       TY=asap.baseorig$parms$endyr)

# Pivot F matrix into a dataframe for plotting
F_retro <- as.data.frame(F_3yrorig[,-1])
names(F_retro) <- asap.baseorig$parms$endyr - 1:npeels
F_retro$Year <- yrs

F_retro <- pivot_longer(F_retro, cols=!Year, names_to="TY", values_to="F_rep")
F_retro$TY <- as.numeric(F_retro$TY)
F_retro <- baseout_orig %>% select(Year, F_rep, F_LCI, F_UCI, TY) %>% bind_rows(F_retro)

F_retro$TYP <- ifelse(F_retro$Year==F_retro$TY, F_retro$TY, NA)
F_retro$TY <- factor(F_retro$TY, levels=unique(F_retro$TY))

## F plot
ggplot(F_retro) +
  geom_line(aes(x=Year, y=F_rep, color=TY)) +
  geom_point(aes(x=TYP, y=F_rep, color=TY)) +
  geom_ribbon(aes(x=Year, ymin=F_LCI, ymax=F_UCI, fill=TY), alpha=0.5) +
  scale_color_manual(values=cbPalette, name="Terminal Year") +
  scale_fill_manual(values=cbPalette, name="Terminal Year") +
  scale_y_continuous(limits=c(0,NA), name="3-Year Average F")+
  theme_bw() + guides(fill="none")

#ggsave("F_retro_CIs.png", height=4.5, width=6.5)

# Pivot the SSB retro matrix into a dataframe for plotting
SSB_retro <- as.data.frame(SSBorig[,-1])
names(SSB_retro) <- asap.baseorig$parms$endyr - 1:npeels
SSB_retro$Year <- yrs

SSB_retro <- pivot_longer(SSB_retro, cols=!Year, names_to="TY", values_to="SSB")
SSB_retro$TY <- as.numeric(SSB_retro$TY)

SSB_retro <- baseout_orig %>% select(Year, SSB, SSB_LCI, SSB_UCI, TY) %>% bind_rows(SSB_retro)
SSB_retro$TYP <- ifelse(SSB_retro$Year==SSB_retro$TY, SSB_retro$TY, NA)
SSB_retro$TY <- factor(SSB_retro$TY, levels=unique(SSB_retro$TY))

## SSB plot
ggplot(SSB_retro) +
  geom_line(aes(x=Year, y=SSB, color=TY)) +
  geom_point(aes(x=TYP, y=SSB, color=TY)) +
  geom_ribbon(aes(x=Year, ymin=SSB_LCI, ymax=SSB_UCI, fill=TY), alpha=0.5) +
  scale_color_manual(values=cbPalette, name="Terminal Year") +
  scale_fill_manual(values=cbPalette, name="Terminal Year") +
  scale_y_continuous(limits=c(0,NA), labels=scales::comma)+
  theme_bw() + guides(fill="none")

#---- Q2: or is the terminal year of the previous assessment ---
# outside the CI of the current assessment?

# Read in the results from the previous assessment
results_prev <- read.csv(file.path(root, 
                                   "data/tog/region_results_2021_3yrAvgF.csv"))

# Filter to your region
results_prev <- results_prev[results_prev$Region=="NJ-NYB",]

baseout_orig$Assessment <- "2025 Update"
baseout_orig$TYP <- ifelse(baseout_orig$Year==max(baseout_orig$Year), baseout_orig$Year, NA)

results_prev$Assessment <- "2021 Update"
results_prev$TYP <- ifelse(results_prev$Year==max(results_prev$Year), results_prev$Year, NA)

hist_retro <- bind_rows(baseout_orig, results_prev)
hist_retro$Assessment <- factor(hist_retro$Assessment, levels=unique(hist_retro$Assessment))

## SSB plot
ggplot(hist_retro) +
  geom_line(aes(x=Year, y=SSB, color=Assessment)) +
  geom_point(aes(x=TYP, y=SSB, color=Assessment)) +
  geom_ribbon(aes(x=Year, ymin=SSB_LCI, ymax=SSB_UCI, fill=Assessment), alpha=0.5) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous(limits=c(0,NA), labels=scales::comma, name="SSB (mt)")+
  theme_bw() + guides(fill="none")

#ggsave("SB_hist_retro_CIs.png", height=4.5, width=6.5)

## F plot
ggplot(hist_retro) +
  geom_line(aes(x=Year, y=F_rep, color=Assessment)) +
  geom_point(aes(x=TYP, y=F_rep, color=Assessment)) +
  geom_ribbon(aes(x=Year, ymin=F_LCI, ymax=F_UCI, fill=Assessment), alpha=0.5) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous(limits=c(0,NA), name="3-Year Average F")+
  theme_bw() + guides(fill="none")

#ggsave("F_hist_retro_CIs.png", height=4.5, width=6.5)

#---- Status plots for retrospectively-adjusted values ----

# If the flowchart indicates you should apply an adjustment to
# the terminal year F and SSB to assess status, use this code to plot
# the adjustment; if you don't need to adjust, comment out the geom_point() line

## SSB adjusted

baseout_orig$SSB_adj <- ifelse(baseout_orig$Year==max(baseout_orig$Year), F_SSBorig$SSB[F_SSBorig$Metric=="Retro-adjusted"], NA)

ggplot(baseout_orig) + geom_line(aes(x=Year, y=SSB)) +
  geom_ribbon(aes(x=Year, ymin=SSB_LCI, ymax=SSB_UCI), alpha=0.4) +
  geom_point(aes(x=Year, y=SSB_adj, shape="Retro-adjusted SSB")) +
  geom_hline(data=SSB_reforig, aes(yintercept=value, linetype=BRP, color=BRP)) +
  scale_color_manual(values=c("black", "red"), name="Reference Points") +
  scale_linetype_manual(values=c(2,1), name="Reference Points") +
  scale_shape_manual(values=c(16), name=NULL) +
  scale_y_continuous(name="SSB (mt)", limits=c(0, NA), labels=scales::comma)+
  theme_bw()

#ggsave("SSB_status.png", height=4.5, width=6.5)

## F adjusted

baseout_orig$F_adj <- ifelse(baseout_orig$Year==max(baseout_orig$Year), F_SSBorig$F_rep[F_SSBorig$Metric=="Retro-adjusted"], NA)

ggplot(baseout_orig) + geom_line(aes(x=Year, y=F_rep)) +
  geom_ribbon(aes(x=Year, ymin=F_LCI, ymax=F_UCI), alpha=0.4) +
  geom_point(aes(x=Year, y=F_adj, shape="Retro-adjusted F")) +
  geom_hline(data=F_reforig, aes(yintercept=value, linetype=BRP, color=BRP)) +
  scale_color_manual(values=c("black", "red"), name="Reference Points") +
  scale_linetype_manual(values=c(2,1), name="Reference Points") +
  scale_shape_manual(values=c(16), name=NULL) +
  scale_y_continuous(name="3-Year Average F", limits=c(0, NA), labels=scales::comma)+
  theme_bw()
