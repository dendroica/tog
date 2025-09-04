#---------------------------#
# ASAP Plots in R           #
# Tautog SAS ASAP Webinar   #
# July 1, 2025              #
#---------------------------#
root <- "C:/Users"
usr <- "jgorzo" #"galax"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)
setwd(file.path(root, "output/tog/asap/Sept3_noadjust"))
#---- ASAPplots library ----

# Load the ASAPplots library, which will do a bunch of plots
# for you automatically. It also needs the "dplyr" library to run.
library(ASAPplots)
library(dplyr)

# The wrapper function to make all the plots is PlotASAP
?PlotASAP

# Give it the name of the directory where your ASAP run is saved
# and the name of the ASAP file for the run you want to plot

pwd <- "base"
fn <- "AUG29_KD_RAW"

# By default, PlotASAP will provide the plots as individual .png files and
# as a compiled pdf with all the plots. For initial exploration of results,
# the pdf is all you need, so we'll skip creating the png files for now.
PlotASAP(pwd, asap.name=fn, save.plots=F)

# PlotASAP will create a "plots" folder for you within that 
# folder to save the plots to

# It will run through a bunch of plots in the plot window, and may
# pause on some grey/empty screens. Wait until the carat returns on the console
# to know when it's done.

# PlotASAP will also recalculate the reference points, but it will use a 5-year
# average for the inputs, not the last year like ASAP does, so the answers
# may be a little different.

# We can read in the retrospective run to see those results as well
pwd <- "retro"
#fn <- "MARI_BASE_RUN_2021_Retro"
PlotASAP(pwd, asap.name=fn, save.plots=F)

# We can look at the MCMC run so we can see the MCMC diagnostics as well
pwd <- "mcmc"
#fn <- "MARI_BASE_RUN_2021_MCMC"
PlotASAP(pwd, asap.name=fn, save.plots=F)

# If you don't like the look of the PlotASAP output, you can read the ASAP
# .rdat file into R yourself and make your own. Some examples of things you
# might want to do are below, but feel free to experiment!


#---- Custom plots by hand ----
library(ggplot2)
# Use dget to read in the .rdat file that ASAP produces
asap <- dget(paste0(pwd, "/", fn, ".rdat"))

# It is a list of input, model configuration, and output
names(asap)

yrs <- seq(asap$parms$styr, asap$parms$endyr)
## Diagnostic plots
# Catch
c.plot <- data.frame()
c.res <- data.frame()
for(i in 1:asap$parms$nfleets){
  c.plot <- rbind(c.plot, data.frame(Year=yrs, Fleet=paste("Fleet", i), 
                                     Catch=asap$catch.obs[i,], Type="Observed"))
  c.plot <- rbind(c.plot, data.frame(Year=yrs, Fleet=paste("Fleet", i), 
                                     Catch=asap$catch.pred[i,], Type="Predicted"))
  c.res <- rbind(c.res, data.frame(Year=yrs, Fleet=paste("Fleet", i), 
                                   Resids=asap$catch.std.resid[i,]))
  
}

ggplot(c.plot, aes(x=Year, y=Catch, color=Type, shape=Type, linetype=Type)) +
  geom_line() + geom_point() +
  scale_color_manual(values=c("black", "blue")) +
  scale_linetype_manual(values=c(0, 1)) +
  scale_shape_manual(values=c(16, NA)) +
  facet_wrap(~Fleet, ncol=2) +
  scale_y_continuous(name="Catch (mt)", labels=scales::comma, limits=c(0,NA))+
  theme_bw()

ggplot(c.res) +
  geom_col(aes(x=Year, y=Resids)) +
  facet_wrap(~Fleet, ncol=2) +
  scale_y_continuous(name="Standardized residuals")+
  theme_bw()

# Indices
i.plot <- data.frame()
i.res <- data.frame()
for(i in 1:asap$parms$nindices){
  i.plot <- rbind(i.plot, data.frame(Year=asap$index.year[[i]], 
                                     Index=paste("Index", i), 
                                     CPUE=asap$index.obs[[i]], Type="Observed"))
  i.plot <- rbind(i.plot, data.frame(Year=asap$index.year[[i]], 
                                     Index=paste("Index", i), 
                                     CPUE=asap$index.pred[[i]], Type="Predicted"))  
  i.res <- rbind(i.res, data.frame(Year=asap$index.year[[i]], 
                                   Index=paste("Index", i), 
                                   Resids=asap$index.std.resid[[i]]))
  
}
ggplot(i.plot, aes(x=Year, y=CPUE, color=Type, shape=Type, linetype=Type)) +
  geom_line() + geom_point() +
  scale_color_manual(values=c("black", "blue")) +
  scale_linetype_manual(values=c(0, 1)) +
  scale_shape_manual(values=c(16, NA)) +
  facet_wrap(~Index, scales="free_y") +
  theme_bw()

ggplot(i.res) +
  geom_col(aes(x=Year, y=Resids)) +
  facet_wrap(~Index) +
  scale_y_continuous(name="Standardized residuals")+
  theme_bw()

## SSB and F
# Read in the .std file to get the standard deviation of SSB and F
# Note: the .std file has a name with a space in it which will throw R off
# Open the .std file in a text editor and change "std dev" to "std.dev"
std <- read.table(paste0(pwd, "\\", fn,".std"), header=T, col.names=c("index", "name", "value", "std.dev"))
head(std)
tail(std)

ssb <- data.frame(Year=seq(asap$parms$styr, asap$parms$endyr, 1), SSB=asap$SSB, 
                  LCI = asap$SSB - 1.96*std$std.dev[std$name=="SSB"],
                  UCI = asap$SSB + 1.96*std$std.dev[std$name=="SSB"])

ggplot(ssb) + geom_line(aes(x=Year, y=SSB)) +
  scale_y_continuous(name="Spawning stock biomass (mt)", limits=c(0,NA),
                     labels = scales::comma) + 
  geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), fill="grey", alpha=0.6) +
  theme_bw()

frep <- data.frame(Year=seq(asap$parms$styr, asap$parms$endyr, 1), F_rep=asap$F.report,
                   LCI = asap$F.report - 1.96*std$std.dev[std$name=="Freport"],
                   UCI = asap$F.report + 1.96*std$std.dev[std$name=="Freport"])

ggplot(frep) + geom_line(aes(x=Year, y=F_rep)) +
  scale_y_continuous(name="Average F", limits=c(0,NA)) + 
  geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), fill="grey", alpha=0.6) +
  theme_bw()

## Sensitivity Run Comparisons
# For this example, I'm comparing the results of all the 2021 
# regional assessments, which are each in their own folder in
# the working directory and which all have the same file name
# structure so I will just loop over the regional abbreviations

runs <- c("MARI", "LIS", "NJ-NYB", "DMV")

ssb <- data.frame()
F.out <- data.frame()
recr <- data.frame()

for(r in runs){
  asap <- dget(paste0(r, "\\", r, "_BASE_RUN_2021.rdat"))
  ssb <- rbind(ssb, data.frame(Year=seq(asap$parms$styr, asap$parms$endyr, 1), 
                               SSB=asap$SSB,
                               Run=r))
  F.total <- apply(asap$F.age, 1, max)
  
  F.out <- rbind(F.out,  data.frame(Year=seq(asap$parms$styr, asap$parms$endyr, 1),
                                    F_rep=asap$F.report, F_total=F.total,
                                    Run=r))
  recr <-  rbind(recr,  data.frame(Year=seq(asap$parms$styr, asap$parms$endyr, 1),
                                    R=asap$N.age[,1],
                                    Run=r))
  
}

ssb$Run <- factor(ssb$Run, levels=runs)
F.out$Run <- factor(F.out$Run, levels=runs)
recr$Run <- factor(recr$Run, levels=runs)

oi<- as.vector(palette("Okabe-Ito"))

ggplot(ssb, aes(x=Year, y=SSB, color=Run, shape=Run)) +
  geom_line() + geom_point() +
  scale_color_manual(values=oi) +
  scale_y_continuous(name="SSB (mt)", limits=c(0,NA), labels=scales::comma)+
  theme_bw()

ggplot(F.out, aes(x=Year, y=F_rep, color=Run, shape=Run)) +
  geom_line() + geom_point() +
  scale_color_manual(values=oi) +
  scale_y_continuous(name="Average F", limits=c(0,NA))+
  theme_bw()

ggplot(F.out, aes(x=Year, y=F_total, color=Run, shape=Run)) +
  geom_line() + geom_point() +
  scale_y_continuous(name="Maximum F-at-age", limits=c(0,NA))+
  theme_bw()

ggplot(recr, aes(x=Year, y=R/1e3, color=Run, shape=Run)) +
  geom_line() + geom_point() +
  scale_color_manual(values=oi) +
  scale_y_continuous(name="Recruitment (millions of age-1 fish)", limits=c(0,NA))+
  theme_bw()

#---- Recruitment for AGEPRO ----
# PlotASAP automatically outputs the 5-year averages of weight, selectivity,
# etc. that you might want for projections. We will also take a minute
# to write out the recruitment time-series that we will draw from.
pwd <- "MARI"
fn <- "MARI_BASE_RUN_2021"

asap <- dget(paste0(pwd, "\\", fn, ".rdat"))

R <- asap$N.age[,1]

write.csv(R, paste0(pwd, "\\Plots\\AGEPRO_recruitment_obs.csv"))
