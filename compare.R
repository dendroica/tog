source("C:/Users/galax/Documents/tog/6a-ASAP_retro_3yrAvg_F.R")
source("C:/Users/galax/Documents/tog/7-ASAP_retro_3yrAvg_F-VTS.R")

F_ref$BRP <- c("Target VTS", "Threshold VTS")
F_ref <- rbind(F_reforig, F_ref)

SSB_ref$BRP <- c("Target VTS", "Threshold VTS")
SSB_ref <- rbind(SSB_reforig, SSB_ref)

baseout_orig$run <- "update"
base_out$run <- "VTS"
base_out <- rbind(baseout_orig, base_out)

ggplot(base_out) + geom_line(aes(x=Year, y=F_rep, group=run, color=run)) +
  #scale_color_manual(values=c("orange","yellow")) +
  #geom_ribbon(aes(x=Year, ymin=F_LCI, ymax=F_UCI, color=run), alpha=0.4) +
  #scale_color_manual(values=c("orange","yellow"))
  #geom_point(aes(x=Year, y=F_adj, shape="Retro-adjusted F")) +
  geom_hline(data=F_ref, aes(yintercept=value, linetype=BRP, color=BRP)) +
  scale_color_manual(values=c("VTS" = "orange", "update" = "green", "Target" = "blue", "Threshold" = "red", "Target VTS" = "lightblue", "Threshold VTS" = "pink")) +
  #scale_linetype_manual(values=c(2,2,1,1)) +
  #scale_shape_manual(values=c(16,16), name=NULL) +
  scale_y_continuous(name="3-Year Average F", limits=c(0, NA), labels=scales::comma)+
  theme_bw()

ggplot(base_out) + geom_line(aes(x=Year, y=SSB, group=run, color=run)) +
  #geom_ribbon(aes(x=Year, ymin=SSB_LCI, ymax=SSB_UCI), alpha=0.4) +
  #geom_point(aes(x=Year, y=SSB_adj, shape="Retro-adjusted SSB")) +
  geom_hline(data=SSB_ref, aes(yintercept=value, linetype=BRP, color=BRP)) +
  scale_color_manual(values=c("VTS" = "orange", "update" = "green", "Target" = "blue", "Threshold" = "red", "Target VTS" = "lightblue", "Threshold VTS" = "pink")) +
  #scale_linetype_manual(values=c(2,1), name="Reference Points") +
  #scale_shape_manual(values=c(16, 16), name=NULL) +
  scale_y_continuous(name="SSB (mt)", limits=c(0, NA), labels=scales::comma)+
  theme_bw()
