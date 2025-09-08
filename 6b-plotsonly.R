load("C:/Users/galax/Downloads/retro3yr.RData")
library(ggplot2)

ggplot(F.comps,aes(x=Year, y=FullF, color=Metric, shape=Metric)) + 
  geom_line() + geom_point() +
  scale_color_manual(values=c("#000000", "#E69F00")) +
  scale_y_continuous(name="Fishing mortality", limits=c(0,NA)) +
  theme_bw()

## No BRPs, only the box
ggplot() +   
  geom_segment(data=CI_box, aes(x=SSB_CI1, y=F_CI1, xend=SSB_CI2, yend=F_CI2)) +
  geom_segment(aes(x=SSB[nyears,1], xend=SSB[nyears,1], y=F_min, yend=F_max, lty="90% CI")) +
  geom_segment(aes(x=SSB_min, xend=SSB_max, y=F_3yr[nyears,1], yend=F_3yr[nyears,1], lty="90% CI"))+
  # geom_hline(data=F_ref, aes(yintercept=value, lty=BRP))+
  # geom_vline(data=SSB_ref, aes(xintercept=value, lty=BRP))+
  geom_point(data=F_SSB, aes(x=SSB, y=F_rep, color=Metric, shape=Metric)) +
  scale_linetype_manual(name=NULL, values=c("90% CI"=1, "Target"=3, "Threshold"=4))+
  scale_color_manual(values=c("red", "black"))+
  scale_shape_manual(values=c(16, 15)) +
  scale_y_continuous(name="3-year Average F", limits=c(0,NA))+
  scale_x_continuous(name="SSB (mt)", limits=c(0,NA), labels=scales::comma)+
  theme_bw() + theme(panel.grid=element_blank(), legend.spacing.y=unit(0.01, "cm"))

## With BRPs
ggplot() +   
  geom_segment(data=CI_box, aes(x=SSB_CI1, y=F_CI1, xend=SSB_CI2, yend=F_CI2)) +
  geom_segment(aes(x=SSB[nyears,1], xend=SSB[nyears,1], y=F_min, yend=F_max, lty="90% CI")) +
  geom_segment(aes(x=SSB_min, xend=SSB_max, y=F_3yr[nyears,1], yend=F_3yr[nyears,1], lty="90% CI"))+
  geom_hline(data=F_ref, aes(yintercept=value, lty=BRP))+
  geom_vline(data=SSB_ref, aes(xintercept=value, lty=BRP))+
  geom_point(data=F_SSB, aes(x=SSB, y=F_rep, color=Metric, shape=Metric)) +
  scale_linetype_manual(name=NULL, values=c("90% CI"=1, "Target"=3, "Threshold"=4))+
  scale_color_manual(values=c("red", "black"))+
  scale_shape_manual(values=c(16, 15)) +
  scale_y_continuous(name="3-year Average F", limits=c(0,NA))+
  scale_x_continuous(name="SSB (mt)", limits=c(0,NA), labels=scales::comma)+
  theme_bw() + theme(panel.grid=element_blank(), legend.spacing.y=unit(0.01, "cm"))

#---- Q2: Are the majority of most recent peels outside of confidence interval---- 
# of the base run...

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
## F plot
ggplot(F_retro) +
  geom_line(aes(x=Year, y=F_rep, color=TY)) +
  geom_point(aes(x=TYP, y=F_rep, color=TY)) +
  geom_ribbon(aes(x=Year, ymin=F_LCI, ymax=F_UCI, fill=TY), alpha=0.5) +
  scale_color_manual(values=cbPalette, name="Terminal Year") +
  scale_fill_manual(values=cbPalette, name="Terminal Year") +
  scale_y_continuous(limits=c(0,NA), name="3-Year Average F")+
  theme_bw() + guides(fill="none")
## SSB plot
ggplot(SSB_retro) +
  geom_line(aes(x=Year, y=SSB, color=TY)) +
  geom_point(aes(x=TYP, y=SSB, color=TY)) +
  geom_ribbon(aes(x=Year, ymin=SSB_LCI, ymax=SSB_UCI, fill=TY), alpha=0.5) +
  scale_color_manual(values=cbPalette, name="Terminal Year") +
  scale_fill_manual(values=cbPalette, name="Terminal Year") +
  scale_y_continuous(limits=c(0,NA), labels=scales::comma)+
  theme_bw() + guides(fill="none")

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

ggplot(base_out) + geom_line(aes(x=Year, y=SSB)) +
  geom_ribbon(aes(x=Year, ymin=SSB_LCI, ymax=SSB_UCI), alpha=0.4) +
  geom_point(aes(x=Year, y=SSB_adj, shape="Retro-adjusted SSB")) +
  geom_hline(data=SSB_ref, aes(yintercept=value, linetype=BRP, color=BRP)) +
  scale_color_manual(values=c("black", "red"), name="Reference Points") +
  scale_linetype_manual(values=c(2,1), name="Reference Points") +
  scale_shape_manual(values=c(16), name=NULL) +
  scale_y_continuous(name="SSB (mt)", limits=c(0, NA), labels=scales::comma)+
  theme_bw()

