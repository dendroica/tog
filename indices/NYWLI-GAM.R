load("C:/Users/galax/OneDrive - New Jersey Office of Information Technology/Documents/output/wlis-gam.RData")
best <- GAM.NB
# We will calculate the index by predicting the mean CPUE in each year while
# holding the other factors constant at one level and holding continuous values
# constant at their mean values. 

# What factors did ZINB.2

p.data <- data.frame(Year=levels(dat$Year), 
                     #Station=levels(dat$Station)[1],
                     SurfaceTemp=mean(dat$SurfaceTemp),
                     DO=mean(dat$DO),
                     Salinity=mean(dat$Salinity))

# The bootstrap_functions.R script also has a function to do this.
#p.data <- expand.pred(best$frame)
p.data <- expand.pred(best$model, re=best$modelInfo$grpVar)

# We may also calculate the marginal mean by expanding the data to get every
# combination of factor in the model and then calculating the mean CPUE
# over all combinations for each year. Your TC/SAS can decide which approach
# is preferred.

index.out <- data.frame(
  Year = as.numeric(unique(dat$Year)), #PICK UP HERE JESS
  #Station=as.character(unique(dat$Station)), #use unique rather than levels bc removed 3 years
  Index= predict(best, newdata=p.data, type="response"))

# We will bootstrap the index to get the CVs and confidence intervals.
# Use boot.NB() for non-zero-inflated models and boot.GAM() for
# GAMs. boot.ZI(), boot.NB(), and boot.GAM() are custom functions included in the 
# "bootstrap_functions.r" file.

# If you have a low percent (<50%) of converged runs, it's a sign that the model
# may not be robust. 

index.out <- cbind.data.frame(index.out, SE2)

index.out$Year <- as.integer(levels(p.data$Year))
allyrs <- 1989:2024
missing <- allyrs[!allyrs %in% p.data$Year]
index.out <- index.out[index.out$Year > 1987,]
index.out <- rbind(index.out, c(1997, -1, -1, -1, -1), c(2010, -1, -1, -1, -1))
index.out <- index.out[order(index.out$Year),]
index.out$CV <- index.out$SE/index.out$Index

jpeg("NY_WLI_index.jpeg", width=8, height=5, units="in",res=600)
ggplot(index.out) + geom_ribbon(aes(x=Year, ymin=LCI, ymax=UCI), alpha=0.4) +
  geom_point(aes(x=Year,y=Index),shape=16)+
  geom_line(aes(x=Year, y=Index)) + 
  ylim(c(0,NA)) + theme_bw()
dev.off()

write.csv(index.out, "NYWLI_index.csv", row.names=F)







# compare to previous index
index.out$scale<-index.out$Index/mean(index.out$Index)
index.out$newYear<-index.out$Year+1
#update<-read.csv("NYWLIindexfromlastupdate.csv",header=TRUE)
#update<-update[-c(1,3,12,27),]
#plot(update$Index)
#update$scale<-update$Index/mean(update$Index)

jpeg("NYWLI_compare.jpeg", width=8, height=5, units="in",res=600)
ggplot(index.out) + 
  geom_point(aes(x=newYear,y=scale),shape=16)+
  geom_line(aes(x=newYear, y=scale),linetype = 1) + 
  #geom_point(data=update,aes(x=Year,y=scale),shape=16,col="blue")+
  #geom_line(data=update,aes(x=Year, y=scale),linetype = 1,col="blue") +
  #geom_path(linejoin = "mitre")+
  ylim(c(0,NA)) + theme_bw()#+ ggtitle("scaled to mean")
dev.off()