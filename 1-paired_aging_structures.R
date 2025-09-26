library(readxl)
library(ggplot2)
setwd("C:\\Users\\jgorzo\\OneDrive - New Jersey Office of Information Technology\\Documents\\data\\")
#here's my comment!
#INPUT#####################
#lines below won't work if file is open
ny_rec <- read_excel("2025SA_NY_Tautog Data 2021-2023_corrected.xlsx", sheet="Paired Age Samples", range = cell_rows(6:208))
##########################

ny_rec$state <- "NY"
ny_rec$oto <- ny_rec$`Otolith Age`
ny_rec$`Otolith Age` <- NULL
ny_rec <- ny_rec[!is.na(ny_rec$`Operculum Age`) & ny_rec$`Operculum Age` != "No Operc",]
ny_rec$operc <-as.integer(ny_rec$`Operculum Age`)
ny_rec$`Operculum Age` <- NULL

#plot(ny_rec$oto, ny_rec$operc)
#ny_rec$mode <- "rec"

#Save summary of the linear model
fit<-lm(operc~oto, data = ny_rec[ny_rec$Region=="NYB",])
sum_lm<-summary(lm(operc~oto, data = ny_rec))

#Get coefficients
coef_lm<-sum_lm$coefficients

#Plot
ggplot(ny_rec[ny_rec$Region=="NYB",], aes(x = oto, y = `Total Length (cm)`)) +
  geom_bin2d(binwidth = 1) + 
  stat_bin2d(geom = "text", aes(label = ..count..), binwidth = 1, colour="white") +
  scale_fill_gradient(low = "blue", high = "red") +
  #geom_smooth(method = lm) +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))

#Plot
ggplot(ny_rec[ny_rec$Region=="NYB",], aes(x = oto, y = operc)) +
  geom_bin2d(binwidth = 1) + 
  stat_bin2d(geom = "text", aes(label = ..count..), binwidth = 1, colour="white") +
  scale_fill_gradient(low = "blue", high = "red") +
  #geom_smooth(method = lm) +
  labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                     "Intercept =",signif(fit$coef[[1]],5 ),
                     " Slope =",signif(fit$coef[[2]], 5),
                     " P =",signif(summary(fit)$coef[2,4], 5)))

#Set the abline as the coefficients obtained by your linear model
#abline(a = coef_lm[1,1], 
#       b = coef_lm[2,1], 
#       col = "lightblue",
#       lwd = 2)

#Add text to the plot in the x and y position (you might need to adjust these values according to your data)
#and add as labels a string that pastes all the info you wish to include. \n is interpreted as a line break. 
#text(x = max(ny_rec$oto)-6, 
#     y = min(ny_rec$operc)+6, 

#     labels = paste0("R^2 = ",
#                     #Round r.squared to 2 decimals
#                     round(sum_lm$r.squared,2),
#                     "\np-value = ",
                     #Round p.value of slope to 2 decimals
#                     round(coef_lm[2,4],2),
#                     "\ny = ", 
                     #Round slope coefficient to 2 decimals
#                     round(coef_lm[2,1],1),
#                     "x + ", 
                     #Round intercept coefficient to 2 decimals
#                     round(coef_lm[1,1],2)),
#     pos = 4)

