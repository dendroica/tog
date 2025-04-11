#Let's try some simple code in R
library(fishmethods)
#set your working directory in drop-down menu: Session -> Set Working Directory -> To Source File Location

#read in data file "SimpleR.txt"
# (Didn't save SimpleR.txt with SimpleR.r? Set Working Directory -> Choose Directory... and choose the folder where you did save it.)

data=read.delim("C:/Users/jgorzo/Downloads/SimpleR.txt")

#print out data file to screen
data
#or check out data in Environment ->

#plot data
plot(data)
#fancier...add lines
lines(data)

#create predicted lengths from our Excel age regression
Linf <- 21.89
K <- 0.66
t0 <- 1.51

data$Pred.Length <- Linf*(1-exp(-K*(data$Age - t0)))

data

#Go crazy...add title and more detailed axis labels
plot(data$Age, data$Length, main="Yellow perch length at age",ylab="Length (cm)",xlab="Age (years)")

lines(data$Age, data$Pred.Length, col="blue")

#Ctrl L -- clears the console (history remains)
#show old code with up and down arrows

#Package Installation

#1. Go to the Packages tab -> Install
#2. Start typing the name of the package you want; popular ones will autocomplete
#   fishmethods
#3. Click "Install"
#4. Wait for it......
#5. You're done when you see the prompt carrot again (>) and fishmethods shows up in the packages list

