root <- "C:/Users"
usr <- "jgorzo"
loc <- "OneDrive - New Jersey Office of Information Technology/Documents"
root <- file.path(root, usr, loc)
load(file.path(root, "output/tog/index/NJOTmodel.RData"))
library(ggplot2)
source(file.path(root, "data/tog/bootstrap_functions.R"))
# Stratified mean:
# Standardize catch for 20 minute tow (the standard unit of effort of NJOT)
dat$Fish <- round((dat$CPUE*20)/dat$EFFORT,0)


# specify stratum weights (km2):
w <- c(14.7,32.5,76.7,17.4,81.3,293,59.9,235.9,279.9,17.1,198.7,185.1,34,133.6,131.9)
w <- w[c(1,4,7,10,13)] # just the isnhore strata
# and scale them so they sum to 1:
wts <- w/sum(w)


#mean (using only inshore strata):
M <- with(dat, tapply(Fish,list(STRATA,YEAR),mean,na.rm=TRUE))
#variance:
V <- with(dat, tapply(Fish,list(STRATA,YEAR),var,na.rm=TRUE))


# Possible workaround if a YEAR-stratum combination has NA variance (i.e., from n= 1), pool mean variance from entire YEAR:
needVar <- which(is.na(V))
if(length(needVar)>0) {
  allYEARs <- as.numeric(as.character(sort(unique(dat$YEAR))))
  # YEARs of interest:
  yoi <- rep(allYEARs,each=5)[needVar] # just using the 5 inshore strata
  lookupTable <- data.frame(YEAR=allYEARs,.var=apply(V,2,mean,na.rm=TRUE))
  # The special case of all NAs in a given YEAR:
  # If NaN, then pool timeseries mean variance
  if(any(is.nan(lookupTable$.var))) {
    mod <- which(is.nan(lookupTable$.var))
    lookupTable$.var[mod] <- mean(lookupTable$.var[-mod])
  }
  ans <- merge(lookupTable,data.frame(YEAR=yoi,tmp=NA),by="YEAR",sort=FALSE)
  V[which(is.na(V))] <- ans$.var
}


N <- with(dat, tapply(Fish,list(STRATA,YEAR),length))

# UCL
UCL <-
  apply(M*wts,2,sum,na.rm=TRUE)+
  ((sqrt(apply(wts^2*V/N,2,sum,na.rm=TRUE)))*qnorm((1-0.95)/2,lower=FALSE))

# stratified mean:
.m <- apply(M*wts,2,sum,na.rm=TRUE)

# LCL
LCL <-
  apply(M*wts,2,sum,na.rm=TRUE)-
  ((sqrt(apply(wts^2*V/N,2,sum,na.rm=TRUE)))*qnorm((1-0.95)/2,lower=FALSE))

SE <- sqrt(apply(wts^2*V/N,2,sum,na.rm=TRUE))
CV <- SE/.m

nom <- data.frame("YEAR"=as.numeric(as.character(unique(dat$YEAR))),"Index"=.m,SE,
                  "LCI"=LCL,"UCI"=UCL,CV,"Method"="stratifiedMean")

p.data <- expand.pred(NB00$frame)
best <- NB00
index.out <- data.frame(
  YEAR = as.numeric(unique(as.character(dat$YEAR))), #PICK UP HERE JESS
  #Station=as.character(unique(dat$Station)), #use unique rather than levels bc removed 3 YEARs
  Index= predict(best, newdata=p.data, type="response"))
index.out <- cbind.data.frame(index.out, out)
#gaps <- which(diff(x) > 1) + 1 # Identify the index of the element after the gap
#missing_numbers <- x[gaps] - 1 # Find the missing number(s)
index.out <- rbind(index.out, c(2020, -1, -1, -1, -1), c(2021, -1, -1, -1, -1))
index.out$CV <- index.out$SE/index.out$Index

index.out <- index.out[order(index.out$YEAR),]
index.out$Method="NB"
write.csv(index.out, "octrawl.csv")
tmp <- rbind(index.out,nom)
tmp


ggplot(tmp, aes(x=YEAR, y=Index, color=Method, shape=Method)) +
  geom_ribbon(aes(x=YEAR, ymin=LCI, ymax=UCI, fill=Method), alpha=0.5) +
  geom_line() + geom_point() +
  xlab("YEAR") + theme_bw()

# scale indices to respective means:
op <- par(mfrow=c(1,1))
#matplot(unique(tmp$YEAR),sapply(split(tmp,tmp$Method), function(x) scaleToMean(x$Index)),
#       type="o",xlab="YEAR",ylab="index scaled to mean")

#plot(sapply(split(tmp,tmp$Method), function(x) scaleToMean(x$Index)))
round(cor(sapply(split(tmp,tmp$Method), function(x) x$Index)),2)
round(cor(sapply(split(tmp,tmp$Method), function(x) x$Index),method="spearman"),2)

Sys.time()