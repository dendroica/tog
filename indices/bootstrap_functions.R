#######################################################
# Function to bootstrap negative binomials and        #
# zero-inflated/zero-altered glmmTMB models to get    #
# CVs and confidence intervals                        #
# K. Drew, ASMFC                                      #
# Based on code from Rob Latour, VIMS                 #
# December 2022                                       #
#######################################################

boot.ZI <- function(best=NULL, nboots=1000, CI=0.95, print.prog=T){
  dat <- best$frame
  dat$YEAR <- dat[,grep("YEAR", names(dat), ignore.case = T)]
  yrs = as.numeric(levels(dat$YEAR))
  index = matrix(nrow = length(yrs),ncol = nboots)
  convg <- vector()
  p.data = expand.pred(best$frame, re=best$modelInfo$grpVar)
  
  if(nboots==0){
    SE_CI <- data.frame(SE = NA,
                        LCI = NA,
                        UCI = NA)
  }else{
    for (i in 1:nboots) {
      
      boot.dat <- data.frame()
      
      #Get bootstrap data for current replicate
      for(j in yrs){
        y = dat[dat$YEAR == j,]
        rows = y[sample(nrow(y),size=nrow(y), replace=TRUE),]
        boot.dat = rbind(boot.dat,rows)   
      }
      if(length(grep("offset", names(boot.dat))) > 0){
        o.v <- names(boot.dat)[grep("offset", names(boot.dat))]
        lnE <- regmatches(o.v, gregexpr("(?<=\\().*?(?=\\))", o.v, perl=T))[[1]]
        names(boot.dat)[grep("offset", names(boot.dat))] <- lnE
      }
      
      if(print.prog) print(paste("Starting bootstrap #", i, "out of", nboots))
      
      mod = glmmTMB(best$modelInfo$allForm$formula, 
                    ziformula=best$modelInfo$allForm$ziformula,
                    data = boot.dat, family = best$modelInfo$family,
                    control=glmmTMBControl(optCtrl= list(iter.max=5e3, eval.max=5e3)))
      if (mod$fit$convergence==0 & mod$sdr$pdHess & sum(is.na(mod$sdr$cov.fixed))==0){
        index[,i] = predict(mod, newdata=p.data, type="response")
        if(print.prog) print(mod$fit$message)
      }else{
        index[,i] <- NA
        if(print.prog){
          # print(mod$fit$message)
          print("**Convergence problem**")
        } 
      }
      convg <- c(convg, mod$fit$message)
    }
    c.percent <- round((1-sum(is.na(colSums(index)))/nboots)*100, 1)
    print(paste0(c.percent, "% of bootstrap runs converged."))
    index.keep =index[, which(!is.na(colSums(index)))]
    
    if(CI>1) CI <- CI / 100
    CI <- 1 - CI
    pLCI <- CI/2
    pUCI <- 1-(CI/2)
    
    SE_CI <- data.frame(SE = apply(index.keep, 1, sd),
                        LCI = apply(index.keep, 1, quantile, probs=c(pLCI)),
                        UCI = apply(index.keep, 1, quantile, probs=c(pUCI)))
  }
    
  return(SE_CI)
}


boot.NB <- function(best=NULL, nboots=1000, CI=0.95, print.prog=T){
  dat <- best$frame
  dat$YEAR <- dat[,grep("YEAR", names(dat), ignore.case = T)]
  yrs = as.numeric(levels(dat$YEAR))
  index = matrix(nrow = length(yrs),ncol = nboots)
  convg <- vector()
  p.data = expand.pred(best$frame, re=best$modelInfo$grpVar)
  
  if(nboots==0){
    SE_CI <- data.frame(SE = NA,
                        LCI = NA,
                        UCI = NA)
    
  }else{
    for (i in 1:nboots) {
      
      boot.dat = data.frame()
      
      #Get bootstrap data for current replicate
      for(j in yrs){
        y = dat[dat$YEAR == j,]
        rows = y[sample(nrow(y),size=nrow(y), replace=TRUE),]
        boot.dat = rbind(boot.dat,rows)   
      }
      if(length(grep("offset", names(boot.dat))) > 0){
        o.v <- names(boot.dat)[grep("offset", names(boot.dat))]
        lnE <- regmatches(o.v, gregexpr("(?<=\\().*?(?=\\))", o.v, perl=T))[[1]]
        names(boot.dat)[grep("offset", names(boot.dat))] <- lnE
      }
      
      if(print.prog) print(paste("Starting bootstrap #", i, "out of", nboots))
      
      mod = glmmTMB(best$modelInfo$allForm$formula,
                    data = boot.dat, family = best$modelInfo$family,
                    control=glmmTMBControl(optCtrl= list(iter.max=5e3, eval.max=5e3)))
      
      if (mod$fit$convergence==0 & mod$sdr$pdHess & sum(is.na(mod$sdr$cov.fixed))==0){
        index[,i] = predict(mod, newdata=p.data, type="response")
        if(print.prog) print(mod$fit$message)
      }else{
        index[,i] <- NA
        if(print.prog) print("**Convergence problem**")
      }
      convg <- c(convg, mod$fit$message)
    }
    c.percent <- round((1-sum(is.na(colSums(index)))/nboots)*100, 1)
    print(paste0(c.percent, "% of bootstrap runs converged."))
    index.keep =index[, which(!is.na(colSums(index)))]
    
    if(CI>1) CI <- CI / 100
    CI <- 1 - CI
    pLCI <- CI/2
    pUCI <- 1-(CI/2)
    
    SE_CI <- data.frame(SE = apply(index.keep, 1, sd),
                        LCI = apply(index.keep, 1, quantile, probs=c(pLCI)),
                        UCI = apply(index.keep, 1, quantile, probs=c(pUCI)))
    
  }
  
  
  return(SE_CI)
}

boot.GAM <- function(best=NULL, nboots=1000, CI=0.95, print.prog=T, re=NULL){
  dat <- best$model
  dat$YEAR <- dat[,grep("YEAR", names(dat), ignore.case = T)]
  yrs = as.numeric(levels(dat$YEAR))
  index = matrix(nrow = length(yrs),ncol = nboots)
  convg <- vector()
  p.data = expand.pred(best$model, re=best$modelInfo$grpVar)
  
  if(!is.null(re)){
    re=paste0("s(", re, ")")
  }
  
  if(nboots==0){
    SE_CI <- data.frame(SE = NA,
                        LCI = NA,
                        UCI = NA)
    
  }else{
    for (i in 1:nboots) {
      
      boot.dat = data.frame()
      
      #Get bootstrap data for current replicate
      for(j in yrs){
        y = dat[dat$YEAR == j,]
        rows = y[sample(nrow(y),size=nrow(y), replace=TRUE),]
        boot.dat = rbind(boot.dat,rows)   
      }
      if(length(grep("offset", names(boot.dat))) > 0){
        o.v <- names(boot.dat)[grep("offset", names(boot.dat))]
        lnE <- regmatches(o.v, gregexpr("(?<=\\().*?(?=\\))", o.v, perl=T))[[1]]
        names(boot.dat)[grep("offset", names(boot.dat))] <- lnE
      }
      
      if(print.prog) print(paste("Starting bootstrap #", i, "out of", nboots))
      
      mod = gam(formula(best),
                    data = boot.dat, family = 'nb', method="ML")
      if (mod$converged){
        index[,i] = predict(mod, newdata = p.data, type = 'response',
                            exclude=re, newdata.guaranteed=TRUE)
        
       }else{
        index[,i] <- NA
        if(print.prog) print("**Convergence problem**")
      }
      # convg <- c(convg, mod$fit$message)
    }
    c.percent <- round((1-sum(is.na(colSums(index)))/nboots)*100, 1)
    print(paste0(c.percent, "% of bootstrap runs converged."))
    index.keep =index[, which(!is.na(colSums(index)))]
    
    if(CI>1) CI <- CI / 100
    CI <- 1 - CI
    pLCI <- CI/2
    pUCI <- 1-(CI/2)
    
    SE_CI <- data.frame(SE = apply(index.keep, 1, sd),
                        LCI = apply(index.keep, 1, quantile, probs=c(pLCI)),
                        UCI = apply(index.keep, 1, quantile, probs=c(pUCI)))
    
  }
  
  
  return(SE_CI)
}

## Function to create a dataframe to predict over
## for a generic mode (pass the model$frame as p.dat)
## to calculate the mean for fixed level factors.
expand.pred <- function(p.dat, re=NULL){
  nl <- list()
  
  vars <- names(p.dat)[-1]
  yy <- vars[grep("YEAR", vars, ignore.case=T)]
  vars <- vars[grep("YEAR", vars, ignore.case=T, invert=T)]
  
  nl[[1]] <- levels(p.dat[,yy])
  
  for (i in 1:length(vars)) {
    if (is.factor(p.dat[, vars[i]])) {
      nl[[i+1]] <- levels(p.dat[, vars[i]])[1]
    } else {
      nl[[i+1]] <- round(mean(p.dat[, vars[i]]), 3)
    }
  }
  
  names(nl) <- c(yy, vars)
  if(length(grep("offset", names(nl))) > 0){
    o.v <- names(nl)[grep("offset", names(nl))]
    lnE <- regmatches(o.v, gregexpr("(?<=\\().*?(?=\\))", o.v, perl=T))[[1]]
    names(nl)[grep("offset", names(nl))] <- lnE
  }
  nl.ex <- expand.grid(nl)
  if(!is.null(re)){
    nl.ex[,re] <- NA
  }
  return(nl.ex)
}  
  
  
# Function to calculate the geometric mean of a vector
geomean <- function(dat.in, adj=1){
  dat.in <- dat.in + 1
  log_data <- log(dat.in)
  gmean <- exp(mean(log_data)) - 1
  return(gmean)
}

# Function to bootstrap the geometric mean to get
# the standard deviation and CIs
boot.geo_mean <- function(data, nrep = 500) {
  storeg <- NULL
  log_data <- log(data + 1)
  for (t in 1:nrep) {
    dodo <- sample(log_data, length(log_data), replace = TRUE)
    gmean <- exp(mean(dodo)) - 1
    storeg <- c(storeg, gmean)
  }
  SEB <- sd(storeg)
  confint <- as.numeric(quantile(storeg, probs = c(0.025, 0.975)))
  
  vals <- data.frame("SE" = SEB, "LCI" = confint[1], "UCI" = confint[2])
  
  return(vals)
}