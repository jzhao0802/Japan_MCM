

rm(list=ls())
source("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/02_Codes/loadpackage.R")
library(lubridate)
# library(ggplot2)
library(scales)  # this for scale date on x axis
library(tidyr)
library(reshape2)
# library(sqldf)
library(dplyr)

setwd("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/03_Output/")

load("004A_Sales_Promo_DM.RData")


sales_ <- Sales_MCM_area_norm[ Sales_MCM_area_norm$sales_norm >0 , ]  # To make sure all sales data points are positive

tpdat <- Sales_Promo_area[Sales_Promo_area$sales_norm >0 , ] # To make sure all sales data points are positive
# note: dimension of the two data created are same as the source data in this case.

n <- nrow(sales_) # number of rows 992

NChannel <- n_distinct(Sales_Promo_area$channel_id) #total number of channels

# set random seed
set.seed(11)


# decay factor -- lambda
#order of promo channel  -- 1. GP_FP_DETAILING, 2. SP_DETAILING, 3.MEETINGS, 4.MS_DETAILING, 5.SAMPLES, 6.MAILING
lam_mean <- c(0.8,  0.8,  0.5,  0.8, 0.5, 0.7)   # mean of lambda; 
lam_lbd <- lam_mean-0.1 # lower bound of lambda
lam_ubd <- lam_mean+0.1 # upper bound of lambda
lam_sd <- rep(0.1, NChannel) # standard deviation of lambda

# impact factor -- beta
# b <- c(0.3, 0.001, 0.03, 0.003, 0.003, 0.001)
# b<- c(0.3, 0.01, 0.03, 0.01, 0.01, 0.01)  #RSquare of this model is:  0.2402011 
b<- c(0.3, 0.3, 0.17, 0.05, 0.01, 0.01)  #RSquare of this model is:  0.2415819
x_init <- lam_mean

############################
# log-likelihood function
############################

llx <- function(param, write_to_csv = FALSE, check_fit = FALSE){
  
  # assign parts of the long vector of param to each parameter: lambda
  lam <- abs(param)
  
  xxdat <- tpdat

  xxdat$ad <- (xxdat$pde)*((lam[xxdat$channel_id])^(xxdat$gap_sales_promo)) # decayed promotion: N_j^{l} \lam_j^{t-l}
  
  # Sum up of decayed promotion for each NRx + each channel: \sum_t N_j^{l} \lam_j^{t-l}
  yydat <- aggregate(xxdat$ad, list(xxdat$area_id , xxdat$month_id, xxdat$channel_id, xxdat$sales_norm, xxdat$sales_others), sum)
  names(yydat) <- c("area_id ", "month_id", "channel_id", "sales_norm", "sales_others", "ad_sum") # re-name variables, ad_sum is the aggregated sum
  
  # normalize each channel's adstock
  yydat$ad_norm <- yydat$ad_sum/(aggregate(yydat$ad_sum, list(yydat$channel_id), mean)$x)[yydat$channel_id]  
  
  yydat$ad_norm <- ifelse(is.nan(yydat$ad_norm) | is.infinite(yydat$ad_norm), 0, yydat$ad_norm)
  
  # sum up adstock multiplying impact factor over channels
  zzdat <- aggregate(b[yydat$channel_id]*log(1+yydat$ad_norm), list(yydat$area_id, yydat$month_id, yydat$sales_norm, yydat$sales_others), sum)
  names(zzdat) <- c("area_id", "month_id", "sales_norm", "sales_others", "bx") 
  
  #### Add dummy for control high points
  zzdat$dummy_peak <- ifelse(zzdat$month_id %in% c(8), 1, 0)
  zzdat$dummy_peak <- ifelse(zzdat$month_id %in% c(14), 2, zzdat$dummy_peak)
#  zzdat$dummy_deep <- ifelse(zzdat$month_id %in% c(9, 10), 1, 0)
  
  #### End of adding customized control variables
  
  if(write_to_csv){
    
    write.csv(zzdat, "zzdat_QC.csv", row.names = FALSE)
    
  }
  
  zzdat$res1 <- zzdat$sales_norm - zzdat$bx
  lmfit <- lm(res1~month_id + sales_others + dummy_peak #+ dummy_deep
              , data=zzdat)
  res <- lmfit$residual  # residual
  v <- var(res)  
  
  if(check_fit){
  
  #check Rsquare
  zzdat$res1_pred <- predict(lmfit, zzdat)
  # print(table(zzdat$res1 - res1_pred  - res))
  # cat("Min: ", min(zzdat$res1 - res1_pred  - res), " Max: ", max(zzdat$res1 - res1_pred  - res), "\n")
  
  zzdat$sales_norm_pred <- zzdat$res1_pred + zzdat$bx
  
  R_Square <- 1 - sum((lmfit$residual)^2)/with(zzdat,sum((res1 - mean(res1))^2))
  
  R_Square2 <- 1 - sum((zzdat$sales_norm - zzdat$sales_norm_pred)^2)/with(zzdat, sum((sales_norm - mean(sales_norm))^2))
  
  cat("\n RSquare of fitting residual model on Area and Month level is: ", R_Square, "\n")
  
  cat("\n RSquare2 of fitting original sales model on Area and Month level is: ", R_Square2, "\n")
  
  ss2 <- group_by(zzdat, month_id) %>% summarise(a1 = sum(sales_norm), a2 = sum(sales_norm_pred)) %>% ungroup()
  
  R_Square4 <- 1- with(ss2, sum((a1-a2)^2))/with(ss2, sum((a1-mean(a1))^2))
  # cat("\n RSquare of fitting residual model on Area level is: ", R_Square3, "\n")
  
  cat("\n RSquare4 of fitting original sales model on Area level is: ", R_Square4, "\n")
  
  }
  
  
  return(-(length(res)/2)*log(v)-sum(res^2)/(2*v)) # return log-likelihood value
  
}

############################
# log-prior function
############################

log_prior <- function(param){
  
  # assign parts of the long vector of param to each parameter: lambda
  lam=abs(param)
  
  # lambda -- truncated normal distribution
  lam_prior = sum(log(dtruncnorm(lam, a=lam_lbd, b=lam_ubd, mean = lam_mean, sd = lam_sd)))
  
  return(lam_prior)
}


########################################################
# Objective function
########################################################

# objective function = log-likelihood + log-prior
fQP <- function(param){
  return(llx(param)+log_prior(param))
}


########################################################
# (1) Adaptive MCMC to solve optimized paramters
########################################################
llx(x_init, check_fit = TRUE)
# llx(x_init, write_to_csv = TRUE)
# 0.9672511
# 231.6086
log_prior(x_init)
# 12.35753
fQP(x_init)
#  229.2951

Sys.time()->start

set.seed(20)

n_iter <- 200  #set the iteration number.
# generate MCMC
mcmcsamp_dma_v2_1 <- MCMC(fQP, n_iter, init=x_init, scale=lam_sd, adapt=TRUE, acc.rate=0.234)
print(Sys.time()-start)

save(mcmcsamp_dma_v2_1, file = "./DMA_Results/mcmcsamp_dma_v2_1.RData")

x_cur <- mcmcsamp_dma_v2_1$samples[n_iter,]
llx(x_cur)
# 220.4588
log_prior(x_cur)
# 11.24491
fQP(x_cur)
# 231.7037


############################################################################
## (2) Plots of convergence of parameter estimation over iterations
############################################################################

# load("./DMA_Results/mcmcsamp_dma_v2_1.RData")
mcmcsamp = mcmcsamp_dma_v2_1

# iterations of MCMC
v <- 1: dim(mcmcsamp$samples)[1]

lam_chain=abs(mcmcsamp$samples[v, 1:NChannel]) # MCMC of lambda
# Lambda -- Decay factor (for N Channels)
for (i in 1:ncol(lam_chain)){
  png(filename=paste("./convergence/", "Lambda_", i, ".png", sep=""))
  plot(v, lam_chain[v,i], type='l', xlab='Iterations', ylab='Estimate', main=paste("Decay factor @ Channel_", i, sep=""))
  dev.off()
}

# logp - Log-likelihood
png(filename=paste("./convergence/", "logp.png", sep=""))
plot(v, mcmcsamp$log.p[v], type='l', xlab='Iterations', ylab='Log-likelihood', main="Log-likelihood")
dev.off()


######################################################
## (3) Parameter estimations
######################################################

# load("./DMA_Results/mcmcsamp_dma_v2_1.RData")
mcmcsamp = mcmcsamp_dma_v2_1

burn_in_pct = 0.01

# Burn-in part of the chain used for parameter estimations
Burn_in = (round(dim(mcmcsamp$samples)[1]*(1-burn_in_pct))):(dim(mcmcsamp$samples)[1])
lam_chain=abs(mcmcsamp$samples[Burn_in, 1:NChannel])

# parameters are estimated as mean of the Burn-in MCMC
lam_est=apply(lam_chain, 2, mean)

# save decay parameter to .csv file
par_decay = data.frame(chnl_cd=as.numeric(1:length(lam_est)), lam_est=lam_est)

write.csv(par_decay, file="./DMA_Results/par_decay_DM.csv", row.names=FALSE)
save(par_decay, file="./DMA_Results/par_decay_DM.RData")

# channel	chnl_cd
# Alerts	1
# Call Center	2
# Detailing	3
# Direct Mail	4
# Email	5
# Print	6
# Sample	7
# Paid Search	8
# Digital Display	9
# Web	10

