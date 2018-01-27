

rm(list=ls())

source("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/02_Codes/loadpackage.R")
source("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/02_Codes/auxfunctions_v2.R")
library(lubridate)
# library(ggplot2)
library(scales)  # this for scale date on x axis
library(tidyr)
library(reshape2)
# library(sqldf)
library(dplyr)

setwd("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/03_Output/")



#########################################################################
### Load Data
#########################################################################

#Load sales promo wide data
load("./DMA_Results/area_sales_promo_wide_DM.RData")

#Load Sales average data 
load("004A_Sales_Promo_DM.RData")

#load reference channel 
ref_channel <- data.frame(channel = c("GP_FP_DETAILING", "SP_DETAILING","MEETINGS", "MS_DETAILING", "SAMPLES", "MAILING"), channel_id = c(1:6), stringsAsFactors = FALSE)
save(ref_channel, file = "ref_channel.RData")

#Note: layout of "area_sales_promo_wide" data should be similar to this --  1. area_id, 2. month_id, 3. sales_norm, a series of other control variables, a series of promo variables.
N_NonePromo <- 4 # include intercept, time trend(can be month_id or other), additional control variables.

N_Channel <- 6

# Position of sales_norm on the "area_sales_promo_wide" data
Sales_Norm_Position <- 4  



tpdat_all <- area_sales_promo_wide
b_self_prior_overall <- c(0.3, 0.3, 0.17, 0.05, 0.01, 0.01)  #use the same as in 004B code

#Area level set 
area_cd_set <- sqldf("select distinct area_id from tpdat_all order by area_id")
n_area <- nrow(area_cd_set)


# function to give more weight to more recent data
time_wgt <- function(v_wk){
  wgt = (v_wk)^(1/4)
  r = length(v_wk)*(wgt/sum(wgt)) # normalize wgt s.t. wgt sum up to length(v_wk)
  return(r)
}

# ridge regression loss function
# b_lambda = c(lambda, b) -- lambda is the penalty factor
RidgeRSS <- function(X,y,b_prior,b_lambda, weight=NULL){
  if(is.null(weight)) {weight=rep(1,length(y))} # if no weight input, then evenly assign weight
  return( sum( weight*(y - X%*%(b_lambda[-1]))^2 ) + b_lambda[1] * sum(((b_lambda[-1])-b_prior)^2) )  
}

# solve ridge regression by optim
# b_lambda = c(lambda, b)
# b_lambda_init is the initial value for c(lambda, b)
# b_lambda_lower is the lower bound for c(lambda, b)
nnridge <- function(X,y,b_prior,b_lambda_init,b_lambda_lower=-Inf, b_lambda_upper=Inf, weight=NULL){
  p = length(b_prior)
  bfgsOptimConst = optim(RidgeRSS, X=X, y=y, b_prior=b_prior, weight=weight,
                         par=b_lambda_init, lower=b_lambda_lower, upper=b_lambda_upper,
                         method = 'L-BFGS-B')
  par_est = bfgsOptimConst$par
  return(list(lambda=par_est[1], b=par_est[-1]))
}


################################################
# (1) get a beta prior on the overall data  
################################################

x0 <- tpdat_all[ , -c(1,Sales_Norm_Position)] # drop area_i and sales_norm
#x0 <- as.matrix(cbind(1, x0[,"month_id"], x0[,"dummy_peak"], -x0[,"dummy_deep"], log(1+x0[,-c(1:(N_NonePromo - 1))]))) # add intercept "1", and set dummy_deep as negative
x0 <- as.matrix(cbind(1, x0[,"month_id"], - log(1 + x0[,"sales_others"]), x0[,"dummy_peak"], log(1 + x0[,-c(1:(N_NonePromo - 1))]))) # add intercept "1", and set dummy_deep as negative
y0 <- log(1+tpdat_all[, Sales_Norm_Position]) # Note: log of (1 + sales_norm)
var0 <- var(y0)

lambda0_init <- (1/var0)
lambda0_lower <- lambda0_init*0.0001  #the set of lower bound has impact on model fitness
lambda0_upper <- lambda0_init*2

mth0 <- tpdat_all$month_id
wgt0 <- time_wgt(mth0)

r0 <- nnridge(X=x0,y=y0,
             b_prior=c(rep(0, N_NonePromo), b_self_prior_overall),    #note: b_self_prior_overall is the priors only for promotional variables 
             b_lambda_init=c(lambda0_init, rep(0, N_NonePromo), b_self_prior_overall), #note: the penalty factor is on the first position
             b_lambda_lower=c(lambda0_lower, rep(0, N_NonePromo + length(b_self_prior_overall))),
             b_lambda_upper=c(lambda0_upper, rep(Inf, N_NonePromo + length(b_self_prior_overall))),
             weight = wgt0
)

rsqr0 <- 1- sum( wgt0*(y0 - x0%*%r0$b)^2 )/sum( wgt0*(y0 - mean(y0))^2 )

print(rsqr0)
# 0.654403  --this seems to be close to the highest weighted RSquare.

print(r0$b)
#[1] 0.03131913

print(r0$lambda)
#[1] 0.55608848 0.00000000 0.07883667 0.02743114 0.07939727 0.02748482 0.02385144 0.01354885 0.04015964 0.00000000


################################################
# (2) estimate on DMA level
################################################

#b_self_prior_overall <- c(0.07939727, 0.02748482, 0.02385144, 0.01354885, 0.04015964, 0.01)  #it turns out much better if re-setting priors to posteriors from above model
# b_self_prior_overall <- c(0.10, 0.02748482, 0.02385144, 0.01354885, 0.04015964, 0.01)
b_self_prior_overall <- c(0.3, 0.3, 0.17, 0.05, 0.01, 0.01) # This prior (0.16 for detailing) works better in terms detailing posterior coeff and mode fit-- Aug 12, 2017
b_prior_area <- c(rep(0, N_NonePromo), b_self_prior_overall) # intercept, time, other promo channels, and promotion channels


# a solver with input = tpdat (merged rx and promo data) and output = estimation of b and contribution scores of each promo channel
score_channel <- function(tpdat, b_prior){
  x1 = tpdat[, -c(1, Sales_Norm_Position)]  # drop area_i and sales_norm
  #x1 = as.matrix(cbind(1, x1[,1], -log(1+x1[,2]), log(1+x1[,-(1:2)]))) # add intercept "1", and set competitors NRx as negative
  x1 = as.matrix(cbind(1, x1[,"month_id"], -log(1+x1[,"sales_others"]), x1[,"dummy_peak"], log(1+x1[,-c(1:(N_NonePromo - 1))])))  #note-- control variables need to set and updated if necessary
  y1 = log(1+tpdat[, Sales_Norm_Position])  # Note: log of (1 + sales_norm)
  var1 = var(y1)
  # var1 = 1/var(y1)
  lambda1_init = (1/var1)
  lambda1_lower = (1/var1)*0.0001    # the lambda1_lower may need to change.
  #lambda1_lower = 0              #Note: if set lambda1_lower to 0, then the Rsquare for each area will be slightly increase.
  lambda1_upper = (1/var1)*5   #originally, it is set to 10 in the case for USA projects
  
  mth1 = tpdat$month_id
  wgt1 = time_wgt(mth1)
  
  r1 = nnridge(X=x1,y=y1,
               b_prior=b_prior,
               b_lambda_init=c(lambda1_init, b_prior),
               # b_lambda_lower=c(lambda1_lower, rep(0, length(b_prior))),
               b_lambda_lower=c(lambda1_lower, 0, -Inf, rep(0, length(b_prior)-2)),
               b_lambda_upper=c(lambda1_upper, rep(Inf, length(b_prior))),
               weight=wgt1
  )
  
  # Yan added --
  
  cat("initial lambda: ", lambda1_init, "\n")
  cat("lambda lower bound: ", lambda1_lower, "\n")
  cat("lambda upper bound: ", lambda1_upper, "\n")
  cat("lambda selected: ", r1$lambda, "\n")
  
  # End of the addition.
  
  # b1 = ifelse(r1$b>=0, r1$b, 0) #-- the original setup
  b1 = r1$b  #this is for -Inf time trend/intercept
  b1[-c(1:N_NonePromo)] <- ifelse(b1[-c(1:N_NonePromo)]>=0, b1[-c(1:N_NonePromo)], 0) # (1:N_NonePromo) are for intercept, time trend and other control variables; make sure coefficients on promo variables are positive

  
  b_int = b1[1] # intercept
  b_time = b1[2] # time trend
  
  # b_comp = b1[3] # Need to updated accordingly for other control variables
  b_sales_others = b1[3] # Need to updated accordingly for other control variable
  b_dummy_peak = b1[4] # Need to updated accordingly for other control variabless
  #b_dummy_deep = b1[4] # Need to updated accordingly for other control variables
  
  b_self = b1[-c(1:N_NonePromo)] # for promo channels
  
  yhat = x1%*%b1 # predicted y response
  
  rsqr = 1- sum( wgt1*(exp(y1) - exp(yhat))^2 )/sum( wgt1*(exp(y1) - mean(exp(y1)))^2 ) # to calculate r-square
  
  # v = list(b_int=b_int, b_time=b_time, b_comp=b_comp, b_self=b_self, yobs=exp(y1)-1, yhat=exp(yhat)-1, rsqr=rsqr)
  v = list(b_int=b_int, b_time=b_time 
           
           , b_sales_others = b_sales_others  # Need to updated accordingly for other control variables
           , b_dummy_peak = b_dummy_peak  # Need to updated accordingly for other control variables
           #, b_dummy_deep = b_dummy_deep  # Need to updated accordingly for other control variables
           
           , b_self=b_self
           , yobs=exp(y1)-1
           , yhat=exp(yhat)-1
           , rsqr=rsqr
           )
  
  return(v)
  
}


# To solve parameters and score for each area level
b_array_area = data.frame(array(0, dim=c(n_area, 1+length(b_prior_area)))) 
names(b_array_area) = c('area_id', 'b_int', 'b_time', 'b_sales_others', 'b_dummy_peak',  paste('b_self', 1:N_Channel, sep="_"))  #should be updated
v_rsqr_area = rep(0, n_area)

yraw_area = c()
yhat_area = c()
v_mth = c()

for(i in 1:n_area){
  
  print(paste("i = ", i, " / ", n_area, sep=""))
  area_cd_ii = area_cd_set$area_id[i]
  
  tpdat_ii = tpdat_all[which(tpdat_all$area_id==area_cd_ii),] # subset data for i-th area
  sales_avg_ii = sales_avg_area[which(sales_avg_area$area_id==area_cd_ii),]$sale_mean
  
  rr = score_channel(tpdat_ii, b_prior=b_prior_area) # to solve the par of i-th area
  
  b_array_area[i,] = c(area_cd_ii, rr$b_int, rr$b_time, rr$b_sales_others , rr$b_dummy_peak , rr$b_self)  #Note: need to update for other control variables
  v_rsqr_area[i] = rr$rsqr  
  
  yraw_area = c(yraw_area, rr$yobs*sales_avg_ii)
  yhat_area = c(yhat_area, rr$yhat*sales_avg_ii)
  v_mth = c(v_mth, tpdat_ii$month_id)
  
}
row.names(b_array_area) = NULL

# > which(b_array_area$b_int <0.1)
# [1]  1  2  6 11 13 17 22 31 39 40

# > which(v_rsqr_area<0)
# [1]  1  2  6 11 13 17 22 31 39 40


# Overall model fitting using area level data

y_obs_hat_area = data.frame(area_id = rep(1:n_area, each = 17), mth=v_mth, yraw=yraw_area, yhat=yhat_area) # this step the y is the average NRx within each DMA, still not the raw total NRx
y_obs_hat_overall = sqldf("select mth, sum(yraw) as yraw, sum(yhat) as yhat from y_obs_hat_area group by mth order by mth")
rsqr_overall = 1-sum((y_obs_hat_overall$yraw-y_obs_hat_overall$yhat)^2)/sum((y_obs_hat_overall$yraw-mean(y_obs_hat_overall$yraw))^2)
rsqr_overall  # 0.9606278

rsqr_area_month_level<- 1-with(y_obs_hat_area, sum((yraw_area - yhat_area)^2))/with(y_obs_hat_area, sum((yraw_area - mean(yraw_area))^2))
rsqr_area_month_level  # 0.9772227 -- almost no change than before

print(v_rsqr_area)
print(sum(v_rsqr_area<0))  # 1 areas with bad fitting

# Check base
sum(b_array_area$b_time<0)  # 1 areas with negative time trend. 

for_base_cal<- tpdat_all[ , -grep("^adstock|^sales_norm$", colnames(tpdat_all))] %>% 
  left_join(b_array_area[,-grep("^b_self_",colnames(b_array_area))], by=c("area_id"="area_id"))

for_base_cal <- mutate(for_base_cal, base = b_int + month_id * b_time - log(1+sales_others)*b_sales_others + dummy_peak*b_dummy_peak)
sum(for_base_cal$base<0) # it is best that all equals to 0. otherwise, please check how many and why, then probably need to adjust control variables and ridge reg setup
# End of check base


#output results for QC
# write.csv(b_array_area, "./DMA_Results/area_model_coefficients_lowerbound_adj.csv", row.names = FALSE)
# write.csv(y_obs_hat_area, "./DMA_Results/y_obs_hat_area_lowerbound_adj.csv", row.names = FALSE)
# write.csv(tpdat_all, "./DMA_Results/area_tpdat_all_lowerbound_adj.csv", row.names = FALSE)


tiff(filename="./DMA_Results/dma_overall_fit.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
plot(y_obs_hat_overall$mth, y_obs_hat_overall$yraw, type='l', col='black', lwd=2, xlab='Month', ylab='Total Sales', main='Total Sales over time')
lines(y_obs_hat_overall$mth, y_obs_hat_overall$yhat, lwd=2, col='green')
legend(x="bottomright",legend=c('Observed', 'Predicted'), lty=c(1,1), lwd=c(2,2), col=c('black','green'))
dev.off()
write.csv(y_obs_hat_overall, "./DMA_Results/y_obs_hat_overall.csv", row.names = FALSE)

# shrink outliers to 5 percentile or 95 percentile
b_array_area_est <- b_array_area
#note: the function has been replace by shrink_to_qtl2_v2 becaue there are much fewer number of areas than in USA (the DMA)
b_array_area_est[-(1:(N_NonePromo+1))] <- apply(b_array_area_est[-(1:(N_NonePromo+1))], 2, function(x){shrink_to_qtl2_v2(x, 0.05, 0.95)}) 

# save area level par estimations 
save(b_array_area_est, file = "./DMA_Results/b_array_area_est.RData")
write.csv(b_array_area_est, "./DMA_Results/b_array_area_est.csv", row.names= FALSE)


# boxplot of area level par estimations at each promo channel
b_array_area_long_tmp1 <- b_array_area_est[,-(2:(N_NonePromo+1))]
names(b_array_area_long_tmp1) <- c('area_id', ref_channel$channel[1:N_Channel])
b_array_area_long = melt(b_array_area_long_tmp1, id='area_id') # melt wide table to long

names(b_array_area_long)[2:3] = c('channel', 'b_est') # rename variables

dd=ggplot(na.omit(b_array_area_long), aes(x=channel, y=b_est)) +
  geom_boxplot() +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.001, binwidth=0.01) +
  scale_x_discrete(name = "Channel") +
  scale_y_continuous(breaks=seq(0, 10, by = 0.05), name = "Impact")
tiff(filename="./DMA_Results/par_est_boxplot_DMA.tiff", res=600, compression = "lzw", height=5, width=10, units="in")
print(dd)
dev.off()
