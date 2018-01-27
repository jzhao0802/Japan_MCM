

######
###### This version is complete, and it is the final version to work on --Aug 18, 2017
###### Trend and Dummies are futher adjusted to get better results.


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


setwd("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/03_Output")

N_Channel <- 6
Sales_Norm_Position <- 3  
N_NonePromo <- 4 # include intercept, time trend(can be month_id or other), additional control variables.


# Load data--
# main HP model data
load("hp_sales_promo_wide.RData")

# area level estimate
load("./DMA_Results/b_array_area_est.RData")

# average sales
load("sales_hp_avg.RData")

########################
## Model
########################

## National average in case some HPs have no area information
b_area_est_avg <- apply(b_array_area_est[,-1],2,mean)


# All HPs set
hp_cd_set <- sqldf("select distinct facility_id from hp_sales_promo_wide order by facility_id")
n_hp <- nrow(hp_cd_set)
hp_cd_set <- data.frame(hp_cd_set, hp_cd = c(1:n_hp))   # assign numeric id to each HP
# write.csv(hp_cd_set, "hp_cd_set_for_hp_model.csv", row.names = FALSE)  #exported on Nov 21, 2017

hp_sales_promo_wide<- hp_cd_set %>% inner_join(hp_sales_promo_wide, by = "facility_id") # append the new hp_cd.

ref_hp <- unique(hp_sales_promo_wide[, c("hp_cd","area_id", "facility_id")])  # this is to retrieve area_id for each facility


tpdat_all <- hp_sales_promo_wide[, -grep("^area|^facility", colnames(hp_sales_promo_wide))]  #just remove area id and original facility id 


### Customized the market event

# tpdat_all$dummy_peak <- ifelse(tpdat_all$month_id %in% c(1, 5, 13), 1, 0)
# tpdat_all$dummy_peak <- ifelse(tpdat_all$month_id %in% c(1, 5), 1, 0)
tpdat_all$dummy_peak <- ifelse(tpdat_all$month_id ==8 , 1, 0)
tpdat_all$dummy_peak <- ifelse(tpdat_all$month_id ==14 , 2, tpdat_all$dummy_peak)

# tpdat_all$dummy_deep <- ifelse(tpdat_all$month_id %in% c(9, 10), 1, 0)
#tpdat_all$dummy_deep <- ifelse(tpdat_all$month_id ==9, 1, 0)
#tpdat_all$dummy_deep <- ifelse(tpdat_all$month_id ==10, 1.2, tpdat_all$dummy_deep)
# tpdat_all$dummy_deep <- ifelse(tpdat_all$month_id ==10, 1, 0)

tpdat_all <- tpdat_all[, c(1:Sales_Norm_Position, grep("^sales_others", colnames(tpdat_all)), grep("^dummy", colnames(tpdat_all)), grep("^adstock", colnames(tpdat_all)))]
### End of customization



#append the hp_cd to sales_hp_avg
sales_hp_avg <- hp_cd_set %>% inner_join(sales_hp_avg, by = "facility_id") 
sales_hp_avg <- sales_hp_avg %>% dplyr::select(-starts_with("facility"))


print(n_distinct(tpdat_all$hp_cd))     # 1068
#print(n_distinct(sales_hp_avg$facility_id))  # 15200
print(n_distinct(sales_hp_avg$hp_cd))  #1068

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


# A solver with tpdat and b_prior as input, and output including par estimation, fitted y and lift
score_channel2 <- function(tpdat, b_prior){
  
  #x1 = tpdat[, -c(1,3)]   # columns other than  hcp_cd, nrx_wk, nrx_norm
  #x1 = as.matrix(cbind(1, x1[,1], -log(1+x1[,2]), log(1+x1[,-(1:2)]))) # add intercept "1", and set competitors NRx as negative
  
  x1 = tpdat[, -c(1, Sales_Norm_Position)]  # drop facility_id and sales_norm
  
  x1 = as.matrix(cbind(1, x1[,"month_id"], -log(1+x1[,"sales_others"]), x1[,"dummy_peak"], log(1+x1[,-c(1:(N_NonePromo - 1))])))  #note-- control variables need to set and updated if necessary
  # log(100000 + x1[,"month_id"]) can achieve RSquare = 0.832109 for aggregated prediction
  
  y1 = log(1+tpdat[, Sales_Norm_Position])  # Note: log of (1 + sales_norm)
  var1 = var(y1)
  lambda1_init = 100
  lambda1_lower = lambda1_init*0.001  #modified
  # lambda1_lower = 0
  lambda1_upper = lambda1_init*10
  
  mth1 = tpdat$month_id
  wgt1 = time_wgt(mth1)  # more rencent data weight more in model
  
  r1 = nnridge(X=x1,y=y1,
               b_prior=b_prior,
               b_lambda_init=c(lambda1_init, b_prior),
               b_lambda_lower=c(lambda1_lower, rep(0, length(b_prior))),
               # b_lambda_lower=c(lambda1_lower, 0, -Inf, rep(0, length(b_prior)-2)),   #this will cause negative base for some facilities
               # b_lambda_lower=c(lambda1_lower, -Inf, rep(0, length(b_prior)-1)),
               b_lambda_upper=c(lambda1_upper, rep(Inf, length(b_prior))),
               weight=wgt1)
  
  #print(paste("lambda=", r1$lambda, sep=""))
  
  # Yan added --
  
  cat("initial lambda: ", lambda1_init, "\n")
  cat("lambda lower bound: ", lambda1_lower, "\n")
  cat("lambda upper bound: ", lambda1_upper, "\n")
  cat("lambda selected: ", r1$lambda, "\n")
  
  # End of the addition.
  
  # b1 = ifelse(r1$b>=0, r1$b, 0) #-- the original setup
  b1 = r1$b  #this is for -Inf time trend/intercept
  b1[-c(1:N_NonePromo)] <- ifelse(b1[-c(1:N_NonePromo)]>=0, b1[-c(1:N_NonePromo)], 0) # (1:N_NonePromo) are for intercept, time trend and other control variables; make sure coefficients on promo variables are positive
  
  
  b_int = b1[1]
  b_time = b1[2]
  # b_comp = b1[3] # Need to updated accordingly for other control variables
  b_sales_others = b1[3] # Need to updated accordingly for other control variables
  b_dummy_peak = b1[4] # Need to updated accordingly for other control variables
  
  # b_self = b1[-(1:3)]
  b_self = b1[-c(1:N_NonePromo)] # for promo channels
  
  ypred = x1%*%b1
  yhat = exp(ypred)-1 # predicted sales
  yobs = exp(y1)-1 # Observed sales
  
  rsqr = 1- sum( wgt1*(yobs - yhat)^2 )/sum( wgt1*(yobs - mean(yobs))^2 ) # R-square
  
  # Sales(>0) contribution from each channel
  lift = rep(0, length(b1))
  ii_pos = which(yobs>0)  # Note: this is the root cause for some promo channels for which the contribution is zero. If the promo channel is always zero when sales is non-zero, then the contribution will be zero.
  if(length(ii_pos)>0) {
    for (j in 1:length(b1)){
      btmp = b1
      btmp[j] <- 0 # what if the j-th channel promo is removed
      ypredtmp = x1[ii_pos,]%*%btmp
      yhattmp= exp(ypredtmp)-1 # predicted NRx with j-th channel promo removed
      lift[j] = sum(yhat[ii_pos] - yhattmp) # lift is calculated as the differences of NRx w/ or w/o j-th promo
    }
    if(sum(lift)>0) {lift = lift*sum(yobs[ii_pos])/sum(lift)} else {lift = c(sum(yobs[ii_pos]), rep(0, length(b1)-1))} # attribute NRx to baseline and promo, if no lift from promo then attribute all NRx to baseline
  }
  
  # v = list(b_int=b_int, b_time=b_time, b_comp=b_comp, b_self=b_self, yobs=yobs, yhat=yhat, rsqr=rsqr, lift=lift)
  # Note: the returned list needs to be updated according to market events that are added.
  v = list(b_int=b_int, b_time=b_time, b_sales_others = b_sales_others, b_dummy_peak = b_dummy_peak, b_self=b_self, yobs=yobs, yhat=yhat, rsqr=rsqr, lift=lift)
  return(v)
  
}

# To solve parameters and score for each HP

v_rsqr_hp = rep(0, n_hp) # r-square (model fitness) for each HP
yraw_hp = c() # raw NRx of each HP
yhat_hp = c() # predicted NRx of each HP
v_mth = c() # month id vector
par_est_hp = list() # save outputs of each HP from function score_channel2()

#yan added
cores<- 4
library(doParallel)
Sys.time()->start

cl <- makeCluster(cores)
registerDoParallel(cl, cores = cores)

test_out <- foreach(i = 1:n_hp) %dopar% {
# test_out <- foreach(i = 1:1000) %dopar% {
  hp_cd_ii <- hp_cd_set$hp_cd[i] # i-th HP id
  
  tpdat_ii <- tpdat_all[which(tpdat_all$hp_cd==hp_cd_ii),] # NRx+Promo
  
  sales_avg_ii <- sales_hp_avg[sales_hp_avg$hp_cd == hp_cd_ii, ]$sales_avg[1]  # average sales for the facility
  
  sales_total_ii <- sum(tpdat_ii$sales_norm)*sales_avg_ii  # sales total over the time
  
  area_cd_ii <- ref_hp[ref_hp$hp_cd == hp_cd_ii,]$area_id[1]
  
  # set facility beta prior as from area model.
  if(!is.na(area_cd_ii)){
    # note: as.numeric is to convert dataframe to a numeric vector
    b_prior_hp <- c(rep(0, N_NonePromo), as.numeric(b_array_area_est[b_array_area_est$area_id ==  area_cd_ii , -c(1:(1+N_NonePromo))])) # set prior for intercept and non-promo variables back to 0, and also remove area_id from prior parameter table.
    
  } else{
    
    b_prior_hp <- c(rep(0, N_NonePromo), as.numeric(b_area_est_avg[-c(1:N_NonePromo)]))  # set prior for intercept and non-promo variables back to 0
    
  }
  
  if(nrow(tpdat_ii)>1){  # those facilities with only one record can not be estimated
    
    rr <- score_channel2(tpdat_ii, b_prior = b_prior_hp)
    
    contr_ii <- rr$lift*sales_avg_ii
    
    #upate the market event accordingly
    b_array_hp <- c(hp_cd_ii, sales_total_ii, rr$b_int, rr$b_time, rr$b_sales_others, rr$b_dummy_peak, rr$b_self, contr_ii) # array with each row for one HP, columns with beta est and sales contribution of each promo channel
    v_rsqr_hp <- rr$rsqr                # r-square (goodness of fit)    
    
    yraw_hp <- rr$yobs*sales_avg_ii
    yhat_hp <- rr$yhat*sales_avg_ii
    
    v_mth <- tpdat_ii$month_id
    
  } else{
    rr <- list() #empty
    b_array_hp <- c(hp_cd_ii, sales_total_ii, rep(0, length(b_prior_hp)), c(sales_total_ii, rep(0, length(b_prior_hp)-1))) # assign sales contribution to baseline for those with only one record
    v_rsqr_hp <- NA
    
    yraw_hp <- numeric() #empty
    yhat_hp <- numeric() #empty
    v_mth <- numeric() #empty
    }
  
  all_results <- list("hp_cd" = hp_cd_ii, "par_est_hp" = rr, "b_array_hp" = b_array_hp, "hp_rsqr" = v_rsqr_hp, "v_mth" = v_mth, "yraw_hp" = yraw_hp, "yhat_hp" = yhat_hp)
  
  return(all_results)
  
}

stopCluster(cl)

print(Sys.time()-start)

time_stamp <- format(Sys.time(), "%F %H-%M") #vary every minute
data_path <- paste0('./HP_Results/HP_Model_', time_stamp, "/")

if (file.exists(data_path) == FALSE) {
  dir.create(data_path, showWarnings = TRUE, recursive = TRUE, mode = "0777")
}


save(test_out, file=paste(data_path, "hp_foreach_out.RData", sep=""))

#### extract outcomes
# n_hp <- 1000
par_est_hp <- lapply(1:n_hp, function(i){return(test_out[[i]][["par_est_hp"]])})

b_array_hp_lst <- lapply(1:n_hp, function(i){return(test_out[[i]][["b_array_hp"]])})
b_array_hp <- do.call(rbind, b_array_hp_lst) %>% data.frame()    
# name the columns-- need to update for your product
colnames(b_array_hp) <- c('hp_cd',  'sales_total', 'b_int', 'b_time', 'b_sales_others', 'b_dummy_peak', 
                      paste('b_self', c(1:N_Channel), sep="_"), "contr_int", "contr_time", 'contr_sales_others', "contr_dummy_peak", paste('contr_self', c(1:N_Channel), sep="_"))
row.names(b_array_hp) <- NULL


v_mth_lst <- lapply(1:n_hp, function(i){return(test_out[[i]][["v_mth"]])})
v_mth<- do.call(c, v_mth_lst)

yraw_hp_lst <-  lapply(1:n_hp, function(i){return(test_out[[i]][["yraw_hp"]])})
yraw_hp<- do.call(c, yraw_hp_lst)

yhat_hp_lst <- lapply(1:n_hp, function(i){return(test_out[[i]][["yhat_hp"]])})
yhat_hp <- do.call(c, yhat_hp_lst)

#QC
QC1<- lapply(1:n_hp, function(i){return(length(test_out[[i]][["yraw_hp"]]))}) %>% do.call(c, .)

# (1) Parameter estimation and NRx contribution for each HCP
save(par_est_hp, file=paste(data_path, "par_est_hp.RData", sep=""))
save(b_array_hp, file=paste(data_path, "b_array_hp.RData", sep=""))
write.csv(b_array_hp, paste(data_path, "b_array_hp.csv", sep=""), row.names=FALSE)

# (2) Observed and predicted (unadjusted) NRx for each HCP over time
y_obs_hat_hp <- data.frame(mth=v_mth, yraw=yraw_hp, yhat=yhat_hp)
save(y_obs_hat_hp, file=paste(data_path, "y_obs_hat_hp.RData", sep=""))

# facility and month level RSquare
cat("Facility and month level RSquare -- \n")
print(1- sum((y_obs_hat_hp$yraw - y_obs_hat_hp$yhat)^2)/sum((y_obs_hat_hp$yraw - mean(y_obs_hat_hp$yraw))^2))

# (3) Adjuted ratio and R-square (model fitness)
y_obs_hat_hp[y_obs_hat_hp<0] <- 0
cat("Facility and month level RSquare after adjusting negative prediction to 0 -- \n")
print(1- sum((y_obs_hat_hp$yraw - y_obs_hat_hp$yhat)^2)/sum((y_obs_hat_hp$yraw - mean(y_obs_hat_hp$yraw))^2))

y_obs_hat_overall <- sqldf("select mth, sum(yraw) as yraw, sum(yhat) as yhat from y_obs_hat_hp group by mth order by mth")
radj <- median(y_obs_hat_overall$yraw/y_obs_hat_overall$yhat)
#radj = 
save(radj, file=paste(data_path, "radj.RData", sep=""))

#before adjustment--yan added
1-sum((y_obs_hat_overall$yraw-y_obs_hat_overall$yhat)^2)/sum((y_obs_hat_overall$yraw-mean(y_obs_hat_overall$yraw))^2)
#after adjustment
y_obs_hat_overall$yhat_adj <- y_obs_hat_overall$yhat*radj
rsqr_overall_hp <- 1-sum((y_obs_hat_overall$yraw-y_obs_hat_overall$yhat_adj)^2)/sum((y_obs_hat_overall$yraw-mean(y_obs_hat_overall$yraw))^2)
cat("Month level RSquare after adjustment. \n")
print(rsqr_overall_hp)
write.csv(rsqr_overall_hp, paste(data_path, "rsqr_overall_hp.csv", sep=""), row.names=FALSE)

write.csv(y_obs_hat_overall, paste(data_path, "y_obs_hat_overall.csv", sep=""), row.names=FALSE)

# Qin start here
hp_cd_lst <- lapply(1:n_hp, function(i){return(test_out[[i]][["hp_cd"]])})
hp_cd <- do.call(c, hp_cd_lst)

R_hp_rsqr_lst <- lapply(1:n_hp, function(i){return(test_out[[i]][["hp_rsqr"]])})
R_hp_rsqr <- do.call(c, R_hp_rsqr_lst)
rsqr_out <- data.frame(hp_cd=hp_cd, rsqr=R_hp_rsqr)
write.csv(rsqr_out, paste(data_path, "rsqr_by_hp.csv", sep=""), row.names=FALSE)

#Qin end here
# (4) Observed and predicted (adjusted) NRx for each HCP over time, and plot curves of observed and predicted total NRx over time

# plot prediction vs. observation of total sales

y_min <- min(y_obs_hat_overall$yraw, y_obs_hat_overall$yhat_adj)
y_max <- max(y_obs_hat_overall$yraw, y_obs_hat_overall$yhat_adj)

tiff(filename=paste(data_path, "hp_overall_fit.tiff", sep=""), res=600, compression = "lzw", height=5, width=5, units="in")
plot(y_obs_hat_overall$mth, y_obs_hat_overall$yraw, lty=1, type='o', pch = 15, col='grey50', lwd=2.5, xlab='Month', ylab='Total Sales', ylim=c(y_min, y_max), main='Total Sales over time')
lines(y_obs_hat_overall$mth, y_obs_hat_overall$yhat_adj, lty=1, lwd=2.5, type='o', pch = 17, col='green')
legend(x="bottomright",legend=c('Observed', 'Predicted'), lty=c(1,1), lwd=c(2.5,2.5), pch=c(15, 17), col=c('grey50','green'))
dev.off()

#End


## QC
load("./Rx_Promo_Data_for_Model_Nov07.RData")

Sales_MCM_hp <- sales_mcm_DM
Sales_MCM_hp <- Sales_MCM_hp %>% filter(sales >= 0)  # note: negative sales data points have been removed before.

Sales_MCM_hp_merged <- hp_cd_set %>% inner_join(Sales_MCM_hp, by = c("facility_id" = "facility_id"))

#calcualte sum of sales for each facility--
Sales_for_each_hp <- Sales_MCM_hp_merged %>% group_by(hp_cd, area_id) %>% summarise(original_sales = sum(sales)) %>% ungroup()
head(Sales_for_each_hp)
tail(Sales_for_each_hp)

#tpdat_all %>% group_by(hp_cd) %>% summarise()
adstock_promo_sum <- aggregate(tpdat_all[ , 3:ncol(tpdat_all)], list(hp_cd = tpdat_all$hp_cd), FUN=sum)

join1 <- Sales_for_each_hp %>% inner_join(adstock_promo_sum, by = c("hp_cd"="hp_cd"))

join2 <- b_array_hp %>% inner_join(join1, by = c("hp_cd"="hp_cd"))

write.csv(join2, paste(data_path, "QC_output_hp_database.csv", sep=""), row.names=FALSE)
  
