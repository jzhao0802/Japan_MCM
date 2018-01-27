


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

#########################################################################
### Load Data
#########################################################################

# Promo and sales data 
# 1. sales_avg_area
# 2. Sales_MCM_area_norm
# 3. Sales_Promo_area
load("004A_Sales_Promo_DM.RData")

#decay parameters from 004B code
load("./DMA_Results/par_decay_DM.RData")

lam <- par_decay$lam_est


#########################################################################
### NRx and Promotion data (DMA level)
#########################################################################

area_ad_tmp1 <- Sales_Promo_area
area_ad_tmp1$ad1 <- area_ad_tmp1$pde * (lam[area_ad_tmp1$channel_id]^area_ad_tmp1$gap_sales_promo)
area_ad_tmp2 <- aggregate(area_ad_tmp1$ad1, list(area_ad_tmp1$area_id, area_ad_tmp1$month_id,  area_ad_tmp1$channel_id), sum)
names(area_ad_tmp2) <- c("area_id", "month_id", "channel_id", "ad1")

## Area Adstock average;
# area_adstock_avg = aggregate(area_ad_tmp2$ad1, list(area_ad_tmp2$area_id , area_ad_tmp2$channel_id), function(x){mean(x[x>0])}) # Yan's comments: this is Dong's way of normalizing, it ignores zero cells
area_adstock_avg <- aggregate(area_ad_tmp2$ad1, list(area_ad_tmp2$area_id , area_ad_tmp2$channel_id), mean)
names(area_adstock_avg) <- c("area_id", "channel_id", "adstock_avg")
sum(area_adstock_avg$mean == 0) %>% print() #see how many 0 cells on the average data

save(area_adstock_avg, file = "./DMA_Results/area_adstock_avg_DM.RData")

# Normalize adstock-- Yan comments: the normalization is on area and channel level, i.e. for each area individually.
area_ad_tmp3 <- sqldf("
                      select a.area_id, a.month_id, a.channel_id, a.ad1/b.adstock_avg as adstock_norm
                      from area_ad_tmp2 a inner join area_adstock_avg b
                      on a.area_id = b.area_id
                      and a.channel_id = b.channel_id")


# Convert long table to wide.
area_ad_tmp4 = reshape(area_ad_tmp3, v.names= 'adstock_norm', idvar = c("area_id", "month_id"), timevar = "channel_id", direction = "wide", sep = "_")

#print(area_ad_tmp4[area_ad_tmp4$area_id==31,]) #This area does have 1 channel not available

apply(area_ad_tmp4, 2, function(col){sum(is.na(col))}) #check if any area contain NA values

# Replace NA with 0
area_ad_tmp4b <- apply(area_ad_tmp4, 2, function(col){
  
                                      if(sum(is.na(col))>0){
                                        
                                        new_col <- ifelse(is.na(col), 0, col)
                                        
                                      } else{
                                        
                                        new_col <- col
                                        
                                      }
  
                                      return(new_col)
  
                                      }) %>%
                data.frame()
print(area_ad_tmp4b[area_ad_tmp4b$area_id==31,])
apply(area_ad_tmp4b, 2, function(col){sum(is.na(col))}) 

# arrange variable order
area_ad_tmp5 <- sqldf("
                      select area_id 
                      , month_id
                      , adstock_norm_1 
                      , adstock_norm_2 
                      , adstock_norm_3 
                      , adstock_norm_4 
                      , adstock_norm_5 
                      , adstock_norm_6
                      
                      from area_ad_tmp4b
                      order by area_id, month_id
                      ")

# append adstock to sales data
area_sales_promo_wide_tmp1 = sqldf("
                                    select a.*
                                  , b.adstock_norm_1
                                  , b.adstock_norm_2
                                  , b.adstock_norm_3
                                  , b.adstock_norm_4
                                  , b.adstock_norm_5
                                  , b.adstock_norm_6
                                  
                                    from Sales_MCM_area_norm a left join area_ad_tmp5 b
                                    on a.area_id = b.area_id and a.month_id = b.month_id
                                   ")

apply(area_sales_promo_wide_tmp1, 2, function(col){sum(is.na(col))})  #check if there is any missing value on the data

### Customized: add addtional control variables--
area_sales_promo_wide_tmp1$dummy_peak <- ifelse(area_sales_promo_wide_tmp1$month_id %in% c(6,13), 1, 0)
area_sales_promo_wide_tmp1$dummy_peak <- ifelse(area_sales_promo_wide_tmp1$month_id %in% c(19), 2, area_sales_promo_wide_tmp1$dummy_peak)
#area_sales_promo_wide_tmp1$dummy_deep <- ifelse(area_sales_promo_wide_tmp1$month_id %in% c(9, 10), 1, 0)
### end of this customization

first_cols <- grep("^adstock", colnames(area_sales_promo_wide_tmp1)) %>% 
                colnames(area_sales_promo_wide_tmp1)[.] %>%
                setdiff(colnames(area_sales_promo_wide_tmp1), .)

last_cols <- grep("^adstock", colnames(area_sales_promo_wide_tmp1)) %>% 
                colnames(area_sales_promo_wide_tmp1)[.]


area_sales_promo_wide <- area_sales_promo_wide_tmp1[,c(first_cols, last_cols)]

save(area_sales_promo_wide, file = "./DMA_Results/area_sales_promo_wide_DM.RData")


