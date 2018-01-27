

####
####  This is to prepare input data for area level model -- DM
####
####

rm(list=ls())

library(lubridate)
library(ggplot2)
library(scales)  # this for scale date on x axis
library(tidyr)
library(reshape2)
library(sqldf)
library(dplyr)


setwd("D:/Project/2017/Japan MCMC/Japan_MCM_POC_CD/03_Output/")

load("Rx_Promo_Data_for_Model_Nov07.RData")

################
###  Sales data: use Sales_MCM_area
###  Promo data: use Promo_MCM2_area
################

## sales-
sales_avg_area <- Sales_MCM_area_DM %>% group_by(area_id) %>% summarise(sale_mean = mean(sales)) %>% ungroup()

sales_avg_area_others <- Sales_MCM_area_DM %>% group_by(area_id) %>% summarise(sale_mean_others = mean(sales_others)) %>% ungroup()

Sales_MCM_area_others  <-Sales_MCM_area_DM %>% left_join(sales_avg_area_others, by = c("area_id")) %>% mutate(sales_others = sales_others/sale_mean_others) %>% select(- sale_mean_others)

Sales_MCM_area_norm  <-Sales_MCM_area_others %>% left_join(sales_avg_area, by = c("area_id")) %>% mutate(sales_norm = sales/sale_mean) %>% select(-sales, - sale_mean)
## promo-
head(Promo_MCM2_area_DM)


## merge data

Sales_Promo_area <- sqldf("select a.area_id, a.month_id, a.sales_norm, a.sales_others,
                          b.channel_id, b.month_id as promo_month_id, a.month_id - b.month_id as gap_sales_promo, b.pde
                          from Sales_MCM_area_norm a left join Promo_MCM2_area_DM b 
                          on a.area_id = b.area_id and a.month_id >= b.month_id
                          ") 

# Sales_Promo_area[1:20,]
sum(is.na(Sales_Promo_area$channel_id))  # 0
sum(is.na(Sales_Promo_area$pde)) # 0

#write.csv(Sales_Promo_area, "004A_Sales_Promo_area.csv", row.names = FALSE)  #exported for QC.

save(  sales_avg_area
       , Sales_MCM_area_norm
       , Sales_Promo_area
       , file = "004A_Sales_Promo_DM.RData"
)

