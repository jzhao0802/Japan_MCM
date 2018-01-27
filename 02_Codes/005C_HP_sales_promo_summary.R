


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

### Load HP sales data
load("./Rx_Promo_Data_for_Model_Nov07.RData")

Sales_MCM_hp <- sales_mcm_DM
print(table(Sales_MCM_hp$month_id))
print(n_distinct(Sales_MCM_hp$facility_id))

sales_hp_filtered <- Sales_MCM_hp %>% filter(sales >= 0)  # Option 1: only remove months with negative sales.
print(table(sales_hp_filtered$month_id))  # from now, some monthly records for some HPs have been removed.
print(n_distinct(sales_hp_filtered$facility_id))
print(sum(sales_hp_filtered$sales<0))

# average sales per HP
sales_hp_avg <- sales_hp_filtered %>% group_by(facility_id) %>% summarise(sales_avg = mean(sales), sales_others_avg=mean(sales_others)) %>% ungroup()
sum(sales_hp_avg$sales_avg <=0)   #-- check if any non-postive average sales
sum(sales_hp_avg$sales_avg <0)
sum(sales_hp_avg$sales_avg == 0)  #-- 341 facilities with zero sales

# decide not to exclude these 341 facilities
# sales_hp_filtered2 <- sqldf("select * from sales_hp_filtered where facility_id in (select facility_id from sales_hp_avg where sales_avg >0 )") # remove HP for which the sales are totally zero
# print(n_distinct(sales_hp_filtered2$facility_id)) 

sales_hp_summary_wide <- sales_hp_filtered %>% group_by(facility_id) %>% summarise(sales = sum(sales), sales_others=sum(sales_others)) %>% ungroup()
head(sales_hp_summary_wide)
n_distinct(sales_hp_summary_wide$facility_id)

save(sales_hp_summary_wide, file = "sales_hp_summary_wide.RData")
write.csv(sales_hp_summary_wide, "sales_hp_summary_wide.csv", row.names = FALSE)


### Load in hp promo data--
promo_hp <- promo_mcm_DM2
head(promo_hp)
dim(promo_hp)
# sum up promo unit count
promo_hp_summed <- promo_hp %>% group_by(facility_id, channel_id) %>% summarise(vst_ct = sum(pde)) %>% ungroup()
# convert to wide table
promo_hp_sum_wide <- spread(promo_hp_summed, channel_id, vst_ct)
colnames(promo_hp_sum_wide) <- c("facility_id", paste0("vst_ct_", 1:N_Channel))
print(head(promo_hp_sum_wide))

promo_vars_position <- grep("^vst_ct_", colnames(promo_hp_sum_wide))

temp_promo_fillna<- apply(promo_hp_sum_wide[, promo_vars_position], 2, 
                                        function(x){
                                        x <- ifelse(is.na(x),0,x)
                                        return(x)})

promo_hp_wide <- cbind(promo_hp_sum_wide[,-promo_vars_position], temp_promo_fillna)
is.factor(promo_hp_wide$facility_id)
n_distinct(promo_hp_wide$facility_id)

save(promo_hp_wide, file = "promo_hp_summary_wide.RData")
write.csv(promo_hp_wide, "promo_hp_summary_wide.csv", row.names = FALSE)


# merge sales and promo data
sales_promo_hp_summary_wide<- sales_hp_summary_wide %>% inner_join(promo_hp_wide, by = c("facility_id" = "facility_id"))
print(head(sales_promo_hp_summary_wide))
sum(sales_promo_hp_summary_wide$sales == 0) # 40

save(sales_promo_hp_summary_wide, file= "sales_promo_hp_summary_wide.RData")
write.csv(sales_promo_hp_summary_wide, "sales_promo_hp_summary_wide.csv", row.names = FALSE)

### End

