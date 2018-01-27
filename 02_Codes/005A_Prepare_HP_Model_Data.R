


### Important Note: HP is only a token, which can mean either a facility or a healthcare professional.

### Note: HP with zero sales have been removed. they are not included in the HP level model.  Aug 18, 2017

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
N_Channel <- 6
# Load decay parameter 
load("./DMA_Results/par_decay_DM.RData")
colnames(par_decay)[1] <- "channel_id"  # make sure the name is consistent with other datasets

# load HP sales data
load("./Rx_Promo_Data_for_Model_Nov07.RData")

Sales_MCM_hp <- sales_mcm_DM
print(table(Sales_MCM_hp$month_id))
print(n_distinct(Sales_MCM_hp$facility_id))

sales_hp_filtered <- Sales_MCM_hp %>% filter(sales >= 0)  # Option 1: only remove months with negative sales.
print(table(sales_hp_filtered$month_id))  # from now, some monthly records for some HPs have been removed.

# average sales per HP
sales_hp_avg <- sales_hp_filtered %>% group_by(facility_id) %>% summarise(sales_avg = mean(sales)) %>% ungroup()
sales_hp_avg_others <- sales_hp_filtered %>% group_by(facility_id) %>% summarise(sales_avg_others = mean(sales_others)) %>% ungroup()
sum(sales_hp_avg$sales_avg <=0)   # 40 -- check if any non-postive average sales
sum(sales_hp_avg$sales_avg <0)
sum(sales_hp_avg$sales_avg == 0)  #-- 341 facilities with zero sales

 # !!! remove the facilities with zero sales for all months
sales_hp_avg <- sales_hp_avg %>% filter(sales_avg>0)
sales_hp_avg_others <- sales_hp_avg_others %>% filter(sales_avg_others>0)  
print(n_distinct(sales_hp_avg$facility_id))   # 1068


# normalize sales by HP
sales_hp_norm <- sales_hp_filtered %>% 
                  inner_join(sales_hp_avg, by = c("facility_id" = "facility_id")) %>% 
                  left_join(sales_hp_avg_others, by = c("facility_id" = "facility_id")) %>%
                  mutate(sales_norm = sales/sales_avg, sales_others = sales_others/sales_avg_others) %>% arrange(facility_id, month_id)

print(n_distinct(sales_hp_norm$facility_id))
print(sum(is.nan(sales_hp_norm$sales_norm)))
print(sum(is.na(sales_hp_norm$sales_norm)))
print(sum(is.nan(sales_hp_norm$sales_others)))
print(sum(is.na(sales_hp_norm$sales_others)))
# promo data
promo_hp <- promo_mcm_DM2
head(promo_hp)
dim(promo_hp)

print(n_distinct(promo_hp$facility_id))

# merge sales and promo data
hp_sales_promo <- sqldf("select a.facility_id, a.month_id, a.month_id - b.month_id as gap_sales_promo, b.channel_id, b.pde
                        from sales_hp_norm a left join promo_hp b
                        on a.facility_id = b.facility_id 
                        and a.month_id >= b.month_id 
                        order by a.facility_id, a.month_id, b.channel_id
                        ")
print(head(hp_sales_promo))
print(tail(hp_sales_promo))

sum(is.na(hp_sales_promo$pde))
sum(is.na(hp_sales_promo$channel_id))
sum(is.na(hp_sales_promo$gap_sales_promo))
print(sum(hp_sales_promo$gap_sales_promo<0))

dim(hp_sales_promo)

#for validation -- in this case, the number of records are same. Total number of records is 6324958.
hp_sales_promo2 <- sqldf("select a.facility_id, a.month_id, a.month_id - b.month_id as gap_sales_promo, b.channel_id, b.pde
                        from sales_hp_norm a left join promo_hp b
                        on a.facility_id = b.facility_id 
                        where a.month_id >= b.month_id 
                        order by a.facility_id, a.month_id, b.channel_id
                        ")
dim(hp_sales_promo2)

# apply decay parameters
hp_ad_tmp1 <- sqldf("select a.*, a.pde * power(b.lam_est , a.gap_sales_promo) as ad1
                     from hp_sales_promo a inner join par_decay b
                      on a.channel_id = b.channel_id
                    ")
dim(hp_ad_tmp1)  #same dimension, no record dropped due to inner join
# calculate adstock
hp_ad_tmp2 <- sqldf("select facility_id, month_id, channel_id, sum(ad1) as ad1
                     from  hp_ad_tmp1
                     group by facility_id, month_id, channel_id
                     order by facility_id, month_id, channel_id
                    ")
dim(hp_ad_tmp2)
sum(hp_ad_tmp2$ad1 == 0)
sum(is.na(hp_ad_tmp2$ad1))
sum(hp_ad_tmp2$ad1 < 0 )

# average adstock
# hp_adstock_avg <- hp_ad_tmp2 %>% group_by(facility_id, channel_id) %>% summarise(adstock_avg = mean(ad1)) %>% ungroup()
# sum(is.na(hp_adstock_avg$adstock_avg))
# sum(hp_adstock_avg$adstock_avg == 0)  # 18029 records
# sum(hp_adstock_avg$adstock_avg < 0)

hp_adstock_avg <- sqldf("select facility_id, channel_id, avg(ad1) as adstock_avg 
                        from hp_ad_tmp2
                        where ad1 > 0 
                        group by facility_id, channel_id
                        order by facility_id, channel_id
                        ")  #Note: the condition ad1>0 is inhereted from USA MCM methodology-- see 7a sas code.
sum(hp_adstock_avg$adstock_avg == 0)  # 0 -- the 0 explains why removing records with ad1 = 0.
sum(hp_adstock_avg$adstock_avg < 0)  #0 
sum(is.na(hp_adstock_avg$adstock_avg))  #0

print(n_distinct(hp_adstock_avg$facility_id))  # 1068-- that means some facilities were lost in the above process compared to 15200 from hp_ad_tmp2 data.


# normalize adstock
hp_ad_tmp3 <- sqldf("select a.facility_id, a.month_id, a.channel_id, a.ad1/b.adstock_avg as adstock_norm
                      from hp_ad_tmp2 a inner join hp_adstock_avg b
                      on a.facility_id = b.facility_id and a.channel_id = b.channel_id
                      ")

#convert long table to wide 
hp_ad_tmp4 <- spread(hp_ad_tmp3, channel_id, adstock_norm)
head(hp_ad_tmp4)
tail(hp_ad_tmp4)
#replace numeric column names
colnames(hp_ad_tmp4)[grep("^\\d", colnames(hp_ad_tmp4))] <- paste0("adstock_norm_", c(1:N_Channel))
n_distinct(hp_ad_tmp4$facility_id)  # 15079
## append adstock to sales


hp_sales_promo_wide_tmp1 <- sales_hp_norm %>% left_join(hp_ad_tmp4, by = c("facility_id" = "facility_id", "month_id"="month_id"))
print(n_distinct(hp_sales_promo_wide_tmp1$facility_id))

hp_sales_promo_wide <- hp_sales_promo_wide_tmp1 %>% select(facility_id, month_id, sales_norm, sales_others, area_id, starts_with("adstock_norm"))
sum(is.na(hp_sales_promo_wide$sales_norm))

#repalce missing with 0s
adstock_position <- grep("^adstock_norm", colnames(hp_sales_promo_wide))
temp_adstock <-apply(hp_sales_promo_wide[, adstock_position], 2, function(x){
                                                                              x <- ifelse(is.na(x),0,x)
                                                                              return(x) }) %>% data.frame()

hp_sales_promo_wide <- cbind(hp_sales_promo_wide[, -adstock_position],temp_adstock)

#write to csv
write.csv(hp_sales_promo_wide, "hp_sales_promo_wide.csv", row.names = FALSE)
write.csv(sales_hp_avg, "sales_hp_avg.csv", row.names = FALSE)
write.csv(sales_hp_norm, "sales_hp_norm.csv",  row.names = FALSE)
write.csv(hp_adstock_avg, "hp_adstock_avg.csv", row.names = FALSE)

#write to R data
save(hp_sales_promo_wide, file = "hp_sales_promo_wide.RData")
save(sales_hp_avg, file = "sales_hp_avg.RData")
save(sales_hp_norm, file = "sales_hp_norm.RData")
save(hp_adstock_avg, file = "hp_adstock_avg.RData")




