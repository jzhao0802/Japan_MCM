

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

hp_final_model_folder <- "HP_Model_2017-11-21 14-41"

#########################################################################
### Load data
#########################################################################

# Load HCP parameter estimation and NRx attribution
load(paste0("./HP_Results/", hp_final_model_folder, "/b_array_hp.RData"))

# Load channel reference table
load("ref_channel.RData")

# HP reference table -- only for HPs that are put in models
hp_cd_set_for_hp_model <- read.csv("hp_cd_set_for_hp_model.csv", stringsAsFactors = FALSE)

# Adjust ratio
load(paste0("./HP_Results/", hp_final_model_folder, "/radj.RData"))

# HP sales and promo summary data
load("sales_promo_hp_summary_wide.RData")

hp_sales_promo_sum <- hp_cd_set_for_hp_model %>% 
                      inner_join(sales_promo_hp_summary_wide, by = c("facility_id" = "facility_id"))

Position_Sales_SUM <- as.numeric(grep("^sales$", colnames(hp_sales_promo_sum)))

#########################################################################
### (1) HCP level contribution
#########################################################################

attr_hp_temp1 <- hp_sales_promo_sum %>% 
                  inner_join(b_array_hp, by = c("hp_cd" = "hp_cd")) %>% 
                   select(-starts_with("b_"))
dim(attr_hp_temp1)

with(attr_hp_temp1, table(sales- sales_total)) # equal

attr_hp_temp2 <- cbind(attr_hp_temp1[, grep("^hp_|^sales$|^vst_ct_", colnames(attr_hp_temp1))]
                       , with(attr_hp_temp1, 
                              data.frame(contr_baseline = contr_int + contr_time + contr_sales_others + contr_dummy_peak))
                       , attr_hp_temp1[,grep("^contr_self_", colnames(attr_hp_temp1))]
                       )

# write.csv(attr_hp_temp2, paste0("./HP_Results/", hp_final_model_folder, "/attr_hp_temp2_QC.csv"), row.names = FALSE)

# Rename 
colnames(attr_hp_temp2)[grep("^vst_ct_", colnames(attr_hp_temp2))] <- paste0("vst_ct (", ref_channel$channel[1:N_Channel],")")

colnames(attr_hp_temp2)[grep("^contr_", colnames(attr_hp_temp2))] <- paste0("attribution (", c("Baseline", ref_channel$channel[1:N_Channel]), ")")

head(attr_hp_temp2)  

POS_sales <- grep("^sales$", colnames(attr_hp_temp2))
POS_baseline<- grep("Baseline", colnames(attr_hp_temp2))
POS_baseline_and_promo <- grep("^attribution", colnames(attr_hp_temp2))
POS_promo <- setdiff(POS_baseline_and_promo, POS_baseline)

attr_adj_bas <- function(v_x){
  
  if(v_x[POS_sales]>0){
    
    v_x[POS_baseline] <- v_x[POS_baseline] + log(radj)
    
    if(v_x[POS_baseline]>=0){  # when baseline contribution is non-negative
      
      v_x[POS_baseline_and_promo] <- v_x[POS_baseline_and_promo]*v_x[POS_sales]/sum(v_x[POS_baseline_and_promo])  # re-assign attribution to promo channels proportionally
      
    } else{   # when baseline<0
    
      v_x[POS_baseline] <- 0  # set negative baseline attr as 0
      
      v_x[POS_promo] <- v_x[POS_promo]*v_x[POS_sales]/sum(v_x[POS_promo]) # re-assign attribution to promo channels proportionally
      
    }
    
  }
  
  return(v_x)
  
}

attr_hp_temp3 <- t(apply(attr_hp_temp2, 1, attr_adj_bas))
attr_hp_temp3 <- data.frame(attr_hp_temp3)
colnames(attr_hp_temp3) <- colnames(attr_hp_temp2)

attr_hp_temp3 <- hp_cd_set_for_hp_model %>% inner_join(attr_hp_temp3, by = c("hp_cd"="hp_cd"))
head(attr_hp_temp3)
dim(attr_hp_temp3)

# append those facilities with zero sales
hp_with_zero_sales <- sales_promo_hp_summary_wide %>% filter(!(facility_id %in% attr_hp_temp3$facility_id)) %>% arrange(facility_id)
dim(hp_with_zero_sales)
head(hp_with_zero_sales)

hp_with_zero_sales$hp_cd <- c((max(attr_hp_temp3$hp_cd) + 1) : (max(attr_hp_temp3$hp_cd) + nrow(hp_with_zero_sales)) )

#rename
hp_with_zero_sales <- hp_with_zero_sales[,c(grep("^facility|^hp_", colnames(hp_with_zero_sales)), grep("^sales",colnames(hp_with_zero_sales)), 
                                            grep("^vst_ct_", colnames(hp_with_zero_sales)))] %>% data.frame( stringsAsFactors = FALSE)
 
colnames(hp_with_zero_sales)[grep("^vst_ct_",colnames(hp_with_zero_sales))] <- paste0("vst_ct (", ref_channel$channel[1:N_Channel],")")
head(hp_with_zero_sales)

attr_hp_temp4 <- bind_rows(attr_hp_temp3, hp_with_zero_sales) %>% data.frame(stringsAsFactors = FALSE)

colnames(attr_hp_temp4) <- colnames(attr_hp_temp3)
tail(attr_hp_temp4)

# replace NA with 0
attr_hp_temp4b <- apply(attr_hp_temp4[,grep("^attribution", colnames(attr_hp_temp4))], 2, function(x){x <- ifelse(is.na(x),0,x) 
                          return(x)})
head(attr_hp_temp4b)

attr_hp_temp4c <- cbind(attr_hp_temp4[,-grep("^attribution", colnames(attr_hp_temp4))], attr_hp_temp4b)

load("Rx_Promo_Data_for_Model_Nov07.RData")

Facility_master <- Facility_Master_Data_DM
n_distinct(Facility_master$facility_id)

attr_hp_temp4d <- Facility_master %>% inner_join(attr_hp_temp4c, by=c("facility_id" = "facility_id"))

# final data on hp level
attr_hp <- cbind(attr_hp_temp4d[,grep("^facility_id|^hp_cd",colnames(attr_hp_temp4d))],  attr_hp_temp4d[,-grep("^facility_id|^hp_cd",colnames(attr_hp_temp4d))])

write.csv(attr_hp, paste0("./HP_Results/", hp_final_model_folder, "/attr_hp.csv"), row.names=FALSE)
save(attr_hp, file = paste0("./HP_Results/", hp_final_model_folder, "/attr_hp.RData") )


##########################
##  Area Level attribution
##########################

vars_to_sum <- grep("^sales|^vst_ct|^attribution", colnames(attr_hp)) %>% colnames(attr_hp)[.]
attr_area_tmp <- attr_hp %>% 
                  group_by(area_id) %>% 
                  summarise_at(vars(vars_to_sum), sum) %>% ungroup() %>%
                  data.frame(stringsAsFactors = FALSE)

colnames(attr_area_tmp)[grep("^vst_ct|^attribution", colnames(attr_area_tmp))] <- colnames(attr_hp)[grep("^vst_ct|^attribution", colnames(attr_hp))]

area_contr_rate <- attr_area_tmp[,grep("^attribution", colnames(attr_area_tmp))]/attr_area_tmp[,"sales"]

# apply(area_contr_rate, 1, sum)
colnames(area_contr_rate) <- paste0("contribution rate (", c("Baseline", ref_channel[,"channel"]), ")")

attr_area <- cbind(attr_area_tmp, area_contr_rate)

write.csv(attr_area, paste0("./HP_Results/", hp_final_model_folder, "/attr_area.csv"), row.names=FALSE)
save(attr_area, file = paste0("./HP_Results/", hp_final_model_folder, "/attr_area.RData") )

