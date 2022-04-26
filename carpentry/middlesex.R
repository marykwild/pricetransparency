library(dplyr)
library(reshape2)
library(janitor)
library(splitstackshape)
library(jsonlite)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

middlesex <- clean_names(read.csv("files/middlesex_12282020.csv")) %>% dplyr::rename(Payor_Parent = fsc_rpt_cat3name, Payor = fsc_name)

middlesex <- middlesex %>% dplyr::filter(Gross.Charge.per.CDM!="")
middlesex$rate_methodology <- as.factor(middlesex$rate_methodology)
middlesex$script_type <- as.factor(middlesex$script_type)
middlesex$fsc_category <- as.factor(middlesex$fsc_category)
middlesex$billing_code_type <- as.factor(middlesex$billing_code_type)
middlesex$revenue_code <- as.factor(middlesex$revenue_code)
#Skip to bottom for cash rates.

middlesex <- middlesex %>% filter(billing_code_type == "CPT Code")

#Going with the average charge for now. 
middlesex <- middlesex %>% subset(select = -c(script_type, billing_code_type,billing_code_description,cpt_hcpcs_code,revenue_code,revenue_code_description,gross_charge_per_cdm,deidentified_lowest_negotiated_rate,deidentifed_max_negotiated_rate,rate_methodology))

#Checking on the payor names.
middlesex_payors <- middlesex %>% 
  dplyr::group_by(Payor_Parent,Payor,fsc_category) %>% 
  dplyr::summarize()

middlesex <- middlesex %>% dplyr::filter(!Payor %in% c("AUTO ACCIDENT/NO FAULT","COMMERCIAL GENERIC","COMMERCIAL SPECIFIC","WORK COMP 1ST INSTANCE") & Payor_Parent != "SELF PAY")

require(plyr)
middlesex$Payor <- mapvalues(middlesex$Payor, from = c("BLUE CROSS FEDERAL PPO",	"BLUE CROSS NATIONAL PPO",	"BLUE CROSS PPO (IN STATE)",	"BLUE CROSS PPO (OUT OF STATE)","BLUE CROSS PPO EXCHANGE", "BLUE CROSS EXCHANGE", "BLUE CROSS HMO AND ST PREFERRED"), to = c("ANTHEM	BLUE CROSS FEDERAL PPO","ANTHEM	BLUE CROSS NATIONAL PPO","ANTHEM	BLUE CROSS PPO (IN STATE)","ANTHEM	BLUE CROSS PPO (OUT OF STATE)",	"ANTHEM	BLUE CROSS PPO EXCHANGE", "ANTHEM	BLUE CROSS EXCHANGE",	"ANTHEM	BLUE CROSS HMO AND ST PREFERRED"), warn_missing = TRUE)

#Rename columns
middlesex <- middlesex %>% subset(select = -c(Payor_Parent,fsc_category,service_area,average_charges_per_vist)) %>% 
  dplyr::rename(Code = billing_code, NegotiatedCharge = average_payment_per_vist)

middlesex$RevenueCode_ExternalID <- NA
middlesex$Hospital <- "Middlesex Hospital"
middlesex$HealthSystem <- "Middlesex Health"

write.csv(middlesex,'processed_files/middlesex.csv',row.names = FALSE)

## SELF PAY. 

middlesex_cash <- middlesex %>% 
  filter(Payor_Parent == "SELF PAY" & billing_code_type == "CPT Code") %>% 
  subset(select = c(Payor,Payor_Parent,billing_code,average_payment_per_vist,deidentified_lowest_negotiated_rate,deidentifed_max_negotiated_rate))

write.csv(middlesex_cash,'self_pay_files/middlesex_cash.csv', row.names = FALSE)
