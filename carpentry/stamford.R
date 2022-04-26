rm(list = ls())
library(dplyr)
library(reshape2)
library(stringr)
library(splitstackshape)

setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

###NEXT###
#stamford hospital
stamford <- read.csv('files/StamfordHospital.csv')

names(stamford) <- gsub(x = names(stamford), pattern = "\\SH.MR.", replacement = "")
names(stamford) <- gsub(x = names(stamford), pattern = "\\.Rate", replacement = "")
names(stamford) <- gsub(x = names(stamford), pattern = "\\.", replacement = "_")
stamford <- stamford %>% 
  rename(Code = "CPT_4", Description = "DESC", RevenueCode = "Revenue_Code")

#NAs were reading in funny. Cleaning.
stamford[stamford ==" -   "] <- NA
stamford[stamford ==" NA "] <- NA


#SKIP AHEAD FORF SELF PAY

#investigating "(blank)"
#stamford_blanks <- stamford %>% filter(Code == "(blank)")
#Leaving this here so I remember later, but should prob omit, it seems to be mostly prescriptions w/o a code

stamford <- stamford %>% 
  #  filter(Code != "(blank)") %>% 
  subset(select = -c(Hospital_Charge,Description)) %>% 
  melt(id.vars = c("Code","RevenueCode"), 
       variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  filter(Payor!="Self_Pay" & !is.na(NegotiatedCharge))

stamford['Hospital'] <- "Stamford Hospital"
stamford['HealthSystem'] <- "Stamford Health"


revcodes <- read.csv("files/stamford_revcodes.csv") %>% 
  subset(select = -code_count)

stamford <- merge(stamford, revcodes, by="RevenueCode", all.x = TRUE) %>% 
  subset(select = -RevenueCode) %>% 
  rename(RevenueCode_ExternalID = Description) %>% 
  filter(Code!="(blank)" & NegotiatedCharge!="  ")

stamford$NegotiatedCharge <- as.numeric(gsub(",","",stamford$NegotiatedCharge))

write.csv(stamford,"processed_files/stamford.csv",row.names = FALSE)


### SELF PAY

stamford <- unique(stamford)

stamford_minmax <- stamford %>% 
  filter(Code != "(blank)") %>% 
  subset(select = -c(Hospital_Charge,Description)) %>% 
  melt(id.vars = c("Code","RevenueCode"), 
       variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  filter(!is.na(NegotiatedCharge))
stamford_minmax$NegotiatedCharge <- as.numeric(gsub(",","",stamford_minmax$NegotiatedCharge))

stamford_minmax <- stamford_minmax %>% 
  group_by(Code,RevenueCode) %>% 
  summarize(minRate = min(NegotiatedCharge), maxRate = max(NegotiatedCharge))

stamford_cash <- stamford %>% 
  filter(Code != "(blank)") %>% 
  subset(select = -c(Hospital_Charge,Description)) %>% 
  melt(id.vars = c("Code","RevenueCode"), 
       variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  filter(Payor=="Self_Pay") %>% 
  subset(select = -Payor) %>% 
  rename(cashRate = NegotiatedCharge)
stamford_cash$cashRate <- as.numeric(gsub(",","",stamford_cash$cashRate))

stamford_cash <- merge(stamford_cash, stamford_minmax, by = c("Code","RevenueCode"), all.x = TRUE) %>% 
  filter(!is.na(cashRate))

write.csv(stamford_cash, 'self_pay_files/stamford_cash.csv', row.names = FALSE)
