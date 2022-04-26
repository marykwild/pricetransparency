library(dplyr)
library(reshape2)
library(jsonlite)
library(splitstackshape)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

#Note: [] had to be deleted from the front and end of the JSON document.
file <- "files/bristol-hospital_standardcharges.json"

bristol <- fromJSON(file) %>% as.data.frame

#For self pay file, skip to end.

bristol <- bristol %>% 
  subset(select=-c(lastUpdated,item.Cash_Discount_Price,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed,item.description,item.Gross_Charge)) %>% 
  rename(Code = item.Associated_Codes, NegotiatedCharge = item.Payer_Allowed_Amount, Payor = item.payer, InpatientOutpatient = item.iobSelection) %>% 
  filter(NegotiatedCharge != "N/A")

bristol <- cSplit(bristol, 'Code', sep=",", type.convert=FALSE)
bristol <- melt(bristol, id.vars = c('InpatientOutpatient','Payor','NegotiatedCharge'), value.name = "Code") %>% subset(select = -variable)

bristol$NegotiatedCharge <- as.numeric(bristol$NegotiatedCharge)

#You can safely drop the InpatientOutpatient column
unique(bristol %>% subset(select = c(Code,InpatientOutpatient))) %>% subset(select=Code) %>% group_by(Code) %>% summarize(count = n()) %>% filter(count>1)

bristol <- bristol %>% subset(select = -InpatientOutpatient)

bristol$Hospital <- "Bristol Hospital"
bristol$HealthSystem <- "Bristol Health"
bristol$RevenueCode_ExternalID <- NA

write.csv(bristol, "processed_files/bristol.csv", row.names = FALSE)

# ==== Cash price file ==== #

bristol_cash <- bristol %>% 
  subset(select=-c(lastUpdated,item.description,item.Gross_Charge,item.Payer_Allowed_Amount,item.payer,item.iobSelection)) %>% 
  rename(Code = item.Associated_Codes, cashPrice = item.Cash_Discount_Price, maxRate = item.DeIdentified_Max_Allowed, minRate = item.Deidentified_Min_Allowed) %>% 
  filter(cashPrice != "N/A")
bristol_cash <- unique(bristol_cash)

bristol_cash <- cSplit(bristol_cash, 'Code', sep=",", type.convert=FALSE)
bristol_cash <- melt(bristol_cash, id.vars = c('cashPrice','maxRate','minRate'), value.name = "Code") %>% subset(select = -variable)

bristol_cash$cashPrice <- as.numeric(bristol_cash$cashPrice)
bristol_cash$maxRate <- as.numeric(bristol_cash$maxRate)
bristol_cash$minRate <- as.numeric(bristol_cash$minRate)

bristol_cash <- bristol_cash %>% filter(!is.na(minRate) & !is.na(maxRate))

bristol_cash %>% 
  summarize(under = sum(cashPrice>=maxRate),
            over = sum(cashPrice<maxRate),
            pct = over / sum(under+over)*100)
#The cash price is higher in the vast majority of cases.

write.csv(bristol_cash, 'self_pay_files/bristol_cash.csv', row.names = FALSE)