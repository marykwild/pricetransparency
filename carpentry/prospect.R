library(dplyr)
library(reshape2)
library(janitor)
library(splitstackshape)
library(jsonlite)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

#Manchester#
#Note: [] had to be deleted from the front and end of the JSON document.
path <- "files/manchester-memorial-hospital-(echn)_standardcharges.json"
manchester <- fromJSON(path) %>% as.data.frame

path <- "files/waterbury-hospital_standardcharges.json"
waterbury <- fromJSON(path) %>% as.data.frame

path <- "files/rockville-general-hospital-(echn)_standardcharges.json"
rockville <- fromJSON(path) %>% as.data.frame

#Drop columns we definitely don't need
manchester <- manchester %>% subset(select = -c(lastUpdated,item.Cash_Discount,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed,item.description,item.Gross_Charge)) %>% 
  filter(!is.na(item.payer) & item.Payer_Allowed_Amount != "N/A") %>% 
  rename(Code=item.Associated_Codes, NegotiatedCharge = item.Payer_Allowed_Amount, inpatientoutpatient = item.iobSelection, Payor = item.payer)

waterbury <- waterbury %>% subset(select = -c(lastUpdated,item.Cash_Discount,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed,item.description,item.Gross_Charge)) %>% 
  filter(!is.na(item.payer) & item.Payer_Allowed_Amount != "N/A") %>% 
  rename(Code=item.Associated_Codes, NegotiatedCharge = item.Payer_Allowed_Amount, inpatientoutpatient = item.iobSelection, Payor = item.payer)

rockville <- rockville %>% subset(select = -c(lastUpdated,item.Cash_Discount,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed,item.description,item.Gross_Charge)) %>% 
  filter(!is.na(item.payer) & item.Payer_Allowed_Amount != "N/A") %>% 
  rename(Code=item.Associated_Codes, NegotiatedCharge = item.Payer_Allowed_Amount, inpatientoutpatient = item.iobSelection, Payor = item.payer)

manchester <- cSplit(manchester, 'Code', sep=",", type.convert=FALSE)
rockville <- cSplit(rockville, 'Code', sep=",", type.convert=FALSE)
waterbury <- cSplit(waterbury, 'Code', sep=",", type.convert=FALSE)

manchester <- melt(manchester, id.vars=c("inpatientoutpatient","Payor","NegotiatedCharge"), value.name = "Code") %>% 
  filter(nchar(Code)==5) %>% 
  subset(select = -variable)

manchester$NegotiatedCharge <- as.numeric(manchester$NegotiatedCharge)
manchester$Hospital <- "Manchester Memorial Hospital"

rockville <- melt(rockville, id.vars=c("inpatientoutpatient","Payor","NegotiatedCharge"), value.name = "Code") %>% 
  filter(nchar(Code)==5) %>% 
  subset(select = -variable)

rockville$NegotiatedCharge <- as.numeric(rockville$NegotiatedCharge)
rockville$Hospital <- "Rockville General Hospital"

waterbury <- melt(waterbury, id.vars=c("inpatientoutpatient","Payor","NegotiatedCharge"), value.name = "Code") %>% 
  filter(nchar(Code)==5) %>% 
  subset(select = -variable)

waterbury$NegotiatedCharge <- as.numeric(waterbury$NegotiatedCharge)
waterbury$Hospital <- "Waterbury Hospital"

echn <- rbind(manchester,rockville,waterbury)
rm(manchester,rockville,waterbury)

echn$HealthSystem <- "Eastern Connecticut Health Network"
echn$RevenueCode_ExternalID <- NA

#We can at least drop the inpatient / outpatient column as it adds nothing.

dup_chk <- echn %>% 
  group_by(Code,Payor,Hospital,inpatientoutpatient) %>% 
  summarize(chargediff = (max(NegotiatedCharge) - min(NegotiatedCharge))) %>% 
  filter(chargediff!=0)
rm(dup_chk)

echn <- echn %>% subset(select = -inpatientoutpatient)

echn <- unique(echn)

#But we still have unexplained duplicates ...
echn <- echn %>% 
  group_by(Hospital,Payor,Code,RevenueCode_ExternalID,HealthSystem) %>% 
  summarize(mincharge = min(NegotiatedCharge), maxcharge = max(NegotiatedCharge), avgCharge = mean(NegotiatedCharge), chargediff = (max(NegotiatedCharge) - min(NegotiatedCharge)))

#The problem isn't too severe.
dim(echn)
dim(echn %>% filter(chargediff == 0))
#In these cases, we'll just take the average. 
dim(echn %>% filter(chargediff <= 100))

echn <- echn %>% 
  filter(chargediff <= 100) %>% 
  subset(select = -c(mincharge,maxcharge,chargediff)) %>% 
  rename(NegotiatedCharge = avgCharge)


##stop
write.csv(echn, 'processed_files/echn.csv', row.names = FALSE)


# ==== Cash discount ==== #

manchester_cash <- manchester %>% subset(select = c(item.Associated_Codes,item.Cash_Discount,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed))
colnames(manchester_cash) <- c("Code","cashRate", "maxRate", "minRate")
manchester_cash <- manchester_cash %>%
  filter(cashRate!="N/A" & maxRate!="N/A" & minRate!="N/A")
manchester_cash <- cSplit(manchester_cash, 'Code', sep=",", type.convert=FALSE)
manchester_cash <- melt(manchester_cash, id.vars=c("cashRate","maxRate","minRate"), value.name = "Code") %>% 
  filter(nchar(Code)==5) %>% 
  subset(select = -variable)

waterbury_cash <- waterbury %>% subset(select = c(item.Associated_Codes,item.Cash_Discount,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed))
colnames(waterbury_cash) <- c("Code","cashRate", "maxRate", "minRate")
waterbury_cash <- waterbury_cash %>%
  filter(cashRate!="N/A" & maxRate!="N/A" & minRate!="N/A")
waterbury_cash <- cSplit(waterbury_cash, 'Code', sep=",", type.convert=FALSE)
waterbury_cash <- melt(waterbury_cash, id.vars=c("cashRate","maxRate","minRate"), value.name = "Code") %>% 
  filter(nchar(Code)==5) %>% 
  subset(select = -variable)

rockville_cash <- rockville %>% subset(select = c(item.Associated_Codes,item.Cash_Discount,item.DeIdentified_Max_Allowed,item.Deidentified_Min_Allowed))
colnames(rockville_cash) <- c("Code","cashRate", "maxRate", "minRate")
rockville_cash <- rockville_cash %>%
  filter(cashRate!="N/A" & maxRate!="N/A" & minRate!="N/A")
rockville_cash <- cSplit(rockville_cash, 'Code', sep=",", type.convert=FALSE)
rockville_cash <- melt(rockville_cash, id.vars=c("cashRate","maxRate","minRate"), value.name = "Code") %>% 
  filter(nchar(Code)==5) %>% 
  subset(select = -variable)

rockville_cash$Hospital <- "Rockville"
manchester_cash$Hospital <- "Manchester"
waterbury_cash$Hospital <- "Waterbury"

prospect_cash <- rbind(rockville_cash,manchester_cash,waterbury_cash)
write.csv(prospect_cash,'self_pay_files/prospect_cash.csv', row.names = FALSE)
