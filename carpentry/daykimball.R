library(dplyr)
library(stringr)
library(readr)

## CHECKED FOR ANY NEW FILE on 03/30/2022. No update.

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")


##DAY KIMBALL HOSPITAL##
#Note: This was a JSON file with problematic formatting that had to be managed in Python.
day_kimball <- read.csv('files/day_kimball.csv') %>% rename(NegotiatedCharge = Charge)
#After 238878, the code numbers turn into three digits like "101" for example, which is not a format I understand.
day_kimball <- day_kimball[0:238878,]

#Run all this to take out all the web formatting. 
day_kimball$Code <- str_replace(day_kimball$Code, "\">\n", "")
day_kimball$Payor <- str_replace(day_kimball$Payor, "\\[", "")
day_kimball$Payor <- str_replace(day_kimball$Payor, "<Contract>Payer=", "")
day_kimball$Payor <- str_replace(day_kimball$Payor, "\"", "")
day_kimball$Payor <- str_trim(str_replace(day_kimball$Payor, "\"", ""))
day_kimball$NegotiatedCharge <- str_replace(day_kimball$NegotiatedCharge, "\"</Contract>", "")
day_kimball$NegotiatedCharge <- str_replace(day_kimball$NegotiatedCharge, "\"", "")
day_kimball$NegotiatedCharge <- as.numeric(str_replace(day_kimball$NegotiatedCharge, "\\]", ""))

day_kimball$Hospital <- "Day Kimball Healthcare"
day_kimball$HealthSystem <- "Day Kimball Healthcare"
#For these four codes, there's no data in the Contracts tag in the XML, which creates NAs here. 
#85060, 99291, 99292, P9604
day_kimball <- day_kimball %>% filter(!is.na(day_kimball$NegotiatedCharge))
day_kimball$RevenueCode_ExternalID <- NA

write.csv(day_kimball, 'processed_files/day_kimball.csv',row.names = FALSE)

