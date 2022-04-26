library(dplyr)
library(reshape2)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

uconn <- read.csv("files/uconn_09_01_2021.csv")

#SKIP FORWARD FOR CASH PRICE

uconn <- uconn %>% subset(select = -c(X,X.2,X.3,X.4,X.5))
colnames(uconn) <- uconn[1,]
uconn <- tail(uconn,-1)

#Melt first this time?
uconn <- melt(uconn,id.vars = c(1,2))

colnames(uconn) <- list("Hospital", "Code", "Payor","NegotiatedCharge")

uconn$NegotiatedCharge <- gsub("\\$", "", uconn$NegotiatedCharge)
uconn$NegotiatedCharge <- gsub(",", "", uconn$NegotiatedCharge)

uconn <- uconn %>% filter(nchar(Code)==5)

uconn$NegotiatedCharge <- as.numeric(uconn$NegotiatedCharge)

dup_chk <- uconn %>% 
  group_by(Hospital,Payor,Code) %>% 
  summarize(count = n(),
            mincharge = min(NegotiatedCharge),
            maxcharge = max(NegotiatedCharge),
            chargediff = maxcharge - mincharge) %>% 
  filter(count>1)


uconn$Hospital <- "John Dempsey Hospital"
uconn$HealthSystem <- "UConn Health"
uconn$RevenueCode_ExternalID <- NA

dup_example <- uconn %>% filter(Payor == "Aetna" & Code == "16035")

write.csv(uconn, 'uconn_dup_example.csv', row.names = FALSE)
write.csv(uconn, 'processed_files/uconn.csv', row.names = FALSE)

## CASH PRICE FILE
uconn_cash <- uconn %>% subset(select = c(X.1,X.3,X.4,X.5))
colnames(uconn_cash) <- c("Code","minRate","maxRate","cashPrice")
uconn_cash <- tail(uconn_cash,-1)
uconn_cash$minRate <- gsub("\\$", "", uconn_cash$minRate)
uconn_cash$minRate <- gsub(",", "", uconn_cash$minRate)
uconn_cash$maxRate <- gsub("\\$", "", uconn_cash$maxRate)
uconn_cash$maxRate <- gsub(",", "", uconn_cash$maxRate)
uconn_cash$cashPrice <- gsub("\\$", "", uconn_cash$cashPrice)
uconn_cash$cashPrice <- gsub(",", "", uconn_cash$cashPrice)

uconn_cash <- uconn_cash %>% filter(nchar(Code)==5)

uconn_cash$minRate <- as.numeric(uconn_cash$minRate)
uconn_cash$maxRate <- as.numeric(uconn_cash$maxRate)
uconn_cash$cashPrice <- as.numeric(uconn_cash$cashPrice)

uconn_cash$Hospital <- "UConn"

write.csv(uconn_cash, 'self_pay_files/uconn_cash.csv', row.names = FALSE)
