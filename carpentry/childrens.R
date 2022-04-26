library(dplyr)
library(reshape2)
library(stringr)
library(janitor)
library(splitstackshape)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

path <- "files/2022-CT-Childrens-Pricing-Transparency-Document-NEW-1.xlsx"
sheet_names <- readxl::excel_sheets(path)

xlsx_data <- purrr::map(
  sheet_names,
  ~readxl::read_excel(path,.x,col_types = "text",col_names = FALSE)
)
childrens <- xlsx_data[[1]]

colnames(childrens) <- childrens[3,]
#Skip to the end for the self-pay file. 

childrens <- childrens[-c(1:3),-c(2:8)] %>% clean_names()
colnames(childrens) <- c("Code","Aetna","Anthem","Cigna","Connecticare","HarvardPilgrim","MultiPlan","UnitedHealthcare")

childrens <- childrens %>% melt(id.vars="Code",value.name = "NegotiatedCharge", variable.name = "Payor")
childrens <- childrens %>% filter(nchar(Code) == 5 & !grepl("MS",Code))
childrens$NegotiatedCharge <- round(as.numeric(childrens$NegotiatedCharge),2)

childrens$Hospital <- "Connecticut Children's Medical Center"
childrens$HealthSystem <- "Connecticut Children's Medical Center"
childrens$RevenueCode_ExternalID <- NA

write.csv(childrens, 'processed_files/childrens.csv', row.names = FALSE)

#### Self pay analysis ####

childrens_cash <- childrens[-c(1:3),-c(2:4, 6,9:15)] %>% 
  clean_names()
colnames(childrens_cash) <- c("Code","selfPay","maxRate","minRate")
childrens_cash <- childrens_cash %>% filter(nchar(Code) == 5 & !grepl("MS",Code))
childrens_cash$selfPay <- round(as.numeric(childrens_cash$selfPay) ,2)
childrens_cash$maxRate <- round(as.numeric(childrens_cash$maxRate) ,2)
childrens_cash$minRate <- round(as.numeric(childrens_cash$minRate) ,2)
childrens_cash %>% 
  summarize(under = sum(selfPay>=maxRate),
            over = sum(selfPay<maxRate),
            pct = over / sum(under+over)*100)

write.csv(childrens_cash, 'self_pay_files/childrens_cash.csv', row.names = FALSE)

