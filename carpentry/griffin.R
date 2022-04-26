library(dplyr)
library(reshape2)
library(stringr)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

griffin_path <- "files/Griffin_01132022.xlsx"
griffin_sheet_names <- readxl::excel_sheets(griffin_path)

xlsx_data <- purrr::map(
  griffin_sheet_names,
  ~readxl::read_excel(griffin_path,.x,col_types = "text",col_names = FALSE)
)
griffin <- xlsx_data[[1]]
rm(xlsx_data)

#Reset column names
colnames(griffin) <- list("CDM_PROC","Description","RevenueCode","CPT_HCPCS","StandardCharge","Min","Max","Aetna"," Aetna_Whole_Health","Anthem","Anthem_Pathway","Anthem_Tier","Cigna","Connecticare","Connecticare_Exchange","Oxford_UnitedHealthcare","UBH","CBH","Multiplan")

#Drop faulty header
griffin <- tail(griffin, -1)

#Remove standard, min, max charges
griffin <- griffin %>% subset(select = -c(StandardCharge,Min,Max))

#Unfortunately we're only really looking at CPT/HCPCS codes
griffin %>% 
  filter(!is.na(CPT_HCPCS)) %>% 
  summarize(count = n())

griffin %>% 
  filter(!is.na(CDM_PROC)) %>% 
  summarize(count = n())

griffin %>% 
  filter(!is.na(CDM_PROC) & !is.na(CPT_HCPCS)) %>% 
  summarize(count = n())

griffin <- griffin %>% filter(!is.na(CPT_HCPCS)) %>% subset(select=-c(CDM_PROC,Description))

#MELT
griffin <- griffin %>% melt(id.vars=c("RevenueCode","CPT_HCPCS"), value.name = "NegotiatedCharge", variable.name = "Payor")

griffin$HealthSystem <- "Griffin Health"
griffin$Hospital <- "Griffin Hospital"
griffin$Payor <- as.factor(griffin$Payor)

#Filter all missing values
griffin <- griffin %>% filter(!is.na(NegotiatedCharge) & !NegotiatedCharge %in% c("NA","N/A"))

griffin$NegotiatedCharge <- as.numeric(griffin$NegotiatedCharge)

griffin <- griffin %>% rename(Code = CPT_HCPCS, RevenueCode_ExternalID = RevenueCode)
griffin <- griffin %>% filter(nchar(Code)==5)

#Turns out all codes are only associated with one Revenue Code, so we can drop it. 
unique(griffin %>% 
         subset(select = c(RevenueCode_ExternalID,Code)) %>% 
         group_by(RevenueCode_ExternalID,Code)) %>% 
          summarize(count = n()) %>%
          filter(count>1)

griffin$RevenueCode_ExternalID <- NA

write.csv(griffin,'processed_files/griffin.csv', row.names = FALSE)
