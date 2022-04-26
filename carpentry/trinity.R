library(dplyr)
library(reshape2)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

###TARGET VARIABLE NAMES: 
#Code, Payor, NegotiatedCharge, Hospital, HealthSystem, RevenueCode

#There are four trinity hospitals. All are posted in Excel format.

### DEFINE PATHS AND SHEET NAMES
francis_path <- "files/TrinityNE_SaintFrancisHospitalandMedicalCenter_standardcharges.xlsx"
francis_sheet_names <- readxl::excel_sheets(francis_path)
marys_path <- "files/TrinityNE_SaintMarysHospital_standardcharges.xlsx"
marys_sheet_names <- readxl::excel_sheets(marys_path)
johnson_path <- "files/TrinityNE_JohnsonMemorialHospital_standardcharges.xlsx"
johnson_sheet_names <- readxl::excel_sheets(johnson_path)
#Sinai is a rehab hospital and we may omit.
sinai_path <- "files/TrinityNE_MtSinaiRehabHospital_standardcharges.xlsx"
sinai_sheet_names <- readxl::excel_sheets(sinai_path)
### 


###UNPACK
### FRANCIS
xlsx_data <- purrr::map(
  francis_sheet_names,
  ~readxl::read_excel(francis_path,.x,col_types = "text",col_names = FALSE)
)
francis <- xlsx_data[[1]]

### MARYS
marys_sheet_names
xlsx_data <- purrr::map(
  marys_sheet_names,
  ~readxl::read_excel(marys_path,.x,col_types = "text",col_names = FALSE)
)
marys <- xlsx_data[[1]]

### JOHNSON
xlsx_data <- purrr::map(
  johnson_sheet_names,
  ~readxl::read_excel(johnson_path,.x,col_types = "text",col_names = FALSE)
)
johnson <- xlsx_data[[1]]

### SINAI
xlsx_data <- purrr::map(
  sinai_sheet_names,
  ~readxl::read_excel(sinai_path,.x,col_types = "text",col_names = FALSE)
)
sinai <- xlsx_data[[1]]

rm(francis_path, francis_sheet_names, sinai_path, sinai_sheet_names, johnson_path, johnson_sheet_names, marys_path, marys_sheet_names, xlsx_data)


# 3rd row are the headers
marys <- as.data.frame(apply(marys, 2, function(y) gsub("_Derived Contracted Rate","",y)))
francis <- as.data.frame(apply(francis, 2, function(y) gsub("_Derived Contracted Rate","",y)))
johnson <- as.data.frame(apply(johnson, 2, function(y) gsub("_Derived Contracted Rate","",y)))
sinai <- as.data.frame(apply(sinai, 2, function(y) gsub("_Derived Contracted Rate","",y)))

### Rename columns, drop first three rows
francis[3,] <- gsub(" ", "_", francis[3,])
francis[3,] <- gsub("-", "_", francis[3,])
colnames(francis) <- francis[3,]
francis <- tail(francis, -3)
marys[3,] <- gsub(" ", "_", marys[3,])
marys[3,] <- gsub("-", "_", marys[3,])
colnames(marys) <- marys[3,]
marys <- tail(marys, -3)
johnson[3,] <- gsub(" ", "_", johnson[3,])
johnson[3,] <- gsub("-", "_", johnson[3,])
colnames(johnson) <- johnson[3,]
johnson <- tail(johnson, -3)
sinai[3,] <- gsub(" ", "_", sinai[3,])
sinai[3,] <- gsub("-", "_", sinai[3,])
colnames(sinai) <- sinai[3,]
sinai <- tail(sinai, -3)

### SKIP AHEAD FOR THE CASH PRICE ###

#I'm going to drop Description, Gross_Charge, Discounted_Cash_Price, De-identified_min_contracted_rate, De-identified_max_contracted_rate, Derived_contracted_rate
francis <- francis %>% subset(select = -c(Description, Gross_Charge, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate, Derived_contracted_rate))
marys <- marys %>% subset(select = -c(Description, Gross_Charge, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate, Derived_contracted_rate))
johnson <- johnson %>% subset(select = -c(Description, Gross_Charge, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate, Derived_contracted_rate))
sinai <- sinai %>% subset(select = -c(Description, Gross_Charge, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate, Derived_contracted_rate))

#MELT
francis <- francis %>% melt(id.vars=c("Code","Type"), value.name = "NegotiatedCharge", variable.name = "Payor")
francis$Hospital <- "Saint Francis Hospital and Medical Center"
marys <- marys %>% melt(id.vars=c("Code","Type"), value.name = "NegotiatedCharge", variable.name = "Payor")
marys$Hospital <- "Saint Mary's Hospital"
johnson <- johnson %>% melt(id.vars=c("Code","Type"), value.name = "NegotiatedCharge", variable.name = "Payor")
johnson$Hospital <- "Johnson Memorial Hospital"
sinai <- sinai %>% melt(id.vars=c("Code","Type"), value.name = "NegotiatedCharge", variable.name = "Payor")
sinai$Hospital <- "Mount Sinai Rehabilitation Hospital"

#RBIND
trinity <- rbind(francis,marys,johnson,sinai)
rm(francis,marys,johnson,sinai)

#REMOVE ALL ROWS WITH "N/A"
trinity <- trinity %>% filter(!grepl("N/A", NegotiatedCharge))

#RECONFIGURE COLUMN TYPES
trinity$NegotiatedCharge <- as.numeric(trinity$NegotiatedCharge)
trinity$Payor <- as.factor(trinity$Payor)

#Check: All codes are unique so long as they're grouped by Type and Code.
unique(trinity %>% 
  subset(select = c(Code,Type))) %>% 
  group_by(Code, Type) %>% 
  summarize(count = n()) %>% 
  filter(count>1)

#And all codes are only either inpatient or outpatient, so we can drop the Type column.
unique(trinity %>% 
  subset(select = c(Code,Type))) %>% 
  group_by(Code) %>% 
  summarize(count = n()) %>% 
  filter(count>1)

trinity <- trinity %>% subset(select = -Type)
trinity$HealthSystem <- "Trinity Health of New England"
trinity$RevenueCode_ExternalID <- NA

#Some initial carpentry on the Payor names.

undesirables_space <- c("_", "   ")
undesirables_none <- c(" INC.*", " LTD.*"," LLC.*")

for (sub in undesirables_space) {
  trinity$Payor <- gsub(sub," ", trinity$Payor)
}

for (sub in undesirables_none) {
  trinity$Payor <- gsub(sub, "", trinity$Payor)
}

#Keep only those records where the Code length is 5
trinity <- trinity %>% filter(nchar(Code)==5)

write.csv(trinity, 'processed_files/trinity.csv',row.names = FALSE)


### SELF PAY FILE ###

francis <- francis %>% subset(select = c(Code, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate))
marys <- marys %>% subset(select = c(Code, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate))
johnson <- johnson %>% subset(select = c(Code, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate))
sinai <- sinai %>% subset(select = c(Code, Discounted_Cash_Price, De_identified_min_contracted_rate, De_identified_max_contracted_rate))

francis$Hospital <- "St. Francis"
johnson$Hospital <- "Johnson + Memorial"
marys$Hospital <- "St. Mary's"
sinai$Hospital <- "Mt. Sinai"

trinity_cash <- rbind(francis, marys, johnson, sinai)
rm(francis, marys, johnson, sinai)
colnames(trinity_cash) <- c("Code", "cashPrice", "maxRate", "minRate","Hospital")

trinity_cash <- trinity_cash %>% 
  filter(maxRate!="N/A" & minRate!="N/A" & nchar(Code) == 5)
trinity_cash$cashPrice <- as.numeric(trinity_cash$cashPrice)
trinity_cash$maxRate <- as.numeric(trinity_cash$maxRate)
trinity_cash$minRate <- as.numeric(trinity_cash$minRate)

write.csv(trinity_cash, 'self_pay_files/trinity_cash.csv', row.names = FALSE)
