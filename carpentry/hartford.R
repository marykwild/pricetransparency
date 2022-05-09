library(dplyr)
library(reshape2)
library(stringr)
library(janitor)
library(splitstackshape)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

hartford_path <- "files/HartfordHealthcare_HartfordHospital.xlsx"
hartford_sheet_names <- readxl::excel_sheets(hartford_path)

centralconn_path <- "files/HartfordHealthcare_CentralConn.xlsx"
centralconn_sheet_names <- readxl::excel_sheets(centralconn_path)

midstate_path <- "files/HartfordHealthcare_Midstate.xlsx"
midstate_sheet_names <- readxl::excel_sheets(midstate_path)

# The Natchaug file is very small and problematic. 
#natchaug_path <- "files/HartfordHealthcare_Natchaug.xlsx"
#natchaug_sheet_names <- readxl::excel_sheets(natchaug_path)

stvincents_path <- "files/HartfordHealthcare_StVincents.xlsx"
stvincents_sheet_names <- readxl::excel_sheets(stvincents_path)

windham_path <- "files/HartfordHealthcare_Windham.xlsx"
windham_sheet_names <- readxl::excel_sheets(windham_path)

chungerford_path <- "files/HartfordHealthcare_CharlotteHungerford.xlsx"
chungerford_sheet_names <- readxl::excel_sheets(chungerford_path)

backus_path <- "files/HartfordHealthcare_Backus.xlsx"
backus_sheet_names <- readxl::excel_sheets(backus_path)


## I tried to loop this process but it wouldn't take. 
xlsx_data <- purrr::map(
  hartford_sheet_names,
  ~readxl::read_excel(hartford_path,.x,col_types = "text",col_names = FALSE)
)
hartford <- xlsx_data[[1]]

xlsx_data <- purrr::map(
  centralconn_sheet_names,
  ~readxl::read_excel(centralconn_path,.x,col_types = "text",col_names = FALSE)
)
centralconn <- xlsx_data[[1]]

xlsx_data <- purrr::map(
  midstate_sheet_names,
  ~readxl::read_excel(midstate_path,.x,col_types = "text",col_names = FALSE)
)
midstate <- xlsx_data[[1]]

xlsx_data <- purrr::map(
  stvincents_sheet_names,
  ~readxl::read_excel(stvincents_path,.x,col_types = "text",col_names = FALSE)
)
stvincents <- xlsx_data[[1]]

xlsx_data <- purrr::map(
  windham_sheet_names,
  ~readxl::read_excel(windham_path,.x,col_types = "text",col_names = FALSE)
)
windham <- xlsx_data[[1]]

xlsx_data <- purrr::map(
  chungerford_sheet_names,
  ~readxl::read_excel(chungerford_path,.x,col_types = "text",col_names = FALSE)
)
chungerford <- xlsx_data[[1]]

xlsx_data <- purrr::map(
  backus_sheet_names,
  ~readxl::read_excel(backus_path,.x,col_types = "text",col_names = FALSE)
)
backus <- xlsx_data[[1]]

rm(xlsx_data)

dfs <- list(backus,centralconn,chungerford,hartford,midstate,stvincents,windham)
df_names <- c("backus","centralconn","chungerford","hartford","midstate","stvincents","windham")

i <- 1
for (df in dfs) {
  #Standardize column names
  df <- df %>% clean_names
  
  #Empty the drop list.
  drop <- c()
  drop <- c("x2","x3","x7","x8","x9","x10","x11","x12","x13","x14")
  print(drop)
  df <- df[,!(names(df) %in% drop)]
  
  #Combine the first two rows with the insurance plan names
  for (k in 1:length(df)) {
    df[1,k] <- paste(df[1,k],df[2,k])
  }
  
  #Drop rows 2 through 6, which are useless.
  df <- df[-c(2:6),]
  
  for (column in colnames(df)) {
    if(which(colnames(df) == column) %% 2 == 0 & !column %in% c("x1","x4","x6")) {
      even_col_index <- which(colnames(df) == column)
      odd_col_index <- even_col_index-1
    
      print(column)
      
      df[1,even_col_index] <- paste(df[1,odd_col_index],"_outpatient")
      df[1,odd_col_index] <- paste(df[1,odd_col_index],"_inpatient")
    }
  }
  
  colnames(df) <- df[1,]
  df <- df[-1,]
  
  #Clean the names again...
  colnames(df)[1] <- "Procedure"
  colnames(df)[2] <- "Code"
  colnames(df)[3] <- "NDC"
  colnames(df)[4] <- "RevenueCode_ExternalID"
  
  df <- reshape2::melt(df, id.vars=c("Procedure","Code","NDC","RevenueCode_ExternalID"), variable.name = "Payor", value.name = "NegotiatedCharge")
  df <- cSplit(df, 'Payor', sep="_", type.convert = FALSE) %>% 
    rename(Payor = Payor_1, InpatientOutpatient = Payor_2)
  df <- df %>% filter(!NegotiatedCharge %in% c("***","NA") & !grepl("MS",Code) & !grepl("CUSTOM",Code) & Code!="0")
  df$Code <- str_sub(df$Code, -5, -1)
  df$NegotiatedCharge <- as.numeric(df$NegotiatedCharge)
  
  df <- unique(df)
  
  assign(df_names[i],df)
  i <- i+1
}

#There's now critical problems with duplicates in this data, unfortunately. 
inp_chk <- backus %>% 
  group_by(Payor, Code, NDC, InpatientOutpatient, Procedure, RevenueCode_ExternalID) %>% 
    #Code, RevenueCode, Payor,Procedure,NDC,InpatientOutpatient) %>% 
  summarize(mincharge = min(NegotiatedCharge), maxcharge = max(NegotiatedCharge)) %>%
  mutate(chargediff = maxcharge-mincharge) %>% 
  filter(chargediff!=0)

backus$Hospital <- "Backus Hospital"
centralconn$Hospital <- "The Hospital of Central Connecticut"
chungerford$Hospital <- "Charlotte Hungerford Hospital"
hartford$Hospital <- "Hartford Hospital"
midstate$Hospital <- "MidState Medical Center"
stvincents$Hospital <- "St. Vincent's Medical Center"
windham$Hospital <- "Windham Hospital"

hartfordhealth <- rbind(backus,centralconn,chungerford,hartford,midstate,stvincents,windham)
rm(backus,centralconn,chungerford,hartford,midstate,stvincents,windham)

hartfordhealth$HealthSystem <- "Hartford HealthCare"
hartfordhealth$Code <- hartfordhealth$Procedure
hartfordhealth <- hartfordhealth %>% 
  subset(select = -Procedure) %>% 
  filter(nchar(Code) == 5)

write.csv(hartfordhealth, 'processed_files/hartford.csv', row.names = FALSE)
