rm(list = ls())
library(dplyr)
library(data.table)
library(stringr)

library(pdftools)
library(qpdf)

#Extract tables
library(tabulizer)
library(splitstackshape)

setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

#I have to split this process into parts for each of the hospitals because my memory can only handle so much.

#####DANBURY#####
######PART 1#####
danbury_file <- file.path('files/danburyCPTs.pdf')
danbury1 <- pdf_subset(danbury_file, pages = 3:1000, output = 'files/danbury1.pdf')

danbury1 <- extract_tables(danbury1, method = "lattice")

#Create an empty list to stage the data
danbury_df1 <- data.frame(Entry = vector(mode = "character", length = length(danbury1)))

#For iterating the list object
#i = 1

for (i in 1:length(danbury1)) {
  danbury_df1$Entry[i] <- danbury1[[i]]
}

danbury_df1_split <- cSplit(danbury_df1, 'Entry', sep="\r", type.convert=FALSE)
danbury_df1_split <- danbury_df1_split %>% 
  select(c(Entry_03,Entry_04,Entry_05,Entry_17,Entry_18))

#danbury_df_copy <- danbury_df
danbury_df1_split$Entry_17 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",danbury_df1_split$Entry_17,ignore.case=T)
danbury_df1_split$Entry_17 <- gsub("\\$","",danbury_df1_split$Entry_17,ignore.case=T)
danbury_df1_split$Entry_17 <- gsub("\\,","",danbury_df1_split$Entry_17,ignore.case=T)
danbury_df1_split$Entry_17 <- as.numeric(danbury_df1_split$Entry_17)

danbury_df1_split$Entry_18 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",danbury_df1_split$Entry_18,ignore.case=T)
danbury_df1_split$Entry_18 <- gsub("\\$","",danbury_df1_split$Entry_18,ignore.case=T)
danbury_df1_split$Entry_18 <- gsub("\\,","",danbury_df1_split$Entry_18,ignore.case=T)
danbury_df1_split$Entry_18 <- as.numeric(danbury_df1_split$Entry_18)

danbury_df1_split$Entry_03 <- str_sub(danbury_df1_split$Entry_03,-5,-1)

danbury_df1_split$Entry_04 <- gsub("Payer","",danbury_df1_split$Entry_04,ignore.case=T)

danbury_df1_split$Entry_05 <- gsub("UNDERSTANDING YOUR VISITUNDERSTANDING YOUR PAYMENT","",danbury_df1_split$Entry_05,ignore.case=T)
danbury_df1_split$Entry_05 <- gsub("Payer","",danbury_df1_split$Entry_05,ignore.case=T)


#####DANBURY#####
######PART 2#####
rm(danbury1)
gc()

danbury2 <- pdf_subset(danbury_file, pages = 1001:2000, output = 'files/danbury2.pdf')
danbury2 <- extract_tables(danbury2, method = "lattice")

#Create an empty df to stage the data
danbury_df2 <- data.frame(Entry = vector(mode = "character", length = length(danbury2)))

for (i in 1:length(danbury2)) {
  danbury_df2$Entry[i] <- danbury2[[i]]
}

danbury_df2_split <- cSplit(danbury_df2, 'Entry', sep="\r", type.convert=FALSE)
danbury_df2_split <- danbury_df2_split %>% 
  select(c(Entry_03,Entry_04,Entry_05,Entry_17,Entry_18))

danbury_df2_split$Entry_17 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",danbury_df2_split$Entry_17,ignore.case=T)
danbury_df2_split$Entry_17 <- gsub("\\$","",danbury_df2_split$Entry_17,ignore.case=T)
danbury_df2_split$Entry_17 <- gsub("\\,","",danbury_df2_split$Entry_17,ignore.case=T)
danbury_df2_split$Entry_17 <- as.numeric(danbury_df2_split$Entry_17)

danbury_df2_split$Entry_18 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",danbury_df2_split$Entry_18,ignore.case=T)
danbury_df2_split$Entry_18 <- gsub("\\$","",danbury_df2_split$Entry_18,ignore.case=T)
danbury_df2_split$Entry_18 <- gsub("\\,","",danbury_df2_split$Entry_18,ignore.case=T)
danbury_df2_split$Entry_18 <- as.numeric(danbury_df2_split$Entry_18)

danbury_df2_split$Entry_03 <- str_sub(danbury_df2_split$Entry_03,-5,-1)

danbury_df2_split$Entry_04 <- gsub("Payer","",danbury_df2_split$Entry_04,ignore.case=T)

danbury_df2_split$Entry_05 <- gsub("UNDERSTANDING YOUR VISITUNDERSTANDING YOUR PAYMENT","",danbury_df2_split$Entry_05,ignore.case=T)
danbury_df2_split$Entry_05 <- gsub("Payer","",danbury_df2_split$Entry_05,ignore.case=T)


#####DANBURY#####
######PART 3#####
rm(danbury2)
gc()

danbury3 <- pdf_subset(danbury_file, pages = 2001:2766, output = 'files/danbury3.pdf')
danbury3 <- extract_tables(danbury3, method = "lattice")

#Create an empty df to stage the data
danbury_df3 <- data.frame(Entry = vector(mode = "character", length = length(danbury3)))

for (i in 1:length(danbury3)) {
  danbury_df3$Entry[i] <- danbury3[[i]]
}

danbury_df3_split <- cSplit(danbury_df3, 'Entry', sep="\r", type.convert=FALSE)
danbury_df3_split <- danbury_df3_split %>% 
  select(c(Entry_03,Entry_04,Entry_05,Entry_17,Entry_18))

danbury_df3_split$Entry_17 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",danbury_df3_split$Entry_17,ignore.case=T)
danbury_df3_split$Entry_17 <- gsub("\\$","",danbury_df3_split$Entry_17,ignore.case=T)
danbury_df3_split$Entry_17 <- gsub("\\,","",danbury_df3_split$Entry_17,ignore.case=T)
danbury_df3_split$Entry_17 <- as.numeric(danbury_df3_split$Entry_17)

danbury_df3_split$Entry_18 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",danbury_df3_split$Entry_18,ignore.case=T)
danbury_df3_split$Entry_18 <- gsub("\\$","",danbury_df3_split$Entry_18,ignore.case=T)
danbury_df3_split$Entry_18 <- gsub("\\,","",danbury_df3_split$Entry_18,ignore.case=T)
danbury_df3_split$Entry_18 <- as.numeric(danbury_df3_split$Entry_18)

danbury_df3_split$Entry_03 <- str_sub(danbury_df3_split$Entry_03,-5,-1)

danbury_df3_split$Entry_04 <- gsub("Payer","",danbury_df3_split$Entry_04,ignore.case=T)

danbury_df3_split$Entry_05 <- gsub("UNDERSTANDING YOUR VISITUNDERSTANDING YOUR PAYMENT","",danbury_df3_split$Entry_05,ignore.case=T)
danbury_df3_split$Entry_05 <- gsub("Payer","",danbury_df3_split$Entry_05,ignore.case=T)


####BIND EM BABY####
danbury_df <- rbind(danbury_df1_split,danbury_df2_split,danbury_df3_split)

#Replace empty values with NA.
#I found this only represented one DRG code so we're just going to drop it.
#Realized I can also drop 18 in that case.
danbury_df <- danbury_df %>% 
  filter(!(is.na(Entry_17) & is.na(Entry_18))) %>% 
  mutate_all(na_if,"") %>% 
  filter(!(grepl('DRG', Entry_04))) %>% 
  select(-Entry_18, -Entry_05) %>% 
  rename(Code = Entry_03,Payor=Entry_04,NegotiatedCharge=Entry_17)

danbury_df$Type <- 'CPT'
danbury_df$HealthSystem <- 'Nuvance Health'
danbury_df$Hospital <- 'Danbury Hospital'
write.csv(danbury_df,'carpentry/danburyCPTs.csv')
rm(list = ls())


#####NORWALK#####
######PART 1#####

gc()
norwalk_file <- file.path('files/norwalkCPTs.pdf')
norwalk1 <- pdf_subset(norwalk_file, pages = 3:1000, output = 'files/norwalk1.pdf')
norwalk1 <- extract_tables(norwalk1, method = "lattice")

norwalk_df1 <- data.frame(Entry = vector(mode = "character", length = length(norwalk1)))

for (i in 1:length(norwalk1)) {
  norwalk_df1$Entry[i] <- norwalk1[[i]]
}

######PART 2#####
rm(norwalk1)
gc()
norwalk2 <- pdf_subset(norwalk_file, pages = 1001:2000, output = 'files/norwalk2.pdf')
norwalk2 <- extract_tables(norwalk2, method = "lattice")
norwalk_df2 <- data.frame(Entry = vector(mode = "character", length = length(norwalk2)))
for (i in 1:length(norwalk2)) {
  norwalk_df2$Entry[i] <- norwalk2[[i]]
}

######PART 3#####
rm(norwalk2)
gc()
norwalk3 <- pdf_subset(norwalk_file, pages = 2001:2431, output = 'files/norwalk3.pdf')
norwalk3 <- extract_tables(norwalk3, method = "lattice")
norwalk_df3 <- data.frame(Entry = vector(mode = "character", length = length(norwalk3)))
for (i in 1:length(norwalk3)) {
  norwalk_df3$Entry[i] <- norwalk3[[i]]
}
rm(norwalk3)

#BIND#
norwalk_df <- rbind(norwalk_df1,norwalk_df2,norwalk_df3)
rm(norwalk_df1,norwalk_df2,norwalk_df3)

#CLEAN#
norwalk_df_split <- cSplit(norwalk_df, 'Entry', sep="\r", type.convert=FALSE)
norwalk_df_split <- norwalk_df_split %>% 
  select(c(Entry_03,Entry_04,Entry_17))

norwalk_df_split$Entry_17 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",norwalk_df_split$Entry_17,ignore.case=T)
norwalk_df_split$Entry_17 <- gsub("\\$","",norwalk_df_split$Entry_17,ignore.case=T)
norwalk_df_split$Entry_17 <- gsub("\\,","",norwalk_df_split$Entry_17,ignore.case=T)
norwalk_df_split$Entry_17 <- as.numeric(norwalk_df_split$Entry_17)

norwalk_df_split$Entry_03 <- str_sub(norwalk_df_split$Entry_03,-5,-1)

norwalk_df_split$Entry_04 <- gsub("Payer","",norwalk_df_split$Entry_04,ignore.case=T)

#Not going to keep 18 and 5 this time, having seen the result from Danbury.
norwalk_df_split <- norwalk_df_split %>% 
  filter(!(is.na(Entry_17))) %>% 
  mutate_all(na_if,"") %>% 
  filter(!(grepl('DRG', Entry_04))) %>% 
  rename(Code = Entry_03,Payor=Entry_04,NegotiatedCharge=Entry_17)

norwalk_df_split$Type <- 'CPT'
norwalk_df_split$HealthSystem <- 'Nuvance Health'
norwalk_df_split$Hospital <- 'Norwalk Hospital'
write.csv(norwalk_df_split,'carpentry/norwalkCPTs.csv')
rm(list = ls())



#####SHARON#####
######PART 1#####
gc()
sharon_file <- file.path('files/sharonCPTs.pdf')
sharon1 <- pdf_subset(sharon_file, pages = 3:1000)
sharon1 <- extract_tables(sharon1, method = "lattice")

sharon_df1 <- data.frame(Entry = vector(mode = "character", length = length(sharon1)))

for (i in 1:length(sharon1)) {
  sharon_df1$Entry[i] <- sharon1[[i]]
}

######PART 2#####
rm(sharon1)
gc()
sharon2 <- pdf_subset(sharon_file, pages = 1001:1320)
sharon2 <- extract_tables(sharon2, method = "lattice")
sharon_df2 <- data.frame(Entry = vector(mode = "character", length = length(sharon2)))
for (i in 1:length(sharon2)) {
  sharon_df2$Entry[i] <- sharon2[[i]]
}
rm(sharon2)

#BIND#
sharon_df <- rbind(sharon_df1,sharon_df2)
rm(sharon_df1,sharon_df2)

#CLEAN#
sharon_df_split <- cSplit(sharon_df, 'Entry', sep="\r", type.convert=FALSE)
sharon_df_split <- sharon_df_split %>% 
  select(c(Entry_03,Entry_04,Entry_17))

sharon_df_split$Entry_17 <- gsub("Average Negotiated Charge \\(Payment\\) \\/ Visit","",sharon_df_split$Entry_17,ignore.case=T)
sharon_df_split$Entry_17 <- gsub("\\$","",sharon_df_split$Entry_17,ignore.case=T)
sharon_df_split$Entry_17 <- gsub("\\,","",sharon_df_split$Entry_17,ignore.case=T)
sharon_df_split$Entry_17 <- as.numeric(sharon_df_split$Entry_17)

sharon_df_split$Entry_03 <- str_sub(sharon_df_split$Entry_03,-5,-1)

sharon_df_split$Entry_04 <- gsub("Payer","",sharon_df_split$Entry_04,ignore.case=T)

#Not going to keep 18 and 5 this time, having seen the result from Danbury.
sharon_df_split <- sharon_df_split %>% 
  filter(!(is.na(Entry_17))) %>% 
  mutate_all(na_if,"") %>% 
  filter(!(grepl('DRG', Entry_04))) %>% 
  rename(Code = Entry_03,Payor=Entry_04,NegotiatedCharge=Entry_17)

sharon_df_split$Type <- 'CPT'
sharon_df_split$HealthSystem <- 'Nuvance Health'
sharon_df_split$Hospital <- 'Sharon Hospital'
write.csv(sharon_df_split,'carpentry/sharonCPTs.csv')
rm(list = ls())


#Read back in and bind them all
danbury <- read.csv('carpentry/danburyCPTs.csv')
norwalk <- read.csv('carpentry/norwalkCPTs.csv')
sharon <- read.csv('carpentry/sharonCPTs.csv')

nuvance <- rbind(danbury,norwalk,sharon) %>% 
  select(-c(X,Type))
rm(danbury,norwalk,sharon)

nuvance$RevenueCode_ExternalID <- NA
write.csv(nuvance,'processed_files/nuvance.csv', row.names=FALSE)

