library(dplyr)
library(reshape2)
library(stringr)

rm(list = ls())
setwd("C:/Users/mkwil/Desktop/Reorganized_Price_Transparency")

bridgeport <- read.csv("files/YaleNewHaven_BridgeportHospital_020122.csv")
lawrence <- read.csv("files/YaleNewHaven_LawrenceMemorial_020122.csv")
newhaven <- read.csv("files/YaleNewHavenHospital_020122.csv")
greenwich <- read.csv("files/YaleNewHaven_GreenwichHospital_020122.csv")

bridgeport <- bridgeport %>% subset(select = -c(X,X.12,X.13,X.14))
greenwich <- greenwich %>% subset(select = -c(X,X.12,X.13,X.14))
newhaven <- newhaven %>% subset(select = -c(X,X.12,X.13,X.14))
lawrence <- lawrence %>% subset(select = -c(X,X.12,X.13,X.14))

colnames(bridgeport) <- list("external_ID","CPT_DRG","GH_Default","Self_Pay","Aetna","Anthem","Cigna","Connecticare","Oxford","UnitedHealthcare","Harvard Pilgrim","Yale Health Plan")
bridgeport <- bridgeport[-c(1:3),]

colnames(greenwich) <- list("external_ID","CPT_DRG","GH_Default","Self_Pay","Aetna","Anthem","Cigna","Connecticare","Oxford","UnitedHealthcare","Harvard Pilgrim","Yale Health Plan")
greenwich <- greenwich[-c(1:3),]

colnames(lawrence) <- list("external_ID","CPT_DRG","GH_Default","Self_Pay","Aetna","Anthem","Cigna","Connecticare","Oxford","UnitedHealthcare","Harvard Pilgrim","Yale Health Plan")
lawrence <- lawrence[-c(1:3),]

colnames(newhaven) <- list("external_ID","CPT_DRG","GH_Default","Self_Pay","Aetna","Anthem","Cigna","Connecticare","Oxford","UnitedHealthcare","Harvard Pilgrim","Yale Health Plan")
newhaven <- newhaven[-c(1:3),]

bridgeport <- bridgeport %>% subset(select=-c(GH_Default,Self_Pay))
greenwich <- greenwich %>% subset(select=-c(GH_Default,Self_Pay))
lawrence <- lawrence %>% subset(select=-c(GH_Default,Self_Pay))
newhaven <- newhaven %>% subset(select=-c(GH_Default,Self_Pay))

bridgeport <- melt(bridgeport,id.vars=c("external_ID","CPT_DRG"), variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  rename(Code = CPT_DRG)
bridgeport['Hospital'] <- factor("Bridgeport Hospital")

greenwich <- melt(greenwich,id.vars=c("external_ID","CPT_DRG"), variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  rename(Code = CPT_DRG)
greenwich['Hospital'] <- factor("Greenwich Hospital")

lawrence <- melt(lawrence,id.vars=c("external_ID","CPT_DRG"), variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  rename(Code = CPT_DRG)
lawrence['Hospital'] <- factor("Lawrence + Memorial Hospital")

newhaven <- melt(newhaven,id.vars=c("external_ID","CPT_DRG"), variable.name = "Payor", value.name = "NegotiatedCharge") %>% 
  rename(Code = CPT_DRG)
newhaven['Hospital'] <- factor("Yale New Haven Hospital")

yale <- rbind(newhaven,greenwich,bridgeport,lawrence)
rm(newhaven,greenwich,bridgeport,lawrence)

yale$NegotiatedCharge <- str_trim(yale$NegotiatedCharge, side = "both")
yale$Code <- str_trim(yale$Code, side = "both")

yale <- yale %>% filter(NegotiatedCharge != "-" & Code != '-')
yale$NegotiatedCharge <- gsub("\\,","",yale$NegotiatedCharge,ignore.case=T)
yale$NegotiatedCharge <- as.numeric(yale$NegotiatedCharge)
yale <- yale %>% filter(!is.na(yale$NegotiatedCharge) & !Code %in% c("#N/A", "","0") & nchar(Code) == 5)

yale['HealthSystem'] <- factor("Yale New Haven Health")
yale <- yale %>% rename(RevenueCode_ExternalID = external_ID)

yale <- unique(yale)

yale_downsized <- yale %>% 
  group_by(HealthSystem,Hospital,Payor,Code) %>% 
  summarize(count = n(),
            minNegotiatedCharge = min(NegotiatedCharge), 
            maxNegotiatedCharge = max(NegotiatedCharge), 
            chargediff = max(NegotiatedCharge) - min(NegotiatedCharge))


#I corresponded with Yale and decided a fair route would be to use the minimum charge. However I'm deciding not to remove that data as it could be interesting later. 
rm(yale_downsized)

#yale <- yale %>% subset(select = -c(maxNegotiatedCharge,external_ID,chargediff))
yale <- unique(yale)

dup_chk <- yale %>% 
  group_by(HealthSystem,Hospital,Payor,Code) %>% 
  summarize(count = n()) %>% 
  filter(count>1)

write.csv(yale,"processed_files/yale.csv", row.names = FALSE)


#SELF PAY

bridgeport <- bridgeport %>% subset(select = c(X.1,X.3,X.13,X.14))
bridgeport$Hospital <- "Bridgeport"
greenwich <- greenwich %>% subset(select = c(X.1,X.3,X.13,X.14))
greenwich$Hospital <- "Greenwich"
newhaven <- newhaven %>% subset(select = c(X.1,X.3,X.13,X.14))
newhaven$Hospital <- "New Haven"
lawrence <- lawrence %>% subset(select = c(X.1,X.3,X.13,X.14))
lawrence$Hospital <- "Lawrence + Memorial"

yale_cash <- rbind(bridgeport,greenwich,newhaven,lawrence)
rm(bridgeport,greenwich,newhaven,lawrence)
colnames(yale_cash) <- c("Code","cashPrice","minRate","maxRate","Hospital")
yale_cash <- yale_cash[-c(1:3),]

yale_cash$cashPrice <- str_trim(yale_cash$cashPrice, side = "both")
yale_cash$minRate <- str_trim(yale_cash$minRate, side = "both")
yale_cash$maxRate <- str_trim(yale_cash$maxRate, side = "both")
yale_cash$Code <- str_trim(yale_cash$Code, side = "both")

yale_cash <- yale_cash %>% filter(cashPrice != "-" & Code != '-')

yale_cash$cashPrice <- gsub("\\,","",yale_cash$cashPrice,ignore.case=T)
yale_cash$minRate <- gsub("\\,","",yale_cash$minRate,ignore.case=T)
yale_cash$maxRate <- gsub("\\,","",yale_cash$maxRate,ignore.case=T)

yale_cash$cashPrice <- as.numeric(yale_cash$cashPrice)
yale_cash$minRate <- as.numeric(yale_cash$minRate)
yale_cash$maxRate <- as.numeric(yale_cash$maxRate)

yale_cash <- unique(yale_cash)

yale_cash <- yale_cash %>% filter(!is.na(cashPrice) & !Code %in% c("#N/A", "","0") & nchar(Code) == 5)

write.csv(yale_cash, 'self_pay_files/yale_cash.csv', row.names = FALSE)
