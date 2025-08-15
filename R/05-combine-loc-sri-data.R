



## NN and SRI derived data ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data
# Outputs: combined SRI and NN file

### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

## Load rdm locs data 
vita <- readRDS("output/ZOE/3-rdm-locs-NN-N10.RDS")

vita$DyadYrIter1 <- as.factor(paste(vita$IDYr,
                                    vita$StartNN_ID, 
                                    vita$iter,
                                    vita$JDate, sep = "_"))
vita$DyadYrIter2 <- as.factor(paste(vita$IDYr,
                                    vita$StartNN_ID, 
                                    vita$iter,
                                    vita$JDate, sep = "_"))


sri <- readRDS("output/ZOE/4-sri-N10.RDS")
sri$DyadYrIter1 <- as.factor(paste(sri$ID1, sri$ID2, sri$iter, sri$JDate, sep = "_"))
sri$DyadYrIter2 <- as.factor(paste(sri$ID2, sri$ID1, sri$iter, sri$JDate, sep = "_"))

sri[,c("Year", "JDate", "iter") := NULL]

## merge modularity data with locs data
aa <- merge(sri, vita,  by = "DyadYrIter1")
bb <- merge(sri, vita,  by = "DyadYrIter2")

vita2 <- rbind(aa[,c("DyadYrIter1", 
                     "DyadYrIter2.x",
                     "DyadYrIter2.y") := NULL],
               bb[,c("DyadYrIter1.y", 
                     "DyadYrIter1.x",
                     "DyadYrIter2") := NULL])

vita2 [,c("ID1", "ID2") := NULL]

vita2<- vita2 %>% 
  rename(sri_startNN = sri)

sri <- readRDS("output/ZOE/4-sri-N10.RDS")
## Add SRI for NN at end of step
vita2$ENDDyadYrIter1 <- as.factor(paste(vita2$IDYr,
                                    vita2$EndNN_ID, 
                                    vita2$iter,
                                    vita2$JDate, sep = "_"))
vita2$ENDDyadYrIter2 <- as.factor(paste(vita2$IDYr,
                                    vita2$EndNN_ID, 
                                    vita2$iter,
                                    vita2$JDate, sep = "_"))


sri$ENDDyadYrIter1 <- as.factor(paste(sri$ID1, sri$ID2, sri$iter, sri$JDate, sep = "_"))
sri$ENDDyadYrIter2 <- as.factor(paste(sri$ID2, sri$ID1, sri$iter, sri$JDate, sep = "_"))

sri[,c("Year", "JDate", "iter") := NULL]

## merge modularity data with locs data
aa <- merge(sri, vita2,  by = "ENDDyadYrIter1")
bb <- merge(sri, vita2,  by = "ENDDyadYrIter2")

vita3 <- rbind(aa[,c("ENDDyadYrIter1", 
                     "ENDDyadYrIter2.x",
                     "ENDDyadYrIter2.y") := NULL],
               bb[,c("ENDDyadYrIter1.y", 
                     "ENDDyadYrIter1.x",
                     "ENDDyadYrIter2") := NULL])


vita3 [,c("ID1", "ID2") := NULL]

vita3<- vita3 %>% 
  rename(sri_EndNN = sri)


## extract EndNN_ID and IDYrIter from main dataset
#EndNN <- data.table(membershipNN = mods$membership, 
#                    EndIDYrIter = mods$IDYrIter)

## add membership for NNend
#DT <- merge(DT, EndNN, by = "EndIDYrIter")
#DT[, comm := (membership==membershipNN)]
#DT$comm[DT$comm == "TRUE"] <- "Same"
#DT$comm[DT$comm == "FALSE"] <- "Different"

#DT[, c("EndIDYrIter") := NULL]


#Add relatedness information into the dataset for issa
R <- read.csv ("data/Vita_Elk_Wang_Relatedness_18ind.csv")
R<- R %>% rename(ID1 = indv1, ID2 = ind2)
Wang <- filter(R, Estimator == "wang")
Wang$ID1 <- gsub("VE", "ER_E", Wang$ID1)
Wang$ID2 <- gsub( "VE", "ER_E", Wang$ID2)

Wang$ENDDyadYr1 <- as.factor(paste(Wang$ID1, Wang$ID2, sep = "_"))
Wang$ENDDyadYr2 <- as.factor(paste(Wang$ID2, Wang$ID1, sep = "_"))
Wang <- setDT(Wang)
Wang[,c("X", "dyad", "Estimator", "CI2.5", "CI97.5") := NULL]


## Add relatedness for NN at end of step
vita3$ENDDyadYr1 <- as.factor(paste(substr(vita3$IDYr, 1, nchar(vita3$IDYr)-5),
                                    substr(vita3$EndNN_ID, 1, nchar(vita3$EndNN_ID)-5), 
                                    sep = "_"))
vita3$ENDDyadYr2 <- as.factor(paste(substr(vita3$IDYr, 1, nchar(vita3$IDYr)-5),
                                    substr(vita3$EndNN_ID, 1, nchar(vita3$EndNN_ID)-5), 
                                    sep = "_"))

## merge modularity data with locs data
aa <- merge(Wang, vita3,  by = "ENDDyadYr1")
bb <- merge(Wang, vita3,  by = "ENDDyadYr2")

vita4 <- rbind(aa[,c("ENDDyadYr1", 
                     "ENDDyadYr2.x",
                     "ENDDyadYr2.y") := NULL],
               bb[,c("ENDDyadYr1.y", 
                     "ENDDyadYr1.x",
                     "ENDDyadYr2") := NULL])


vita4 [,c("ID1", "ID2") := NULL]

vita4<- vita4 %>% 
  rename(Wang_End_NN = Relatedness)


## Add relatedness for NN at start of step
vita4$StartDyadYr1 <- as.factor(paste(substr(vita4$IDYr, 1, nchar(vita4$IDYr)-5),
                                    substr(vita4$StartNN_ID, 1, nchar(vita4$StartNN_ID)-5), 
                                    sep = "_"))
vita4$StartDyadYr2 <- as.factor(paste(substr(vita4$IDYr, 1, nchar(vita4$IDYr)-5),
                                    substr(vita4$StartNN_ID, 1, nchar(vita4$StartNN_ID)-5), 
                                    sep = "_"))

Wang <- Wang %>% rename(StartDyadYr1 = ENDDyadYr1, StartDyadYr2 = ENDDyadYr2)

## merge modularity data with locs data
aa <- merge(Wang, vita4,  by = "StartDyadYr1")
bb <- merge(Wang, vita4,  by = "StartDyadYr2")

vita5 <- rbind(aa[,c("StartDyadYr1", 
                     "StartDyadYr2.x",
                     "StartDyadYr2.y") := NULL],
               bb[,c("StartDyadYr1.y", 
                     "StartDyadYr1.x",
                     "StartDyadYr2") := NULL])


vita5 [,c("ID1", "ID2") := NULL]

vita5<- vita5 %>% 
  rename(Wang_Start_NN = Relatedness)




saveRDS(vita5, "output/ZOE/5-rdm-locs-sri-NN-N10.RDS")
