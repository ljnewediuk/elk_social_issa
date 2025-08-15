## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)


# Load data
DT <- readRDS("output/ZOE/5-rdm-locs-sri-NN-N10.RDS")
median (DT$sl_)
median(DT$ta_, na.rm = TRUE)
range(DT$ta_, na.rm = TRUE)

#Add relatedness information into the dataset for issa
R <- read.csv ("data/Vita_Elk_Wang_Relatedness_18ind.csv")
R<- R %>% rename(ID1_ch = indv1, ID2_ch = ind2)
Wang <- filter(R, Estimator == "wang")

#Re-format the ID columns
DT$ID1_ch <- as.character(DT$ID1)
DT$ID2_ch <- as.character(DT$ID2)

DT$ID1_ch <- substr(DT$ID1_ch,1,nchar(DT$ID1_ch)-5)
DT$ID2_ch <- substr(DT$ID2_ch,1,nchar(DT$ID2_ch)-5)

DT$ID1_ch <- gsub("ER_E", "VE", DT$ID1_ch)
DT$ID2_ch <- gsub("ER_E", "VE", DT$ID2_ch)

DT <- left_join(Wang, DT, by = c("ID1_ch", "ID2_ch"))

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## number of groups
length(unique(DT[iter == 1]$groupEnd))

## create unique step id by animal
DT[,'elk_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')

## Remove rows with Not Available habitat value, these either fell outside the aci raster or in open water
DT<- filter (DT, Cover_end != "NotAvailable")
DT<- filter (DT, Cover_start != "NotAvailable")

##Code discrete variables as factors
DT$Cover_end<- as.factor(DT$Cover_end)
DT$Cover_start <- as.factor(DT$Cover_start)
DT$Calving <- as.factor(DT$Calving)
DT$IDYr <- as.factor(DT$IDYr)


##Select just 2 individuals
DT_4_animals <- filter(DT, ANIMAL_ID == c("ER_E_15", "ER_E_18", "ER_E_24", "ER_E_26"))

# Run full model with 10 available steps for these 2 animals
set.seed(123456)
Full_10_4_animals <- glmmTMB(case_ ~ 
                               ## step length
                               I(log(sl_+1)) + 
                               I(log(sl_+1)):Cover_start +
                               I(log(sl_+1)):tod_end_ +
                               I(log(sl_+1)):Calving +
                               I(log(sl_+1)):I(log(StartDist + 1))+ 
                               #I(log(sl_+1)):I(log(sri+0.125)) +
                               #I(log(sl_+1)):I(log(Relatedness + 1)) +
                               
                               ## habitat variables
                               Cover_end+
                               
                               #Timing variables
                               Calving:Cover_end+
                               tod_end_:Cover_end+
                               Year+
                               
                               ## social variables in interactions with movement and habitat 
                               I(log(EndDist + 1)) + 
                               I(log(EndDist + 1)):Cover_end + 
                               
                               #I(log(sri+0.125)) +
                               #I(log(sri+0.125)):Cover_end + 
                               
                               ## genetic relatedness
                               #I(log(Relatedness + 1)) +
                               #I(log(Relatedness + 1)):Cover_end + 
                               
                               ## random effects  
                               (1|elk_step_id_) + 
                               (0 + I(log(sl_+1))| ANIMAL_ID) + 
                               (0 + I(log(sl_+1)):Cover_start | ANIMAL_ID) +
                               (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                               (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                               (0 + I(log(sl_+1)):I(log(StartDist + 1)) | ANIMAL_ID)+ 
                               #(0 + I(log(sl_+1)):I(log(sri+0.125)) | ANIMAL_ID)+
                               #(0 + I(log(sl_+1)):I(log(Relatedness + 1)) | ANIMAL_ID) +
                               (0 + Cover_end | ANIMAL_ID)+
                               (0 + Calving:Cover_end | ANIMAL_ID)+
                               (0 + tod_end_:Cover_end | ANIMAL_ID)+
                               (0 + I(log(EndDist + 1)) | ANIMAL_ID) + 
                               (0 + I(log(EndDist + 1)):Cover_end | ANIMAL_ID),
                               #(0 + I(log(sri+0.125)) | ANIMAL_ID) +
                               #(0 + I(log(sri+0.125)):Cover_end |ANIMAL_ID)+
                               #(0 + I(log(Relatedness + 1)) |ANIMAL_ID) +
                               #(0 + I(log(Relatedness + 1)):Cover_end | ANIMAL_ID),
                             
                             family=poisson(), 
                             data = DT_4_animals,  
                             map = list(theta=factor(c(NA,1:38))), 
                             start = list(theta=c(log(1000), seq(0,0, length.out = 38))))


summary(Full_10_4_animals)
check_collinearity(NN_10)

saveRDS(NN_10, "output/ZOE/Model output/NN_issa_10.RDS")

new_df <- df %>% group_by(ID) %>% slice_sample(n=500)