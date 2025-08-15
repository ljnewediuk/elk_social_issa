## Re-run iSSAs by splitting them up - meeting with Levi 25th October


## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)


# Load data
DT <- readRDS("output/ZOE/5-rdm-locs-sri-NN-N10.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

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


#Calving iSSA
set.seed(123456)
C_10 <- glmmTMB(case_ ~ 
                   ## step length
                   I(log(sl_+1)) + 
                   I(log(sl_+1)):Calving +
                   I(log(sl_+1)):I(log(StartDist + 1))+
                   I(log(sl_+1)):I(log(sri_startNN+0.125))+
                   I(log(sl_+1)):Wang_Start_NN+
                   
                   #Timing variables
                   Year+
                   
                   ## social variables in interactions with movement and habitat 
                   I(log(EndDist + 1)) + 
                   I(log(EndDist + 1)):Calving + 
                   I(log(sri_EndNN+0.125)) +
                    I(log(sri_EndNN+0.125)):Calving +
                   Wang_End_NN +
                   Wang_End_NN: Calving +
                   
                   ## random effects  
                   (1|elk_step_id_) + 
                   (0 + I(log(sl_+1))| ANIMAL_ID) + 
                   (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):I(log(StartDist + 1)) | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):I(log(sri_startNN+0.125)) | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):Wang_Start_NN | ANIMAL_ID) +
                   (0 + I(log(EndDist + 1)) | ANIMAL_ID)+
                   (0 +  I(log(EndDist + 1)):Calving| ANIMAL_ID)+
                   (0 + I(log(sri_EndNN+0.125)) | ANIMAL_ID)+
                   (0 + I(log(sri_EndNN+0.125)):Calving| ANIMAL_ID) +
                   (0 + Wang_End_NN | ANIMAL_ID)+
                  (0 + Wang_End_NN: Calving| ANIMAL_ID),
                 
                 family=poisson(), 
                 data = DT,  
                 map = list(theta=factor(c(NA,1:19))), 
                 start = list(theta=c(log(1000), seq(0,0, length.out = 19))))


summary(C_10)
check_collinearity(C_10)

saveRDS(C_10, "output/ZOE/Model output/C_issa_10.RDS")


### ISSF MODEL WITH ONLY OPEN POINTS AND TIME OF DAY AS A PREDICTOR
DT_OPEN_ONLY <- filter(DT, Cover_start == "Open")
set.seed(123456)
TOD_10 <- glmmTMB(case_ ~ 
                  ## step length
                  I(log(sl_+1)) + 
                  I(log(sl_+1)):tod_end_ +
                  I(log(sl_+1)):I(log(StartDist + 1))+
                  I(log(sl_+1)):I(log(sri_startNN+0.125))+
                  I(log(sl_+1)):Wang_Start_NN+
                  
                  #Timing variables
                  Year+
                  
                  ## social variables in interactions with movement and habitat 
                  I(log(EndDist + 1)) + 
                  I(log(EndDist + 1)):tod_end_ + 
                  I(log(sri_EndNN+0.125)) +
                  I(log(sri_EndNN+0.125)):tod_end_ +
                  Wang_End_NN +
                  Wang_End_NN: tod_end_ +
                  
                  ## random effects  
                  (1|elk_step_id_) + 
                  (0 + I(log(sl_+1))| ANIMAL_ID) + 
                  (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                  (0 + I(log(sl_+1)):I(log(StartDist + 1)) | ANIMAL_ID) +
                  (0 + I(log(sl_+1)):I(log(sri_startNN+0.125)) | ANIMAL_ID) +
                  (0 + I(log(sl_+1)):Wang_Start_NN | ANIMAL_ID) +
                  (0 + I(log(EndDist + 1)) | ANIMAL_ID)+
                  (0 +  I(log(EndDist + 1)):tod_end_| ANIMAL_ID)+
                  (0 + I(log(sri_EndNN+0.125)) | ANIMAL_ID)+
                  (0 + I(log(sri_EndNN+0.125)):tod_end_| ANIMAL_ID) +
                  (0 + Wang_End_NN | ANIMAL_ID)+
                  (0 + Wang_End_NN: tod_end_| ANIMAL_ID),
                
                family=poisson(), 
                data = DT_OPEN_ONLY,  
                map = list(theta=factor(c(NA,1:19))), 
                start = list(theta=c(log(1000), seq(0,0, length.out = 19))))

summary(TOD_10)
check_collinearity(TOD_10)

saveRDS(TOD_10, "output/ZOE/Model output/TOD_issa_10.RDS")

### ISSF MODEL WITH DAY POINTS ONLY DISTANCE TO FOREST AS A PREDICTOR
tod_dat <- read.csv('data/sunrise_sunset_2019.txt') %>%
  rbind(read.csv('data/sunrise_sunset_2020.txt')) %>%
  # Make column for string dates and day, year
  mutate(Date = as.Date(Date, '%b%d%Y'),
         # Make date columns for POSIX sunrise and sunset in EST
         sunrise = lubridate::with_tz(paste(Date, `Sun.rise`, sep = ' '),
                                      tzone = 'America/New_York'),
         sunset = lubridate::with_tz(paste(Date, `Sun.set`, sep = ' '),
                                     tzone = 'America/New_York'),
         # Force into CST
         sunrise = lubridate::force_tz(sunrise, tzone = 'Canada/Central'),
         sunset = lubridate::with_tz(sunset, tzone = 'Canada/Central'),
         # Add julian date and year
         JDate = lubridate::yday(sunrise),
         Year = as.factor(lubridate::year(Date))) %>%
  dplyr::select(Year, JDate, sunrise, sunset)

# Load and combine sample IDs and hormone levels
DT_DAY_ONLY <- DT %>%
  left_join(tod_dat) %>%
  # Add column for day/night (if location is between sunrise and sunset)
  mutate(TOD = ifelse(t2_ <= sunrise | t2_ > sunset, 'night', 'day'))%>%
  #filter for day points only
  filter(TOD == "day")

set.seed(123456)
DTF_10 <- glmmTMB(case_ ~ 
                    ## step length
                    I(log(sl_+1)) + 
                    I(log(sl_+1)):dist_to_forest_start +
                    I(log(sl_+1)):I(log(StartDist + 1))+
                    I(log(sl_+1)):I(log(sri_startNN+0.125))+
                    I(log(sl_+1)):Wang_Start_NN+
                    
                    #Timing variables
                    Year+
                    
                    ## social variables in interactions with movement and habitat 
                    I(log(EndDist + 1)) + 
                    I(log(EndDist + 1)):dist_to_forest_end+ 
                    I(log(sri_EndNN+0.125)) +
                    I(log(sri_EndNN+0.125)):dist_to_forest_end +
                    Wang_End_NN +
                    Wang_End_NN: dist_to_forest_end+
                    
                    ## random effects  
                    (1|elk_step_id_) + 
                    (0 + I(log(sl_+1))| ANIMAL_ID) + 
                    (0 + I(log(sl_+1)):dist_to_forest_end | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):I(log(StartDist + 1)) | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):I(log(sri_startNN+0.125)) | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):Wang_Start_NN | ANIMAL_ID) +
                    (0 + I(log(EndDist + 1)) | ANIMAL_ID)+
                    (0 +  I(log(EndDist + 1)):dist_to_forest_end| ANIMAL_ID)+
                    (0 + I(log(sri_EndNN+0.125)) | ANIMAL_ID)+
                    (0 + I(log(sri_EndNN+0.125)):dist_to_forest_end| ANIMAL_ID) +
                    (0 + Wang_End_NN | ANIMAL_ID)+
                    (0 + Wang_End_NN: dist_to_forest_end| ANIMAL_ID),
                  
                  family=poisson(), 
                  data = DT_DAY_ONLY,  
                  map = list(theta=factor(c(NA,1:11))), 
                  start = list(theta=c(log(1000), seq(0,0, length.out = 11))))
summary(DTF_10)
check_collinearity(DTF_10)

saveRDS(DTF_10, "output/ZOE/Model output/DTF_issa_10.RDS")

***************************STOP HERE***************************************************
  ## ISSF MODEL WITH IDYR AS RANDOM EFFECT
  
  #set.seed(123456)
  #NN_10_IDYR <- glmmTMB(case_ ~ 
  ## step length
  #I(log(sl_+1)) + 
  #I(log(sl_+1)):Cover_start +
  #I(log(sl_+1)):tod_end_ +
  #I(log(sl_+1)):Calving +
  #I(log(sl_+1)):I(log(StartDist + 1))+ 
  
## habitat variables
#Cover_end+

#Timing variables
#Calving:Cover_end+
#tod_end_:Cover_end+

## social variables in interactions with movement and habitat 
#I(log(EndDist + 1)) + 
#I(log(EndDist + 1)):Cover_end + 

## random effects  
#(1|elk_step_id_) + 
#(0 + I(log(sl_+1))| IDYr) + 
#(0 + I(log(sl_+1)):Cover_start | IDYr) +
#(0 + I(log(sl_+1)):Calving | IDYr) +
#(0 + I(log(sl_+1)):tod_end_ | IDYr) +
#(0 + I(log(sl_+1)):I(log(StartDist + 1)) | IDYr)+ 
#(0 + Cover_end | IDYr)+
#(0 + Calving:Cover_end | IDYr)+
#(0 + tod_end_:Cover_end | IDYr)+
#(0 + I(log(EndDist + 1)) | IDYr) + 
#(0 + I(log(EndDist + 1)):Cover_end | IDYr),

#family=poisson(), 
#data = DT,  
#map = list(theta=factor(c(NA,1:38))), 
#start = list(theta=c(log(1000), seq(0,0, length.out = 38))))


#summary(NN_10_IDYR)
#check_collinearity(NN_10_IDYR)

#saveRDS(NN_10_IDYR, "output/ZOE/Model output/NN_issa_10_IDYR.RDS")

### HABITAT ONLY MODEL
set.seed(123456)
H_10 <- glmmTMB(case_ ~ 
                  ## step length
                  I(log(sl_+1)) + 
                  I(log(sl_+1)):Cover_start +
                  I(log(sl_+1)):tod_end_ +
                  I(log(sl_+1)):Calving +
                  
                  ## habitat variables
                  Cover_end+
                  
                  #Timing variables
                  Calving:Cover_end+
                  tod_end_:Cover_end+
                  Year+
                  
                  ## random effects  
                  (1|elk_step_id_) + 
                  (0 + I(log(sl_+1))| ANIMAL_ID) + 
                  (0 + I(log(sl_+1)):Cover_start | ANIMAL_ID) +
                  (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                  (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                  (0 + Cover_end | ANIMAL_ID)+
                  (0 + Calving:Cover_end | ANIMAL_ID)+
                  (0 + tod_end_:Cover_end | ANIMAL_ID),
                
                family=poisson(), 
                data = DT,  
                map = list(theta=factor(c(NA,1:33))), 
                start = list(theta=c(log(1000), seq(0,0, length.out = 33))))


summary(H_10)
check_collinearity(H_10)

saveRDS(H_10, "output/ZOE/Model output/H_issa_10.RDS")




##assign model names
Modnames <- c("G_10", "H_10", "NN_10")

##compute model selection table
aicctable.out <- aictab(cand.set = list(G_10, H_10, NN_10), 
                        modnames = Modnames)

##compute evidence ratio
evidence(aic.table = aicctable.out)












## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)


# Load data
DT <- readRDS("output/ZOE/5-rdm-locs-sri-NN-N10.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

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


#Nearest neighbour iSSA
set.seed(123456)
NN_10 <- glmmTMB(case_ ~ 
                   ## step length
                   I(log(sl_+1)) + 
                   I(log(sl_+1)):Cover_start +
                   I(log(sl_+1)):tod_end_ +
                   I(log(sl_+1)):Calving +
                   I(log(sl_+1)):I(log(StartDist + 1))+ 
                   
                   ## habitat variables
                   Cover_end+
                   
                   #Timing variables
                   Calving:Cover_end+
                   tod_end_:Cover_end+
                   Year+
                   
                   ## social variables in interactions with movement and habitat 
                   I(log(EndDist + 1)) + 
                   I(log(EndDist + 1)):Cover_end + 
                   
                   ## random effects  
                   (1|elk_step_id_) + 
                   (0 + I(log(sl_+1))| ANIMAL_ID) + 
                   (0 + I(log(sl_+1)):Cover_start | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):I(log(StartDist + 1)) | ANIMAL_ID)+ 
                   (0 + Cover_end | ANIMAL_ID)+
                   (0 + Calving:Cover_end | ANIMAL_ID)+
                   (0 + tod_end_:Cover_end | ANIMAL_ID)+
                   (0 + I(log(EndDist + 1)) | ANIMAL_ID) + 
                   (0 + I(log(EndDist + 1)):Cover_end | ANIMAL_ID),
                 
                 family=poisson(), 
                 data = DT,  
                 map = list(theta=factor(c(NA,1:38))), 
                 start = list(theta=c(log(1000), seq(0,0, length.out = 38))))


summary(NN_10)
check_collinearity(NN_10)

saveRDS(NN_10, "output/ZOE/Model output/NN_issa_10.RDS")

## ISSF MODEL WITH IDYR AS RANDOM EFFECT

#set.seed(123456)
#NN_10_IDYR <- glmmTMB(case_ ~ 
                   ## step length
                   #I(log(sl_+1)) + 
                   #I(log(sl_+1)):Cover_start +
                   #I(log(sl_+1)):tod_end_ +
                   #I(log(sl_+1)):Calving +
                   #I(log(sl_+1)):I(log(StartDist + 1))+ 
                   
                   ## habitat variables
                   #Cover_end+
                   
                   #Timing variables
                   #Calving:Cover_end+
                   #tod_end_:Cover_end+
                   
                   ## social variables in interactions with movement and habitat 
                   #I(log(EndDist + 1)) + 
                   #I(log(EndDist + 1)):Cover_end + 
                   
                   ## random effects  
                   #(1|elk_step_id_) + 
                   #(0 + I(log(sl_+1))| IDYr) + 
                   #(0 + I(log(sl_+1)):Cover_start | IDYr) +
                   #(0 + I(log(sl_+1)):Calving | IDYr) +
                   #(0 + I(log(sl_+1)):tod_end_ | IDYr) +
                   #(0 + I(log(sl_+1)):I(log(StartDist + 1)) | IDYr)+ 
                   #(0 + Cover_end | IDYr)+
                   #(0 + Calving:Cover_end | IDYr)+
                   #(0 + tod_end_:Cover_end | IDYr)+
                   #(0 + I(log(EndDist + 1)) | IDYr) + 
                   #(0 + I(log(EndDist + 1)):Cover_end | IDYr),
                 
                 #family=poisson(), 
                 #data = DT,  
                 #map = list(theta=factor(c(NA,1:38))), 
                 #start = list(theta=c(log(1000), seq(0,0, length.out = 38))))


#summary(NN_10_IDYR)
#check_collinearity(NN_10_IDYR)

#saveRDS(NN_10_IDYR, "output/ZOE/Model output/NN_issa_10_IDYR.RDS")


### ISSF MODEL WITH SRI 
set.seed(123456)
SRI_10 <- glmmTMB(case_ ~ 
                    ## step length
                    I(log(sl_+1)) + 
                    I(log(sl_+1)):Cover_start +
                    I(log(sl_+1)):tod_end_ +
                    I(log(sl_+1)):Calving +
                    I(log(sl_+1)):I(log(sri_startNN+0.125)) +
                    
                    ## habitat variables
                    Cover_end+
                    
                    #Timing variables
                    Calving:Cover_end+
                    tod_end_:Cover_end+
                    Year+
                    
                    ## social variables in interactions with movement and habitat 
                    I(log(sri_EndNN+0.125)) +
                    I(log(sri_EndNN+0.125)):Cover_end + 
                    
                    ## random effects  
                    (1|elk_step_id_) + 
                    (0 + I(log(sl_+1))| ANIMAL_ID) + 
                    (0 + I(log(sl_+1)):Cover_start | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                    (0 + I(log(sl_+1)):I(log(sri_startNN+0.125)) | ANIMAL_ID)+
                    (0 + Cover_end | ANIMAL_ID)+
                    (0 + Calving:Cover_end | ANIMAL_ID)+
                    (0 + tod_end_:Cover_end | ANIMAL_ID)+
                    (0 + I(log(sri_EndNN+0.125)) | ANIMAL_ID) +
                    (0 + I(log(sri_EndNN+0.125)):Cover_end |ANIMAL_ID),
                  
                  family=poisson(), 
                  data = DT,  
                  map = list(theta=factor(c(NA,1:38))), 
                  start = list(theta=c(log(1000), seq(0,0, length.out = 38))))
  

summary(SRI_10)
check_collinearity(SRI_10)

saveRDS(SRI_10, "output/ZOE/Model output/SRI_issa_10.RDS")

### ISSF MODEL WITH GENETICS
set.seed(123456)
G_10 <- glmmTMB(case_ ~ 
                   ## step length
                   I(log(sl_+1)) + 
                   I(log(sl_+1)):Cover_start +
                   I(log(sl_+1)):tod_end_ +
                   I(log(sl_+1)):Calving +
                   I(log(sl_+1)):Wang_Start_NN+ 
                   
                   ## habitat variables
                   Cover_end+
                   
                   #Timing variables
                   Calving:Cover_end+
                   tod_end_:Cover_end+
                   Year+
                   
                   ##Genetic relatedness
                    Wang_End_NN + 
                  Wang_End_NN:Cover_end + 
                   
                   ## random effects  
                   (1|elk_step_id_) + 
                   (0 + I(log(sl_+1))| ANIMAL_ID) + 
                   (0 + I(log(sl_+1)):Cover_start | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):Wang_Start_NN | ANIMAL_ID)+ 
                   (0 + Cover_end | ANIMAL_ID)+
                   (0 + Calving:Cover_end | ANIMAL_ID)+
                   (0 + tod_end_:Cover_end | ANIMAL_ID)+
                   (0 + Wang_End_NN | ANIMAL_ID) + 
                   (0 + Wang_End_NN:Cover_end | ANIMAL_ID),
                 
                 family=poisson(), 
                 data = DT,  
                 map = list(theta=factor(c(NA,1:38))), 
                 start = list(theta=c(log(1000), seq(0,0, length.out = 38))))


summary(G_10)
check_collinearity(G_10)

saveRDS(G_10, "output/ZOE/Model output/G_issa_10.RDS")


### HABITAT ONLY MODEL
set.seed(123456)
H_10 <- glmmTMB(case_ ~ 
                   ## step length
                   I(log(sl_+1)) + 
                   I(log(sl_+1)):Cover_start +
                   I(log(sl_+1)):tod_end_ +
                   I(log(sl_+1)):Calving +
                   
                   ## habitat variables
                   Cover_end+
                   
                   #Timing variables
                   Calving:Cover_end+
                   tod_end_:Cover_end+
                   Year+
                   
                   ## random effects  
                   (1|elk_step_id_) + 
                   (0 + I(log(sl_+1))| ANIMAL_ID) + 
                   (0 + I(log(sl_+1)):Cover_start | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):tod_end_ | ANIMAL_ID) +
                   (0 + I(log(sl_+1)):Calving | ANIMAL_ID) +
                   (0 + Cover_end | ANIMAL_ID)+
                   (0 + Calving:Cover_end | ANIMAL_ID)+
                   (0 + tod_end_:Cover_end | ANIMAL_ID),

                 family=poisson(), 
                 data = DT,  
                 map = list(theta=factor(c(NA,1:33))), 
                 start = list(theta=c(log(1000), seq(0,0, length.out = 33))))


summary(H_10)
check_collinearity(H_10)

saveRDS(H_10, "output/ZOE/Model output/H_issa_10.RDS")







##assign model names
Modnames <- c("G_10", "H_10", "NN_10")

##compute model selection table
aicctable.out <- aictab(cand.set = list(G_10, H_10, NN_10), 
                        modnames = Modnames)

##compute evidence ratio
evidence(aic.table = aicctable.out)