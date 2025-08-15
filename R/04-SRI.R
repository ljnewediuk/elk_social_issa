

## Cleaned Locs -  SRI ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data
# Outputs: SRI  file

### Packages ----
libs <- c('data.table', 'spatsoc', 'igraph', 'dplyr')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
vita <- readRDS("output/ZOE/3-rdm-locs-NN-N10.RDS")
vita$Year <- as.factor(vita$Year)
vita$IDYrIter <- as.factor(paste(vita$IDYr, vita$iter, sep = "_"))

#####################################################
############# CALCULATE MOVING WINDOW SRI ###########
####################################################

source("functions/get_sri.R")
source("functions/moving_window.R")


out <- c()
timeOut2019 <- c()
timeOut2020 <- c()

summary<- vita %>% 
  group_by(Year) %>% 
  summarise(range = range(JDate))


for(i in 1:11) {
  timeOut2019[[i]] <- vita[iter == i &
                             Year == "2019"][, moving_window(.SD, 121, 213, 
                                                             by = c("iter","Year"))]
}

for(i in 1:11){
  timeOut2020[[i]] <- vita[iter == i &
                              Year == "2020"][, moving_window(.SD, 122, 229, 
                                                             by = c("iter","Year"))]
}

timeOut <- rbind(rbindlist(timeOut2019), 
                    rbindlist(timeOut2020))

#timeOut[, comm := (membershipID1==membershipID2)]
#timeOut$comm[timeOut$comm == "TRUE"] <- "Same"
#timeOut$comm[timeOut$comm == "FALSE"] <- "Different"
#timeOut$comm <- as.factor(timeOut$comm)
#timeOut$membershipID1 <- as.factor(timeOut$membershipID1)
#timeOut$membershipID2 <- as.factor(timeOut$membershipID2)

saveRDS(timeOut, "output/ZOE/4-sri-N10.RDS")

