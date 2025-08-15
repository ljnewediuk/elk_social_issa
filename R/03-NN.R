## Cleaned Locs - Calculate NN ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/ rdm points
# Outputs: Cleaned collar data + NNID + NNdist 

### Packages ----
libs <- c( 'ggplot2', 'rgdal',  'data.table',
           'spatsoc', 'igraph','devtools', 'asnipe')
lapply(libs, require, character.only = TRUE)

#devtools::install_github('ropensci/spatsoc')
#install_local("/Users/quinnwebber/Downloads/data.table-1.12.6.zip")

### Input raw data ----
DT <- setDT(readRDS("output/ZOE/2-clean-all-rdm-N10.RDS"))

## extract year
DT$Year <- as.factor(DT$Year)
DT$JDate <- yday(DT$t1_)

# Temporal grouping
DT <- group_times(DT, datetime = 't1_', threshold = '5 minutes')
DT <- DT[order(DT$timegroup),]
#DT$timegroupStart <- DT$timegroup
#DT$timegroupEnd <- DT$timegroupStart +1
#DT[,c("timegroup") := NULL]


DT$IDYrTimeIter <- as.factor(paste(DT$IDYr, DT$timegroup, DT$iter, sep = "_"))
#DT$STARTIDYrTimeIter <- as.factor(paste(DT$IDYr, DT$timegroupStart, DT$iter, sep = "_"))
#DT$ENDIDYrTimeIter <- as.factor(paste(DT$IDYr, DT$timegroupEnd, DT$iter, sep = "_"))
#####################################################
############# CALCULDATE DISTANCE ##################
####################################################

## Nearest neighbor at starting step
edistStart <- edge_dist(DT = DT, id = 'IDYr', coords = c('x1_', 'y1_'),
                        timegroup = 'timegroup', threshold = 45000, returnDist = TRUE, 
                        splitBy = c("iter", "Year"))
colnames(edistStart)[6] <- "StartDist"
edistStart <- edistStart[!is.na(edistStart$StartDist)]

edistStart <- edistStart[edistStart[,.I[which.min(StartDist)],by=. (ID1, timegroup, iter)][['V1']]]
edistStart$IDYrTimeIter <- as.factor(paste(edistStart$ID1, 
                                           edistStart$timegroup, 
                                           edistStart$iter, sep = "_"))
edistStart[,c("ID1", "timegroup", "iter", "Year") := NULL]
colnames(edistStart)[1] <- "StartNN_ID"


## Nearest neighbor at end step
edistEnd <- edge_dist(DT = DT, id = 'IDYr', coords = c('x2_', 'y2_'),
                      timegroup = 'timegroup', threshold = 45000, returnDist = TRUE, 
                      splitBy = c("iter", "Year"))
colnames(edistEnd)[6] <- "EndDist"
edistEnd <- edistEnd[!is.na(edistEnd$EndDist)]

edistEnd <- edistEnd[edistEnd[,.I[which.min(EndDist)],by=. (ID1, timegroup, iter)][['V1']]]
edistEnd$IDYrTimeIter <- as.factor(paste(edistEnd$ID1, 
                                         edistEnd$timegroup, 
                                         edistEnd$iter, sep = "_"))
edistEnd[,c("ID1", "timegroup", "iter","Year") := NULL]
colnames(edistEnd)[1] <- "EndNN_ID"

DT <- merge(DT, edistStart, by = "IDYrTimeIter")
DT <- merge(DT, edistEnd, by = "IDYrTimeIter")

DT[, c("IDYrTimeIter", "minutes") := NULL]

###### GENERATE NETWORKS FROM RANDOM POINTS ######
DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('Year', 'iter'),
  timegroup = 'timegroup',
  id = 'IDYr',
  coords = c('x2_', 'y2_')
)

DT$groupEnd <- DT$group

DT[, c("group") := NULL]

saveRDS(DT, "output/ZOE/3-rdm-locs-NN-N10.RDS")

