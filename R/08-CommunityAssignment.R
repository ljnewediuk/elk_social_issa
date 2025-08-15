
## Cleaned Locs - Calculate community assortment ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: list of random networks, DT random metrics, observed igraphs for each year
# Outputs: 

### Packages ----
libs <- c('data.table', 'spatsoc', 'asnipe', 'igraph',
          'reshape2', 'assortnet', 'ggplot2')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS("output/ZOE/1-clean_vita_elk.Rds")
DT <- (DT %>% st_drop_geometry())
DT <- DT %>% dplyr::filter(lagYear != 'Year3')
DT <- setDT(DT)


#DT <- DT[season == "winter"]

DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')


DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('lagYear'),
  timegroup = 'timegroup',
  id = 'animal_ID',
  coords = c('EASTING', 'NORTHING')
)

## Network statistics
obs <- readRDS("output/ZOE/6-network-stats.RDS")

## split dataset by herd/Yr
vita19_20 <- DT[lagYear == "Year1"]
vita20_21 <- DT[lagYear == "Year2"]


source("functions/calc_rc.R")
rc_vita2019 <- calc_rc(get_gbi(vita19_20,  id = 'animal_ID', group = 'group'), 
        n.bootstraps=100, plot.result=T)
rc_vita2020 <- calc_rc(get_gbi(vita20_21,  id = 'animal_ID', group = 'group'), 
        n.bootstraps=100, plot.result=T)

## Rcom = 0 indicates no confidence in the assignment of an individual to its community
## Rcom = 1 indicates certainty in the assignment of an individual to its community

sumStats = data.table(N = c(length(unique(vita19_20$animal_ID)),
                 length(unique(vita20_21$animal_ID))),
          rc = c(rc_vita2019, rc_vita2020),
          Q = c(mean(obs[lagYear == "Year1"]$Q),
                mean(obs[lagYear == "Year2"]$Q)),
         obs[, uniqueN(membership), by = c("lagYear")])
 
saveRDS(sumStats, "output/ZOE/8-rcom.RDS")
