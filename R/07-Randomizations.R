
### Randomizations ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra',
          'lme4', 'spatsoc', 'igraph', 'gmodels', 'sf')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('output/ZOE/1-clean_vita_elk.Rds')
locs <- (locs %>% st_drop_geometry())
locs <- locs %>% dplyr::filter(lagYear != 'Year3')
locs <- setDT(locs)


### Proximity Based Social Networks ----
# Need to allocate columns since reading from .Rds
if (truelength(locs) == 0) alloc.col(locs)

group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
  locs,
  threshold = 50,
  splitBy = c('lagYear'),
  timegroup = 'timegroup',
  id = 'animal_ID',
  coords = c('EASTING', 'NORTHING')
)

source("functions/dynamic_network2.R")

### Randomizations ----
# Number of iterations
N <-  100

lsDynNets <- lapply(1:N, FUN = function(i) {
  locsRandom <-
    randomizations(
      locs,
      id = 'animal_ID',
      type = 'trajectory',
      splitBy = c('lagYear'),
      group = 'group',
      datetime = 'datetime',
      iterations = 1,
      coords = c('EASTING', 'NORTHING')
    )[!(observed)]
  
  group_times(locsRandom, datetime = 'randomdatetime', threshold = '5 minutes')
  
  group_pts(
    locsRandom,
    threshold = 50,
    splitBy = c('lagYear'),
    timegroup = 'timegroup',
    id = 'animal_ID',
    coords = c('EASTING', 'NORTHING')
  )
  
  print(i)
  
  return(dynamic_network(locsRandom, id = 'animal_ID',
                         c('lagYear'))[, iteration := i])
}
)

dynNets <- rbindlist(lsDynNets)

### Output ----
saveRDS(dynNets, 'output/ZOE/rdmNets-100.RDS')

