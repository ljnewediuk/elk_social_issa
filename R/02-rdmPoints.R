## Cleaned Locs - generate random points ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data + NN
# Outputs: 

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'spatsoc', 'amt',
          'tidyverse', 'lubridate', 'raster', 'sp', 'sf', 'rgee', 'reticulate')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
DT <- readRDS("output/ZOE/1-clean_vita_elk.Rds")
DT <- setDT(DT %>% st_drop_geometry())
DT <- filter(DT, Season == "Summer")

## calculate number of fixes per IDYr

summary<- DT %>% 
  group_by(IDYr) %>% 
  summarise(n = n()) %>%
  summarise(mean = mean(n), sd = sd(n), range = range(n))

## order by datetime
DT <- arrange(DT, datetime)
DT <- DT %>% drop_na(datetime)

## Variables
N <- 10
crs <- 26914


### NOTE: everything needs to run by ID, except random steps
## Easting = x axis = x coord = east to west = longitude
## Northing = y axis = ycoord = north to south = latitude

## Generate random steps by ID
## Default SL distribution is gamma and default TA distribution is vonmises (no need to specify)
## Used a 3 minute tolerance around the 30 minute resample rates


rand_by <- function(x, y, t, c, d, e, n, crs) {
  trk <- track(x = x, y = y, t = t, c = c, d = d, crs = crs) %>%
    track_resample(rate = minutes(30), tolerance = minutes(3)) %>%
    steps_by_burst(keep_cols = 'start') %>%
    time_of_day() %>%
    random_steps(n = n)
}


set.seed(123456)

r1 <- DT[, rand_by(
  x = EASTING,
  y = NORTHING,
  t = datetime,
  n = N,
  crs = crs,
  c = calving,
  d = animal_ID
),
by = IDYr]

r1 <- rename(r1, Calving = c, ANIMAL_ID = d)
r1$Year <- year(r1$t1_)

range (r1$dt_)
setDF(r1)

# Load environmental
aci_2019 <- raster("data/aci_2019.tif/")
aci_2020 <- raster("data/aci_2020.tif/")

aci_2019 <- na_if(aci_2019, 0)
aci_2020 <- na_if(aci_2020, 0)

new.extent <- extent(661560, 698070, 5426000, 5499360)
aci_2019 <- crop(x = aci_2019, y = new.extent)
aci_2020 <- crop(x = aci_2020, y = new.extent)

## extract habitat type at end and start of step using R

source("functions/ExtractPoints.R")

r1_2019 <- filter(r1, Year == "2019")
r1_2019$aci_end <- ExtractPoints(matrix(c(r1_2019$x2_, r1_2019$y2_), ncol = 2),
                               raster = aci_2019)
r1_2019$aci_start <- ExtractPoints(matrix(c(r1_2019$x1_, r1_2019$y1_), ncol = 2),
                                 raster = aci_2019)


r1_2020 <- filter(r1, Year == "2020")
r1_2020$aci_end <- ExtractPoints(matrix(c(r1_2020$x2_, r1_2020$y2_), ncol = 2),
                             raster = aci_2020)
r1_2020$aci_start <- ExtractPoints(matrix(c(r1_2020$x1_, r1_2020$y1_), ncol = 2),
                                 raster = aci_2020)




##Calculate the distance from forest for each step

aci_reclassification <- read.csv("data/aci reclassification.csv")
aci_reclassification <- aci_reclassification %>% dplyr::select(-Old_classification)

## 1. Specify values that correspond to  forest for 2019 and 2020 separately
v <- aci_reclassification %>%
 filter(New_aci_classification == "Closed")
value <- v$Value

## 2. Make all pixels 1 if they contain the right value
na_aci_2019 <- aci_2019
na_aci_2019[na_aci_2019 %in% value] <- 1

na_aci_2020 <- aci_2020
na_aci_2020[na_aci_2020 %in% value] <- 1

## 3. Make all pixels NA if not the right value
na_aci_2019[na_aci_2019 > 1] <- NA
na_aci_2020[na_aci_2020 > 1] <- NA

## 4. Calculate the distance to the forest for each step
dist_rast_2019 <- raster::distance(na_aci_2019)
dist_rast_2020 <- raster::distance(na_aci_2020)


r1_2019$dist_to_forest_end <- ExtractPoints(matrix(c(r1_2019$x2_, r1_2019$y2_), ncol = 2),
                                 raster = dist_rast_2019)
r1_2019$dist_to_forest_start <- ExtractPoints(matrix(c(r1_2019$x1_, r1_2019$y1_), ncol = 2),
                                   raster = dist_rast_2019)


r1_2020$dist_to_forest_end <- ExtractPoints(matrix(c(r1_2020$x2_, r1_2020$y2_), ncol = 2),
                                            raster = dist_rast_2020)
r1_2020$dist_to_forest_start <- ExtractPoints(matrix(c(r1_2020$x1_, r1_2020$y1_), ncol = 2),
                                              raster = dist_rast_2020)

#reclassify extracted raster values
#aci_values <- as.data.frame(c(unique(aci_2019), unique(aci_2020))) %>% distinct() %>% rename(Value = 1)
#cdl_values <- as.data.frame(c(unique(cdl_2019), unique(cdl_2020))) %>% distinct() %>% rename(Value = 1)

r1_total = bind_rows(r1_2019,r1_2020)

r1_total <- left_join(r1_total, aci_reclassification, by = c("aci_end" = "Value"))
names(r1_total)[names(r1_total) == "New_aci_classification"] <- "Cover_end"

r1_total <- left_join(r1_total, aci_reclassification, by = c("aci_start" = "Value"))
names(r1_total)[names(r1_total) == "New_aci_classification"] <- "Cover_start"

summary<- r1_total %>% 
  drop_na(aci_end) %>%
  group_by(IDYr) %>% 
  summarise(n = n())
range(summary$n)
mean(summary$n)


## convert NAs to unavailable habitat
r1_total$Cover_start[is.na(r1_total$Cover_start)] <- "NotAvailable"
r1_total$Cover_end[is.na(r1_total$Cover_end)] <- "NotAvailable"

## check number of fixes by habitat type: 
#r1_total[, .N, by = "Value"]




## assign value to each iteration
r1_total <- r1_total %>% group_by(IDYr, step_id_) %>% mutate(iter = seq_len(n())) %>% ungroup()

saveRDS(r1_total, "output/ZOE/2-clean-all-rdm-N10.RDS")

