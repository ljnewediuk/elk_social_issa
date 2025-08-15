

### All Locs - Cleaning ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 'lubridate','dplyr', 'tidyverse', 'sf')
lapply(libs, require, character.only = TRUE)

### Load data ----

vita_elk <- readRDS("data/vita_elk_vectronic_feb_2019-march_2021_cleaned.rds")
calving <- readRDS("data/calving_dates.rds")

# Add in column for year
vita_elk$Year <-  format(as.Date(vita_elk$time_utc, format="%Y-%m-%d %H:%M"),"%Y")

# Add in column for Julian Date
vita_elk$JDate <- yday(vita_elk$time_utc)

### ID by Yr
vita_elk$IDYr <- paste(vita_elk$animal_ID, vita_elk$Year, sep = "_")

### Add days from  as "year" 24th Feb - this creates 2 full years of data and some left over in March 2021

vita_elk$lagYear <- ifelse(vita_elk$Year %in% c("2019"), 'Year1',
                          ifelse(vita_elk$Year %in% c("2020") & vita_elk$JDate <= 54, 'Year1',
                                 ifelse (vita_elk$Year %in% c("2020") & vita_elk$JDate >= 55, 'Year2',
                                      ifelse (vita_elk$Year %in% c("2021") & vita_elk$JDate <= 54, 'Year2',
                                   'Year3'))))
                            
### ID by lag Yr
vita_elk$IDLagYr <- paste(vita_elk$animal_ID, vita_elk$lagYear, sep = "_")


#Rename datetime column
names(vita_elk)[names(vita_elk) == "dat_time"] <- "datetime"

#Add in column for season (Summer vs. winter), summer is May 1st to August 31st, Winter is the rest of the year
vita_elk$Season <- ifelse(vita_elk$Year %in% c("2020") & vita_elk$JDate >= 122 & vita_elk$JDate <= 244, 'Summer', 
                          ifelse (vita_elk$Year %in% c("2019","2021") & vita_elk$JDate >= 121 & vita_elk$JDate <= 243, 'Summer',
                          'Winter'))

#Add in column for pre- and post calving
calving <- calving %>% dplyr::select(animal_ID, calved)
vita_elk <- left_join(vita_elk, calving, by = c("IDYr" = "animal_ID"))
vita_elk$calved [vita_elk$Season == "Winter"] <- NA
vita_elk$calving <- ifelse(vita_elk$JDate >= vita_elk$calved, 'Post-calving',
                           ifelse (vita_elk$JDate < vita_elk$calved, 'Pre-calving',
                                   NA))
vita_elk <- vita_elk %>% dplyr::select(!calved)

### Projection ----

elk_dat_utm <- st_transform(vita_elk, crs = st_crs(26914))

XY_dat <- elk_dat_utm %>%
  cbind(st_coordinates(elk_dat_utm))

XY_dat<- rename(XY_dat, EASTING = X, NORTHING = Y)

## Export data
saveRDS(XY_dat, 'output/ZOE/1-clean_vita_elk.Rds')


message('=== PREP COMPLETE ===')



