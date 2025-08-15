
## Cleaned Locs - Calculate Annual Community Assignment ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/o rdm points
# Outputs: Individual annual community assignment
### Packages ----
libs <- c('data.table','rgdal', 'ggplot2', 'tidyverse', 'rnaturalearth', 'gganimate', 'gifski',
          'spatsoc', 'igraph', 'asnipe', 'sf', 'dplyr', 'tidyr', 'png', 'stars', 'ggmap')
lapply(libs, require, character.only = TRUE)

### Input raw data ----
D <- readRDS("output/ZOE/1-clean_vita_elk.Rds")
D <- (D %>% st_drop_geometry())
DT <- D %>% dplyr::filter(lagYear != 'Year3')
DT <- setDT(DT)
DT[, .N, by = "IDYr"]

Summer <- DT[Season == "Summer"]

###### GENERATE NETWORKS FOR OBSERVED DATA ######

DT <- group_times(DT, datetime = 'datetime', threshold = '5 minutes')

DT <- group_pts(
  DT,
  threshold = 50,
  splitBy = c('lagYear'),
  timegroup = 'timegroup',
  id = 'animal_ID',
  coords = c('EASTING', 'NORTHING')
)

DT[, .N, by = "IDYr"]


#Set colour palette
c18 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99",
  "gray70",
  "maroon", "deeppink1", "blue1", "steelblue4", "yellow4", "yellow3", "brown", "dark green"
)
pie(rep(1, 18), col = c18)

ggplot(DT) +
  geom_point(aes(EASTING, NORTHING, color = animal_ID), size = 0.1) +
  facet_wrap(~lagYear)+
  scale_color_manual(values = c18)+
  theme_minimal()+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))


### Calculate network metrics
source("functions/dynamic_network2.R")

mods <- dynamic_network(DT, id = 'animal_ID', 
                        by = c('lagYear'))
saveRDS(mods, "output/ZOE/6-network-stats.RDS")

#mods$ID1 <- mods$ID
#mods$Year[mods$lagYear == "Year1"] <- "2019"
#mods$Year[mods$lagYear == "Year2"] <- "2020"
mods$IDLagYr <- as.factor(paste(mods$ID, mods$lagYear, sep = "_"))
mods2 <- mods[,c("IDLagYr", "membership")]

### merge community assignment to DT file

DT2 <- merge(mods2, DT, by = "IDLagYr")

saveRDS(DT2, "output/ZOE/7-locs-mod.RDS")

##### GENERATE NETWORKS FOR SUMMER ONLY #####
Summer <- group_times(Summer, datetime = 'datetime', threshold = '5 minutes')

Summer <- group_pts(
  Summer,
  threshold = 50,
  splitBy = c('lagYear'),
  timegroup = 'timegroup',
  id = 'animal_ID',
  coords = c('EASTING', 'NORTHING')
)

Summer[, .N, by = "IDYr"]

ggplot(Summer) +
  geom_point(aes(EASTING, NORTHING, color = animal_ID), size = 0.1) +
  facet_wrap(~lagYear)+
  scale_color_manual(values = c18)+
  theme_minimal()+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))


### Calculate network metrics
mods_summer <- dynamic_network(Summer, id = 'animal_ID', 
                        by = c('lagYear'))
saveRDS(mods_summer, "output/ZOE/6-network-stats-summer-only.RDS")

#mods_summer$ID1 <- mods_summer$ID
#mods_summer$Year[mods_summer$lagYear == "Year1"] <- "2019"
#mods_summer$Year[mods_summer$lagYear == "Year2"] <- "2020"
mods_summer$IDLagYr <- as.factor(paste(mods_summer$ID, mods_summer$lagYear, sep = "_"))
mods2_summer <- mods_summer[,c("IDLagYr", "membership")]

### merge community assignment to DT file

Summer2 <- merge(mods2_summer, Summer, by = "IDLagYr")

saveRDS(Summer2, "output/ZOE/7-locs-mod-summer-only.RDS")




##### Animated graph #####

# compute daily average positions and speeds
D$Date <- format(as.Date(D$datetime, format="%Y-%m-%d %H:%M"),"%Y-%m-%d")
D_ave = D %>%
  group_by(animal_ID,Date) %>%
  summarise(
    lat= mean(lat, na.rm = TRUE), 
    long = mean(long, na.rm = TRUE),
  ) 
D_ave$Date<- as.Date(D_ave$Date)


# create 'ideal' data with all combinations of data
ideal = expand_grid(
  animal_ID = unique(D_ave$animal_ID),
  Date = seq.Date(from = min(D_ave$Date), to = max(D_ave$Date), by = 1)
)

# create complete dataset
df_all = left_join(ideal,D_ave)
df_all$animal_ID <- gsub("ER_E_", "Elk ", df_all$animal_ID)

#load background map
bg <- get_stamenmap(bbox = c(left = min(df_all$long, na.rm = TRUE)-0.05, 
                                    bottom = min(df_all$lat, na.rm = TRUE)-0.05, 
                                    right = max(df_all$long, na.rm = TRUE)+0.05, 
                                    top = max(df_all$lat, na.rm = TRUE)+0.05), 
                           zoom = 13, maptype = "terrain", crop = FALSE)


p = ggmap(bg)+
  
  # lines and points
  geom_path(data = df_all, 
            aes(x=long,y=lat,group=animal_ID,color=animal_ID), 
            alpha = 0.3)+
  geom_point(data = df_all, 
             aes(x=long,y=lat,group=animal_ID,fill=animal_ID),
             alpha = 0.7, shape=21, size = 2)+
  
  # formatting
  scale_size_continuous(range = c(0.1,10))+
  scale_fill_manual(values = c18)+
  scale_color_manual(values = c18)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank())
p




# animate

anim = p + 
  transition_reveal(along = Date)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

animate(anim, nframes = length(unique(df_all$Date)), fps = 5)
anim_save("graphics/elk movement.gif", anim)

library(leaflet)
m <- leaflet(data = df_all) %>% setView(lng = ((min(df_all$long, na.rm = TRUE))+(max(df_all$long, na.rm = TRUE)))/2,
                           lat = ((min(df_all$lat, na.rm = TRUE))+(max(df_all$lat, na.rm = TRUE)))/2,
                           zoom = 12)%>% 
  addTiles() %>%
  addMarkers(~long, ~lat)
m
