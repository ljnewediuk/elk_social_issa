#Bits of code I cut out

##### Prepping the LC_2015 layer #####
# Crop LC_2015 to the data and reproject to UTM14
#r1_sf <- r1 %>% 
#  select('x2_', 'y2_')%>%
#  st_as_sf(coords = c('x2_', 'y2_'), crs = crs)

LC_2019 <- raster("data/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.ige")
#crs_leae <- crs(LC_2015)
#r1_leae<- st_transform(r1_sf, crs = crs_leae) 
#r1_leae_df <- r1_leae %>%
#  cbind(st_coordinates(r1_leae))
#r1_leae_df <- r1_leae_df %>% st_drop_geometry()

#xmin <- min(r1_leae_df$X)-2000
#xmax <- max(r1_leae_df$X)+2000
#ymin <- min(r1_leae_df$Y)-2000
#ymax <- max(r1_leae_df$Y)+2000

#e <- as(extent(xmin, xmax, ymin, ymax), 'SpatialPolygons')
#crs(e) <- crs(LC_2015)
#r <- crop(LC_2015, e)
#plot(r)
#LC_2015_UTM <- projectRaster(r, crs = crs(aci_2019))
#writeRaster(LC_2015_UTM , "data/north_america_2015_v2/NA_NALCMS_2015_v2_land_cover_30m/NA_NALCMS_2015_v2_land_cover_30m_UTM.tif", format="GTiff")



######Google Earth engine #######

#Filtered_Open_closed_sf <- Filtered_Open_closed %>%  
#select('x2_', 'y2_', 'Year')%>%
#st_as_sf(coords = c('x2_', 'y2_'), crs = crs)

#Filtered_Open_closed_sf_2019 <- filter(Filtered_Open_closed_sf, Year == "2019")
#Filtered_Open_closed_sf_2020 <- filter(Filtered_Open_closed_sf, Year == "2020")





# Check for clashing land classifications
checking_combos <- r1_total %>%
  group_by(New_cdl_classification, New_aci_classification) %>%
  summarize(count = n())

#Investigate clashing land classifications using rgee

Filtered_Open_closed <- filter(r1_total, r1_total$New_aci_classification == 'Closed', r1_total$New_cdl_classification == 'Open')
Filtered_Closed_open <- filter(r1_total, r1_total$New_aci_classification == 'Open', r1_total$New_cdl_classification == 'Closed')
clashing_pts <- rbind(Filtered_Open_closed,Filtered_Closed_open)
clashing_classifications <- clashing_pts%>%group_by(aci,cdl)%>%dplyr::mutate(Freq=n())
clashing_pts_2019 <- filter(clashing_pts, Year == 2019)
clashing_pts_2020 <- filter(clashing_pts, Year == 2020)

# Map the clashing points
E <- as(extent(661560, 698070, 5420000, 5435000), 'SpatialPolygons')
crs(E) <- crs(aci_2019)
r <- crop(aci_2019, E)
q<- crop (cdl_2019, E)

library(leaflet)
library(mapview)


aci_pal <- colorNumeric("RdBu", values(r))
cdl_pal <- colorNumeric("Blues", values(q))


map_2019 <- leaflet(data = clashing_pts_2019) %>%
  # Base groups
  addRasterImage(r, colors = aci_pal, opacity = 1, group = "aci") %>%
  addLegend(position="bottomright", pal=aci_pal, values=values(r),title = "aci",group = "aci", layerId = "aci") %>% 
  
  addRasterImage(q, colors = cdl_pal, opacity = 1, group = "cdl") %>%
  addLegend(position="bottomright",pal=cdl_pal,values = values(q),title = "cdl",group = "cdl",layerId = "cdl") %>% 
  
  addCircles(~'_x2', ~'_y2', stroke = F, group = "points", fillOpacity = 1) %>%
  
  addLayersControl(
    baseGroups = c("aci", "cdl"),
    overlayGroups = c("points"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  htmlwidgets::onRender("
    function() { 
      var map = this;
      var legends = map.controls._controlsById;
      function addActualLegend() {
         var sel = $('.leaflet-control-layers-base').find('input[type=\"radio\"]:checked').siblings('span').text().trim();
         $.each(map.controls._controlsById, (nm) => map.removeControl(map.controls.get(nm)));
         map.addControl(legends[sel]);
      }
      $('.leaflet-control-layers-base').on('click', addActualLegend);
      addActualLegend();
   }") 


map_2019



checking_combos <- r1_total %>%
  group_by(Second_aci_classification, Second_classification) %>%
  summarize(count = n())
Filtered_Open_closed <- filter(r1_total, r1_total$Second_aci_classification == 'Closed', r1_total$Second_classification == 'Mixed')
Filtered_Closed_open <- filter(r1_total, r1_total$New_aci_classification == 'Open', r1_total$New_cdl_classification == 'Closed')

clashing_pts <- rbind(Filtered_Open_closed,Filtered_Closed_open)
clashing_classifications <- clashing_pts%>%group_by(aci,cdl)%>%summarise(n = n())

