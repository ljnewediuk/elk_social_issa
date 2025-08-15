

## Calculate home range overlap of social communites ====
# Authors: Quinn Webber, ..., Eric Vander Wal
# Inputs: Cleaned collar data w/ community membership
# Outputs: list of pairwise UDOI values for communities in a given year

#################################
##### GENERATE HOME RANGES ######
################################ 

### Packages ----
libs <- c('data.table', 'ggplot2', 'rgdal', 
          'spatsoc', 'igraph', 'gridExtra', 'adehabitatHR')
lapply(libs, require, character.only = TRUE)


### Input data ----
DT <- readRDS('output/ZOE/7-locs-mod.RDS')

DT[, .N, by = c("membership", "lagYear")]


## year by membership
DT$IDmodLagYr <- as.character(paste(as.character(DT$animal_ID), 
                              as.character(DT$membership), 
                              as.character(DT$lagYear), sep = "_"))

DT$modLagYr <- as.character(paste(as.character(DT$membership), 
                               as.character(DT$lagYear), sep = "_"))

############################################################
###### Calculate Home range area for each individual #######
###########################################################

utm14N <- '+proj=utm +zone=14 ellps=WGS84'

coords <- c('EASTING', 'NORTHING')

### generate ranges for communities
ptsCommVita <- SpatialPointsDataFrame(DT[, ..coords],
                                  proj4string = CRS(utm14N),
                                  data = DT[, .(IDmodLagYr)])

#### THIS IS FOR THE BHATTACHARYA HOMERANGE OVERLAP ####
#data.xy <- DT[,c('EASTING', 'NORTHING')]
#xysp <- SpatialPoints(data.xy)
#proj4string(xysp) <- CRS("+proj=utm +zone=21 ellps=WGS84")

#Creates a Spatial Data Frame from all locations
#sppt<-data.frame(xysp)

#Creates a spatial data frame of ID
#idsp<-data.frame(DT$IDmodLagYr)
#coordinates(idsp)<-sppt

udCommVita <- kernelUD(ptsCommVita, grid = 700, extent = 7)
verticesVita <- getverticeshr(udCommVita, 95)
dfVita <- fortify(verticesVita)
dfVita$id <- gsub("ER_E_", "VE", dfVita$id)
setDT(dfVita)[, c("ID", "membership", "Year") := tstrsplit(id, "_", fixed=TRUE)]

saveRDS(dfVita, "output/ZOE/Vertices/verticesVita.RDS")

## generate herd-level MCP

ptsCommMCP <- SpatialPointsDataFrame(DT[, ..coords],
                       proj4string = CRS(utm14N),
                       data = DT)

VitaMCP <- mcp(ptsCommMCP, percent = 95)
saveRDS(VitaMCP, "output/ZOE/Vertices/Vita.mcp.RDS")


## Calculate UDOI
KOverVita <- adehabitatHR::kerneloverlap(ptsCommVita,
                                     method = "UDOI",
                                     percent = 95,
                                     grid = 700)

## comparison of social communities 
KOverVita <- as.matrix(KOverVita)
diag(KOverVita) <- NA
KOverVita[lower.tri(KOverVita)] <- NA
UDOIVita <- na.omit(melt(KOverVita))
UDOIVita$Var1 <- gsub("ER_E_", "VE", UDOIVita$Var1)
UDOIVita$Var2 <- gsub("ER_E_", "VE", UDOIVita$Var2)
setDT(UDOIVita)[, c("ID1", "membership1", "Yr1") := tstrsplit(Var1, "_", fixed=TRUE)][, c("ID2", "membership2", "Yr2") := tstrsplit(Var2, "_", fixed=TRUE)][,c("Var1", "Var2") := NULL]
UDOIVita <- UDOIVita[, combo := (ID1==ID2)][combo != "TRUE"] ## remove comparisons of same individuals
UDOIVita <- UDOIVita[, combo := (Yr1==Yr2)][combo != "FALSE"] ## remove comparisons across years
UDOIVita <- UDOIVita[, memb := (membership1==membership2)][, c("combo") := NULL]
UDOIVita$memb[UDOIVita$memb == "TRUE"] <- "Same"
UDOIVita$memb[UDOIVita$memb == "FALSE"] <- "Different"

### Output ----
saveRDS(UDOIVita, 'output/ZOE/9-community-UDOI-overlap.Rds')


############################################################
###### Calculate Home range area for each community  #######
###########################################################

### generate ranges for communities
ptsCommVitaMod <- SpatialPointsDataFrame(DT[, ..coords],
                                      proj4string = CRS(utm14N),
                                      data = DT[, .(modLagYr)])

udCommVitaMod <- kernelUD(ptsCommVitaMod, grid = 700, extent = 7)
kernel.area(udCommVitaMod, unout=("km2"), 95)
verticesVitaMod <- getverticeshr(udCommVitaMod, 95)
dfVitaMod <- fortify(verticesVitaMod)
setDT(dfVitaMod)[, c("membership", "Year") := tstrsplit(id, "_", fixed=TRUE)]
saveRDS(dfVitaMod, "output/ZOE/Vertices/verticesVitaCommunity.RDS")

## Calculate UDOI
KOverVita <- adehabitatHR::kerneloverlap(ptsCommVitaMod,
                                         method = "UDOI",
                                         percent = 95,
                                         grid = 700)

## comparison of social communities 
KOverVita <- as.matrix(KOverVita)
diag(KOverVita) <- NA
KOverVita[lower.tri(KOverVita)] <- NA
UDOIVita <- na.omit(melt(KOverVita))
setDT(UDOIVita)[, c("membership1", "Yr1") := tstrsplit(Var1, "_", fixed=TRUE)][, c("membership2", "Yr2") := tstrsplit(Var2, "_", fixed=TRUE)][,c("Var1", "Var2") := NULL]
UDOIVita <- UDOIVita[, combo := (Yr1==Yr2)][combo != "FALSE"] ## remove comparisons across years

mean(UDOIVita$value)
sd(UDOIVita$value)
range(UDOIVita$value)

UDOIVita$value <- round(UDOIVita$value, digits = 3)

UDOIVita$comm <- paste(UDOIVita$membership1, UDOIVita$membership2, sep = "_")

UDOIVita[, mean(value, na.rm = T), by = "Yr1"]
UDOIVita[, sd(value, na.rm = T), by = "Yr1"]


### Output ----
fwrite(UDOIVita, 'output/ZOE/10-community-UDOI-overlap.csv')


