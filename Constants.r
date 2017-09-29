# Constants and Parameters
# 
# Constants defined here. This file must be changed to fit your data
# 
# Jon Weissman 2016
# California Dept. of Fish and Wildlife
# jonathan.weissman@wildlife.ca.gov



# File Name and Directory Consants
##########################
allCollarDataFile <- "AllCollarLocations.Rda"
rasterDir <- "./data/raster/"
gisDir <- "./gis/"
dataSourceDir <- "./data/HDF5/"
YEAR_ZERO <- 2003
##########################

# Recovery Unit Definitions
##########################
ruSouth <- c('Bx','Ln','Sw','Wl')
ruCentral <- c('Wh')
ruNorth <- c('Gb','Wr')
validHerds <- c( ruSouth, ruCentral, ruNorth)
validRegions <- c('S', 'C', 'N')
##########################

# Geospatial Constants
##########################
# Spatial Coordinates of Hazard Data
sierraCoordinates <- c( left = -122.004,
                        right = -115.996 , 
                        bottom = 34.996 , 
                        top = 40.004)

mapCoordinates <- c( left = -119.2,
                       right = -118.1,
                       bottom = 36.6,
                       top = 38)

# Coordinate Reference System
myCrsStr <- "+proj=longlat +datum=WGS84"
myCrs <- CRS(myCrsStr)
gCrsStr <- "+init=epsg:3857"
gCrs <-  CRS(gCrsStr)

# Conversion Factors
M_PER_FOOT <- 0.3048

##########################

# Graphics Constants
##########################
lavaPalette <- colorRampPalette(c("black", "red4","red"," yellow","white"), alpha=0.35)(n = 100)
lavaPalette2 <- lavaPalette
lavaPalette2[1:10] <- "#FF000000"
hazardPalette <- colorRampPalette(c("black", "red4","red"," yellow","white"), alpha=0.35)(n = 100)
hazardPalette[1:40] <- "#FF000000"
##########################

# Season Index
##########################
avySeasonIndex <- c(15:245)
##########################