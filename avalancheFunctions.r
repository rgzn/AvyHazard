# Function definitions
# 
# This file contains useful functions definitions used
# elsewhere in our avalanche hazard analyses.
#
# Jon Weissman 2016
# California Dept. of Fish and Wildlife
# jonathan.weissman@wildlife.ca.gov


# some of these require the constants defines in Constants.R
source('Constants.r')

# nPixels
# Find the number of valued pixels in a raster object
# arguments: Raster Object
# returns: integer # of pixels
nPixels <- function(raster) {
  nValidPixels <- sum(!is.na(as.matrix(raster)))
  return(nValidPixels)
}

# areaOfBrick
# estimates area of raster brick
#   ignoring NA values, 
#   and assuming the same pixels
#   are NA throughout brick
areaOfBrick <- function(brick) {
  if ( class(brick) != "RasterBrick") {
    ERROR_MESSAGE = "Argument is not a RasterBrick"
    stop(ERROR_MESSAGE)
  }
  areas <- area(brick[[1]], na.rm=TRUE)
  area <- cellStats(areas, sum)
  return(area)
}

# midLatitude
# Input a raster* object
#   and return mean of the latitude extent.
midLatitude <- function(raster) {
  return(mean(extent(raster)[3:4]))
}

# midCoords <- function(raster) {
#   midLat = mean(extent(raster)[3:4])
#   midLon = mean(extent(raster)[1:2])
#   return(
# }

# fractionOverThreshold
# Find the fraction of pixels in a raster above a certain threshold 
# arguments: 
#   raster: a raster layer to analyze
#   threshold: a threshold value to compare pixel values to
# returns: a real valued ratio 
fractionOverThreshold <- function(raster, threshold) {
  valueMatrix <- as.matrix(raster)
  nValues <- sum(!is.na(valueMatrix))
  nOverThreshold <- sum(!is.na(valueMatrix[valueMatrix > threshold]))
  return(nOverThreshold/nValues)
}


# gUnionByList
# applies the gUnion function to a list of polygons
# arguments: 
#   - polyList: list of spatial polygon objects
#   - gUnion arguments: see rgeos::gUnion
# returns: 
#   sp object that is the union of input polygons
gUnionByList <- function(polyList, 
                         byid=FALSE, 
                         id=NULL, 
                         drop_lower_td=FALSE,
                         unaryUnion_if_byid_false=TRUE,
                         checkValidity=FALSE) {
  nPolys <- length(polyList)
  if (nPolys < 1) stop ('less than one object in list')
  else if (nPolys == 1) {
    if ( attr(class(polyList[[1]]),"package") == "sp") return(polyList[[1]])
    else stop('single list object not from sp spatial package')
  }
  unionPoly <- polyList[[1]]
  for ( i in 2:nPolys) {
    unionPoly <- gUnion(unionPoly, polyList[[i]], 
                        byid, id, drop_lower_td, 
                        unaryUnion_if_byid_false, checkValidity)
  }
  return(unionPoly)
}

# filterRaster
# applies a filter to a raster object
filterRaster <- function( dataRaster, filterRaster) {
  croppedFilterRaster <- crop(filterRaster, dataRaster)
  maskedDataRaster <- mask(dataRaster, croppedFilterRaster)
  return(maskedDataRaster)
}

# loadHazardYear
# This function loads a year's hazard data file into a raster object
# This function is messy and ugly. Mostly for readability.
# arguments: 
#   - year: integer water year to pick data file
#   - dir: directory in which data resides
#   - extension: regular expression to denote the file type
#   - startYear: first year of data to be considered. 
# returns: hazardBrick: a raster brick object full of a year's data.
loadHazardYear <- function(year, 
                           dir='data/HDF5',
                           extension='.h5$',
                           startYear=2004,
                           dataCrs='+proj=longlat +datum=NAD83',
                           projectCrs=myCrsStr) {
  hazardFilenames <- list.files(dir,extension)
  hazardFiles <- list.files(dir, extension, full.names=TRUE)
  years <-  seq(length(hazardFiles)) + startYear-1
  hazardFileFrame <- data.frame(years, hazardFilenames, hazardFiles, stringsAsFactors=FALSE)
  dataFile <- hazardFileFrame[hazardFileFrame$years == year,'hazardFiles']
  hazardCube <- h5read(dataFile, 'Grid/hazard_cube')  # load h5 file
  hazardBrick <- brick(hazardCube,  
                       xmn = sierraCoordinates['left'], 
                       xmx = sierraCoordinates['right'], 
                       ymn = sierraCoordinates['bottom'], 
                       ymx = sierraCoordinates['top'],
                       crs=dataCrs )
  if (dataCrs != projectCrs) {
    hazardBrick <- projectRaster(hazardBrick, crs=projectCrs)
  }
  
  rm(hazardCube)   # unload file

  # Generate the dates corresponding to the hazardBrick
  #   The water year starts october 1
  startDate <- as.Date(paste(as.character(year-1),"-10-1",sep=''))
  dates <- seq(startDate,by='days',length=nlayers(hazardBrick))
  hazardBrick <- setZ(hazardBrick, dates) # add dates as a Z dimension
  
  return(hazardBrick)
}

# hazardHF52Rasters
# One-off function to convert files into native raster format
# Using this format it is faster to load an unload
#   large raster bricks
# hazardHD52Rasters <- function() {
#   hazardRasters <- list()
#   for (year in years) {
#     hazardBrick <- loadHazardYear(year)
#     filename <-
#       paste(rasterDir, "hazardBrick", as.character(year), ".tif",sep = '')
#     writeRaster(hazardBrick, filename, format='GTiff')
#     rm(hazardBrick)
#     hazardRasters[as.character(year)] <- filename
#   }
# }

# generateBricksForRegions
# generates individual raster bricks for a list of spatial polygons
#   that are within the extent of the a larger raster brick
# arguments: 
#   polyList: list of spatial polygons to define raster boundaries
#   rawBrick: raster brick object with all the raw data
# returns: 
#   brickList: list of raster bricks
generateBricksForRegions <- function(polyList, rawBrick) {
  brickList <- list()
  for (i in 1:length(polyList)) {
    brickList[names(polyList[i])] <-
      mask(crop(rawBrick,polyList[[i]]), polyList[[i]])
  }
  return(brickList)
}

# generateElevationFilter
# generates a raster filter based on elevation
# inputs: 
#   - elevationFile: raster of elevations
#   - alignmentRaster: raster object to serve as reference for extent/resolution
#   - alignmenFile: raster file that is used for extent/resolution/crs
#   - minElevation: elevations below this will be excluded
#   - maxElevation: elevations above this will be excluded
# returns:
#   elevationFilterRaster: raster layer with value 1 or NA based on elevation
generateElevationFilter <- function(elevationFile = 'gis/elev_hdr.tif',
                                    alignmentRaster = NULL,
                                    alignmentFile = 'gis/exampleAvyRaster.tif',
                                    minElevation = -Inf,
                                    maxElevation = Inf) {
  if (!is.null(alignmentRaster)) {
    writeRaster(
      alignmentRaster, filename = alignmentFile,format = "GTiff",overwrite = T
    )
  }
  elevation <- align_rasters(
    unaligned = elevationFile,
    reference = alignmentFile,
    dstfile = "gis/elevationAligned.tif",
    output_Raster = TRUE,
    verbose = TRUE ,
    r = "average"
  )
  elevationFilterRaster <- calc(elevation, function(x) 
    ifelse(x > minElevation, TRUE, NA)*ifelse(x < maxElevation, TRUE, NA))
  return(elevationFilterRaster)
}


brickListStats <- function(brickList, statArg, na.rm.Arg=T) {
  statList <- lapply(brickList, FUN=cellStats, stat=statArg, na.rm=na.rm.Arg)
  statList <- lapply(statList, statArg)
  return(statList)
}


# brickList2ScalarList
# This is a generic function to aggregate data in a list of raster bricks.
# Arguments: 
#     brickList: list of raster bricks
#     statFun: by-layer integration function. Defaults to mean
#     raster2scalarFun: within-layer integration function
# Returns: 
#   scalarList: list of scalar values, one for each brick in brickList
brickList2ScalarList <- function(brickList,
                                 statFun=  function(x){ mean(x,na.rm=T)},
                                 raster2scalarFun, ...) {
  scalarList <- list()
  for (i in 1:length(brickList)) {
    brick <- brickList[[i]]
    y = vector()
    for (j in 1:nlayers(brick)) {
      y[j] <- raster2scalarFun( brick[[j]], ...)
    }
    scalarList[[i]] <- y
  }
  scalarList <- lapply(scalarList, statFun)
  if (!is.null(names(brickList))) {
    names(scalarList) <- names(brickList)
  }
  return(scalarList)
}

# meanFOTofBrickList
# Apply the fraction-over-threshold stat to a brick list
# Generates a list of meanFOT stats for each brick. 
meanFOTofBrickList <- function(brickList, thresholdArg) {
  overThresholdList <- list()
  for(brickName in names(brickList)) {
    y = vector()
    for (i in 1:nlayers(brickList)) {
      y[i] <- fractionOverThreshold(brickList[[brickName]][[i]], threshold=thresholdArg)
    }
    overThresholdList[[brickName]] <- y
  }
  meanFOTList <- lapply(overThresholdList, mean)
  return(meanFOTList)
}

# rasterApply
# Apply an input function FUN each raster layer
#   in a raster brick. 
# Returns output for each layer in a list.
brickApply <- function(brick, FUN, ...) {
  b <- list()
  for (i in 1:nlayers(brick)) {
    b[[i]]  <- FUN(brick[[i]], ...)
  }
  return(b)
}


