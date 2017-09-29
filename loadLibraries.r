# loadLibraries.r
#
# This script loads the packages required by the rest of the project
#
# Jon Weissman 2016
# California Dept. of Fish and Wildlife
# jonathan.weissman@wildlife.ca.gov


################################
# Load Bioconductor Libraries  #
################################
# Uncomment these if you have not installed Bioconductor:

# source("https://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

################################
# Load Standard Libraries      #
################################
# This section loads the packages necessary to run the data processing
libraries <- c("rgdal",
               "gdalUtils",
               "rgeos",
               "maptools",
               "raster",
               "rasterVis",
               "ggmap",
               "dismo",
               "sp",
               "Rcpp",
               "coda",
               "bitops",
               "tmap", 
               "ggplot2",
               "reshape2", 
               "grid",
               "gridBase",
               "gridExtra",
               "plyr",
               "dplyr"
               # "animation"
)

# Install Packages that are not already installed:
for (i in 1:length(libraries) ) {
  if ( !require(libraries[i], character.only=TRUE) ) {
    install.packages(libraries[i])
  }
}

# Packages from the bioconductor repo:
if ( !require("rhdf5", character.only=TRUE) ) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("rhdf5")
}

# Load the libraries:
lapply(libraries, library, character.only=TRUE)

