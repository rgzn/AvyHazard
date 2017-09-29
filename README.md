# Avalanche Hazard Analysis #

This is code to analyze Ned Bair's avalanche hazard layers for the Sierra Nevada Bighorn Sheep Recovery Project. We wanted to measure the effect of avalanche hazard on the suitability of bighorn habitat in the Sierra. 

In order to measure the relative exposure to avalanche hazard between herd units, we used locations from GPS collared animals to sample the hazard grid. Each collar record was matched with the corresponding hazard value based on location and date. We used at most one record per animal per day. These values were then aggregated by the animals' herd units, and averaged to develop a measure of total yearly hazard exposure by herd. In certain years some herds had no GPS collars so this measure was not applicable. For a herd with such a year, we filled the missing-data year with the herd's mean hazard value from the years with data.

## Data Format: ##

I did not include data files in this repo because they are large. Please contact me if you would would like copies. Here are some notes on the format of data files in case you would like to create your own:
- "AllCollarLocations.Rda" : This is an R dataframe file with the following format:

```
> head(collar.data)
  AnimalID     Date  UTM_E   UTM_N Method    Time DOP SV  Fix            keyfield CollarSerialNo_Date_FK HU RU Sex
1     S167 20130422 379060 4074451     LI 7:00:20 2.0    <NA> 201304227:00:20S167         33212_20121015 Bx  S   F
2     S233 20130422 353009 4145740     LI 7:00:29 1.6    <NA> 201304227:00:29S233         33235_20121015 Wh  C   M
3     S261 20130422 379666 4086350     LI 7:00:30 3.2    <NA> 201304227:00:30S261         33225_20121015 Sw  S   F
4     S266 20130422 389645 4043888     LI 7:00:30 5.6    <NA> 201304227:00:30S266         33229_20121015 Ln  S   F
5      S89 20130422 299242 4211124     LI 7:00:30 2.2    <NA>  201304227:00:30S89         33219_20121015 Wr  N   F
6     S260 20130422 383472 4082053     LI 7:00:36 2.6    <NA> 201304227:00:36S260         33211_20121015 Bx  S   F
```

- hazard data files: These files should be in the ./data/HDF4/ directory. Filename format is as follows:   
     ```hazardWY[YYYY].h5```    
EG the hazard file for water year 2011 is "hazardWY2011.h5". The contents of these files are georeferenced 700x700x365 grids, with a hazard value for each pixel-day. THe code converts these to rasterbrick objects, so if you would like to create your own data, I recommend creating them as rasterbricks to start with, as that would be easier. Each year should have a rasterbrick with the following format:
```
class       : RasterBrick 
dimensions  : 612, 732, 447984, 365  (nrow, ncol, ncell, nlayers)
resolution  : 0.00832, 0.00832  (x, y)
extent      : -122.0456, -115.9554, 34.95376, 40.0456  (xmin, xmax, ymin, ymax)
coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
data source : C:\Users\jweissman\AppData\Local\Temp\RtmpOqiCs8\raster\r_tmp_2017-09-29_144253_9192_52900.grd 
names       :   layer.1,   layer.2,   layer.3,   layer.4,   layer.5,   layer.6,   layer.7,   layer.8,   layer.9,  layer.10,  layer.11,  layer.12,  layer.13,  layer.14,  layer.15, ... 
min values  :         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0,         0, ... 
max values  :  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.000000, ... 
time        : 2014-10-01, 2015-09-30 (min, max)
```


  


### Project Contents: ###
- **collarData2.r** : This is the main analysis file. It processes the hazard data, assigns a hazard value to each GPS collar point, and then aggregates by herd and recovery units. 
- **Constants.r** : Useful constants and parameters specific to our data set.
- **avalancheFunctions.r** : Function definitions used in the analysis. 
- **loadLibraries.r** : Script to load and install required libraries from standard repositories. 

### Requirements: ###
Tested with R version R version 3.4.1 (2017-06-30) on x86_64-w64-mingw32    
Libraries used from [bioconductor](https://bioconductor.org):
- rhdf5

Libraries from the standard cran repo:
- rgdal
- gdalUtils
- rgeos
- maptools
- raster
- rasterVis
- ggmap
- dismo
- sp
- Rcpp
- coda
- bitops
- tmap
- ggplot2
- reshape2
- grid
- gridBase
- gridExtra
- plyr
- dplyr
