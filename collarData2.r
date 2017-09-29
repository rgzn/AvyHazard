# collarData2.r
#
# This program samples the daily hazard raster data using GPS
# points from collared animals. 
# It then aggregates data by Herd and Region unit to produce
# comparative statistics.
#
# Jon Weissman 2016
# California Dept. of Fish and Wildlife
# jonathan.weissman@wildlife.ca.gov


################################
# Source Project Files         #
################################
# Libraries:
source('loadLibraries.r')
# Load project constants:
source('Constants.r')
# Load project functions:
source('avalancheFunctions.r')

################################
# Hazard Data Prep             #
################################

# Hazard Data Files
hazardFilenames <- list.files(dataSourceDir, ".h5$")  # h5 files
hazardFiles <- list.files(dataSourceDir, ".h5$", full.names=TRUE) # files with path
years <- as.integer(gsub(".*([0-9]{4}).*", "\\1", hazardFiles)) # data years

################################
# Collar Data Prep             #
################################
load(allCollarDataFile)  # R data file with all animal locations
data.xy <- collar.data[c("UTM_E", "UTM_N")]
coordinates(data.xy) <- ~UTM_E+UTM_N
sppt <- SpatialPointsDataFrame(coordinates(data.xy),collar.data)
rm(collar.data)
rm(data.xy)
sp::proj4string(sppt) <- "+proj=utm +zone=11 +north +ellps=WGS84 +datum=WGS84"
sppt <- spTransform(sppt, CRS("+proj=longlat +datum=WGS84"))

coords <- coordinates(sppt)
colnames(coords) <- c("lon", "lat")
sppt <- cbind(sppt, coords)

sppt@data["WaterYear"] <- -1 # default water year is -1, an invalid year
sppt@data["Hazard"] <- NA
hazardDataNames <- sapply(as.character(years), FUN=function(x) {paste("h",x, sep='')})
sppt@data[hazardDataNames] <- NA

################################
# Main Data Crunch Loop        #
################################

# Main data proceesing loop, runs over years in analysis

for ( year in years) {
  # Select hazard data file by year:
  hazardBrick <- loadHazardYear(year)                         # 1 year of data
  hazardBrick <- hazardBrick[[avySeasonIndex]]                # subset year to avalanche season
  dates <- getZ(hazardBrick)
  yearField <- paste("h", as.character(year), sep = '' )
  for (i in seq_along(dates) ) {
    d <- dates[i]
    dateNum <- as.numeric(format(d, "%Y%m%d"))
    monthDayNum <- as.numeric(format(d, "%m%d"))
    hazardColumn <- extract(hazardBrick[[i]] ,  sppt[sppt$Date == dateNum, ])
    sppt[sppt$Date == dateNum, "Hazard"] <- hazardColumn
    hazardColumn <- extract(hazardBrick[[i]], sppt[ (sppt$Date %% 10000) == monthDayNum, ] )
    sppt[ (sppt$Date %% 10000) == monthDayNum, yearField] <- hazardColumn
    sppt[sppt$Date == dateNum, "WaterYear"] <- year                          
  }
}

# If animal has multiple points in one day, take use only the middle point
sppt2 <- sppt[sppt$WaterYear > 0, ]
sppt2 <- sppt2@data %>%
  group_by(AnimalID, Date) %>%
  slice(ceiling(n()/2)) %>%
  ungroup()
rm(sppt)
colnames(sppt2)[colnames(sppt2) %in% as.character(years)] <- hazardDataNames


# yearFields <- sapply(years, FUN = function(x) { paste("h", as.character(x), sep='')} )
huMeans <- sppt2 %>% 
  group_by(HU, RU) %>%
  summarize_each_(funs(mean(., na.rm = TRUE)), hazardDataNames) 

ruMeans <- sppt2 %>% 
  group_by(RU) %>%
  summarize_each_(funs(mean(., na.rm = TRUE)), hazardDataNames)

huMeansLong <- melt(huMeans, id.vars=c("HU","RU"))
colnames(huMeansLong)[3] <- "WaterYear"
colnames(huMeansLong)[4] <- "MeanHazard"
huMeansLong <- mutate(huMeansLong, WaterYear = as.numeric(sub("h","", WaterYear)))

ruMeansLong <- melt(ruMeans, id.vars=c("RU"))
colnames(ruMeansLong)[2] <- "WaterYear"
colnames(ruMeansLong)[3] <- "MeanHazard"
ruMeansLong <- mutate(ruMeansLong, WaterYear = as.numeric(sub("h","", WaterYear)))


################################
# Plots:                       #
################################
d <- huMeansLong[huMeansLong$HU %in% validHerds, ] 
ggplot(d, aes(x=WaterYear, y=MeanHazard, color=HU)) + 
  geom_line(size=2, alpha=0.5) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ylab("means") + xlab("water year") +
  ggtitle("Mean of Hazard, Sampled by Sheep, Grouped by Herd Unit")

d <- ruMeansLong[ruMeansLong$RU %in% validRegions, ] 
ggplot(d, aes(x=WaterYear, y=MeanHazard, color=RU)) + 
  geom_line(size=2, alpha=0.5) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ylab("means") + xlab("water year") +
  ggtitle("Mean of Hazard, Sampled by Sheep, Grouped by Recovery Unit")

# Melt datqa to 'long' format:
melt(ruMeans, id.vars=c(""))

ggplot(ruMeans, aes(x=WaterYear, y=MeanHazard, color=RU)) + 
  geom_line(size=2, alpha=0.5) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ylab("means") + xlab("water year") +
  ggtitle("Mean of Sheep Points Grouped by Recovery Unit")

d <- huMeans[huMeans$HU %in% validHerds, ] 
ggplot(d, aes(x=WaterYear, y=MeanHazard, color=HU)) + 
  geom_line(size=2, alpha=0.5) + 
  scale_colour_brewer(palette="Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ylab("means") + xlab("water year") +
  ggtitle("Mean of Hazard, Sampled by Sheep, Grouped by Herd Unit")


baseMap <- get_map(location = c(-118.35581, 36.80818), 
                   zoom=10, source="stamen", 
                   maptype="terrain-background")

d <- sppt[sppt$HU=="Bx" & sppt$WaterYear > 0, ]@data
bxMap <- ggmap(baseMap, zoom=12)
bxMap + 
  geom_point(aes(x=lon, y=lat, color=as.factor(WaterYear), group=WaterYear), data=d, alpha=0.1) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("Winter Baxter Points, by year")

d <- sppt[sppt$HU="Bx" & sppt$WaterYear == 2011, ]@data
bxMap + 
  geom_point(aes(x=lon, y=lat, color=as.factor(AnimalID), group=AnimalID), data=d, alpha=0.1) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + hw
ggtitle("Winter 2011 Baxter Points, by sheep")


baseMap2 <- get_map(location = c(-119.1986, 37.84898), 
                    zoom=11, source="stamen", 
                    maptype="terrain-background")

d <- sppt[sppt$HU=="Gb" & sppt$WaterYear > 0, ]@data
GbMap <- ggmap(baseMap2, zoom=14)
GbMap + 
  geom_point(aes(x=lon, y=lat, color=as.factor(WaterYear), group=WaterYear), data=d, alpha=0.1) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("Winter Gibbs Points, by year")

d <- sppt[sppt$HU="Gb" & sppt$WaterYear == 2015, ]@data
GbMap + 
  geom_point(aes(x=lon, y=lat, color=as.factor(AnimalID), group=AnimalID), data=d, alpha=0.1) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("Winter 2015 Gibbs Points, by sheep")


# d <- huMeans[huMeans$HU %in% validHerds, ]




################################
# Tabular Data Output:         #
################################


#############################################


sppt2 <- ddply(sppt[sppt$WaterYear > 0 , ]@data, 
               .(AnimalID, lat, lon, Sex, Date, HU, RU, WaterYear ),
               summarize, MeanHazard = mean(Hazard))

#############################################



sppt2[,1:18]
pointMeans <- ddply(sppt2[sppt2$WaterYear > 0 , ], 
                 .(AnimalID, Sex, Date, HU, RU, WaterYear ),
                 summarize, MeanHazard = mean(Hazard))
huMeans2 <- ddply(pointMeans[pointMeans$HU != "No", ], .(HU, RU, WaterYear), 
                 summarize, MeanHazard = mean(MeanHazard))
ruMeans2 <- ddply(pointMeans[pointMeans$HU %in% validHerds, ], .(RU, WaterYear), 
                 summarize, MeanHazard = mean(MeanHazard))
huTotalMeans = ddply(huMeans2, .(HU, RU), summarize, MeanHazard = mean(MeanHazard))
ruTotalMeans = ddply(ruMeans2, .(RU), summarize, MeanHazard = mean(MeanHazard))



# Raw data as text files:
write.table(huMeans[huMeans$HU != "No", ], "HU_Hazard_SampledAcrossYears.txt", sep="\t", row.names = F)
write.table(ruMeans[ruMeans$RU != "No", ], "RU_Hazard_SampledAcrossYears.txt", sep="\t", row.names = F)

# Fill in empty years with means from respective herd units:
huMeansWide <- dcast(huMeans2, HU + RU ~ WaterYear)
for( i in 1:nrow(huMeansWide)) {
  huMeansWide[i , is.na(huMeansWide[i,])] <- 
    mean( as.numeric(huMeansWide[i,as.character(years)]), na.rm = T)
}
write.table(huMeansWide, "HU_HazardSampled_MeanFilled.txt", sep = "\t", row.names = F)

# Fill in empty years with means from respective recovery units:
ruMeansWide <- dcast(ruMeans2, RU ~ WaterYear)
for( i in 1:nrow(ruMeansWide)) {
  ruMeansWide[i , is.na(ruMeansWide[i,])] <- 
    mean( as.numeric(ruMeansWide[i,as.character(years)]), na.rm = T)
}
write.table(ruMeansWide, "RU_HazardSampled_MeanFilled.txt", sep = "\t", row.names = F)

# data check:
# means over all data for each herd 
huMeans2 %>%
  group_by(HU) %>%
  summarise( groupMean = mean(MeanHazard, na.rm = T))

herds = unique(huMeans2[,"HU"])
for ( herd in herds) {
  dataYears <- huMeans2[huMeans2$HU == herd, "WaterYear"]
  missingYears <- years[!(years %in% dataYears)]
  print ( missingYears)
}

