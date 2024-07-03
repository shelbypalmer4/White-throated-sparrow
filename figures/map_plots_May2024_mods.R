require(devtools)
library(sf)
library(terra)
library(scales)
library(sp)
#install.packages(c("maps", "mapproj", "mapdata", "rgeos", "maptools", "raster"))
library(raster)
library(geodata)
install.packages("remotes")
library(remotes)

install_version("geodata", "0.5-9")

library(geodata)
##getData function is from raster package, which works Nov 9 2023
##suggested replacement is to use geodata package
# can0<-getData('GADM', country="CAN", level=0) # Canada
setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-Sparrow/figures/")
#can1<-getData('GADM', country="CAN", level=1) # provinces

# can2<-getData('GADM', country="CAN", level=2) # counties
# can0 <- vect("gadm41_CAN_shp/gadm41_CAN_0.shp")
# can1 <- vect("gadm41_CAN_shp/gadm41_CAN_1.shp")
# can2 <- vect("gadm41_CAN_shp/gadm41_CAN_2.shp")  
# can3 <- vect("gadm41_CAN_shp/gadm41_CAN_3.shp")
# 
# can <- vect(can0, can1, can2, can3)
can <- gadm(country = "Canada", level = 1, resolution = 2,
                  path = ".")

# us1 <- getData('GADM', country="USA", level=1)
# us2 <- getData('GADM', country="USA", level=2)
# us0 <- vect("gadm41_USA_shp/gadm41_USA_0.shp")
# us1 <- vect("gadm41_USA_shp/gadm41_USA_1.shp")
# us2 <- vect("gadm41_USA_shp/gadm41_USA_2.shp")
us <- gadm(country = "USA", level = 1, resolution = 2,
           path = ".")
#us <- c(us0, us1, us2)
mex <- gadm(country = "Mexico", level = 1, resolution = 2,
           path = ".")

## Specify a geographic extent for the map
## by defining the top-left and bottom-right geographic coordinates
mapExtent <- rbind(c(-140, 70), c(-64, 21))

## Specify the required projection using a proj4 string
## Use http://www.spatialreference.org/ to find the required string
## Polyconic for North America
##Following line worked Nov 9 2023
#newProj <- CRS("+proj=poly +lat_0=0 +lon_0=-100 +x_0=0 
#            +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## Project the map extent (first need to specify that it is longlat) 
##Following line worked Nov 9 2023
# mapExtentPr <- spTransform(SpatialPoints(mapExtent, 
#                                          proj4string=CRS("+proj=longlat")),
#                            newProj)

# mapExtentPr <- st_as_sf(mapExtentPr)
# 
# ## Project other layers
# canPr <- spTransform(can, newProj)
# usPr <- spTransform(us, newProj) 


# can <- st_as_sf(can)
# us <- st_as_sf(us)


##Convert sp package objects to sf package objects

the_maps <- read.csv(file = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/clustering_for_maps.csv")
##Remove 33 rows of data which have not been georeferenced, mostly recordings from our lab
the_maps <- the_maps[complete.cases(the_maps$Longitude),]

the_maps$jitter.Latitude <- jitter(the_maps$Latitude, amount = 0.25)
the_maps$jitter.Longitude <- jitter(the_maps$Longitude, amount = 0.25)

coordinates(the_maps) <- ~ jitter.Longitude+jitter.Latitude
crs(the_maps) <- CRS("+proj=longlat +datum=WGS84")
#the_maps.projected <- spTransform(the_maps, newProj)

#the_maps.projected <- st_as_sf(the_maps.projected)

the_maps_2015on <- the_maps[which(the_maps$Year > 2014),]
the_maps_20102014 <- the_maps[which(the_maps$Year < 2015 & the_maps$Year > 2009),]
the_maps_20052009 <- the_maps[which(the_maps$Year < 2010 & the_maps$Year > 2004),]
the_maps_2004_and_earlier <- the_maps[which(the_maps$Year < 2005),]


#the_maps.projected_2014_and_earlier <- the_maps.projected[which(the_maps.projected$Year <= 2014),]

#pal = colorRampPalette(c("blue", "red"))

# points(the_maps.projected_2015on, pch = 16, cex = 2, col = rgb(red = 1- the_maps.projected_2015on$min_max_ratio/max(the_maps.projected_2015on$min_max_ratio), green = 0, blue = the_maps.projected_2015on$min_max_ratio/max(the_maps.projected_2015on$min_max_ratio)))

# plot(mapExtentPr, pch=NA)
# plot(can1Pr, border="black", col="white", add=TRUE)
# plot(us1Pr, border="black", col="white", add=TRUE)

  
# points(the_maps.projected_2014_and_earlier, pch = 16, cex = 2, col = rgb(red = 1- the_maps.projected_2014_and_earlier$min_max_ratio/max(the_maps.projected_2014_and_earlier$min_max_ratio), green = 0, blue = the_maps.projected_2014_and_earlier$min_max_ratio/max(the_maps.projected_2014_and_earlier$min_max_ratio)))


setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures/")
#par(mar = c(5.1,2.1,4.1,2.1))
#dev.new(width=14, height=7, unit="in", noRStudioGD = TRUE)
#layout(matrix(1:3,ncol=3), width = c(3,3,1), height = c(1,1,1))


palette(alpha(c("#37a8b7", "#FE9929", "#AE017E"), 0.8))
##Rhythm plots
png(filename = "pre-2005_poetry", width = 7, height = 7, units = "in", res = 300)
plot(mapExtent, pch=NA, xlab = "", ylab = "")
plot(can, border="black", col="white", add=TRUE)
plot(us, border="black", col="white", add=TRUE)
plot(mex, border="black", col="white", add=TRUE)
points(the_maps_2004_and_earlier, pch = 16, cex = 1.2, col = the_maps_2004_and_earlier$poetry)
dev.off()

png(filename = "2005-2009_poetry", width = 7, height = 7, units = "in", res = 300)
plot(mapExtent, pch=NA, xlab = "", ylab = "")
plot(can, border="black", col="white", add=TRUE)
plot(us, border="black", col="white", add=TRUE)
plot(mex, border="black", col="white", add=TRUE)
points(the_maps_20052009, pch = 16, cex = 1.2, col = the_maps_20052009$poetry)
dev.off()

png(filename = "2010-2014_poetry", width = 7, height = 7, units = "in", res = 300)
plot(mapExtent, pch=NA, xlab = "", ylab = "")
plot(can, border="black", col="white", add=TRUE)
plot(us, border="black", col="white", add=TRUE)
plot(mex, border="black", col="white", add=TRUE)
points(the_maps_20102014, pch = 16, cex = 1.2, col = the_maps_20102014$poetry)
dev.off()

png(filename = "post-2014", width = 7, height = 7, units = "in", res = 300)
plot(mapExtent, pch=NA, xlab = "", ylab = "")
plot(can, border="black", col="white", add=TRUE)
plot(us, border="black", col="white", add=TRUE)
plot(mex, border="black", col="white", add=TRUE)
points(the_maps_2015on, pch = 16, cex = 1.2, col = the_maps_2015on$poetry)
dev.off()

# colfunc <- colorRampPalette(c("blue", "red"))
# legend_image <- as.raster(matrix(colfunc(20), ncol=1))
# png(filename = "map_legend.png", width = 3, height = 7, units = "in", res = 300)
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Onset interval ratio')
# text(x=1.5, y = seq(0,1,l=5), labels = round(seq(1,max(the_maps.projected_2015on$max_min_ratio),l=5), digits = 1))
# rasterImage(legend_image, 0, 0, 1,1)
# dev.off()

### three note strophes only

the_maps <- read.csv(file = "/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/the_maps.csv")
##Remove 33 rows of data which have not been georeferenced, mostly recordings from our lab
# the_maps_threenote <- the_maps[which(the_maps$min_max_ratio > 0.68),]
# the_maps_threenote <- the_maps_threenote[complete.cases(the_maps_threenote$Longitude),]
# 
# coordinates(the_maps_threenote) <- ~Longitude+Latitude
# crs(the_maps_threenote) <- CRS("+proj=longlat +datum=WGS84")
# the_maps_threenote.projected <- spTransform(the_maps_threenote, newProj)
# 
# the_maps_threenote.projected <- st_as_sf(the_maps_threenote.projected)


#the_maps_threenote.projected_2015on <- the_maps_threenote.projected[which(the_maps_threenote.projected$Year > 2014),]
#pal = colorRampPalette(c("blue", "red"))

# points(the_maps.projected_2015on, pch = 16, cex = 2, col = rgb(red = 1- the_maps.projected_2015on$min_max_ratio/max(the_maps.projected_2015on$min_max_ratio), green = 0, blue = the_maps.projected_2015on$min_max_ratio/max(the_maps.projected_2015on$min_max_ratio)))

# plot(mapExtentPr, pch=NA)
# plot(can1Pr, border="black", col="white", add=TRUE)
# plot(us1Pr, border="black", col="white", add=TRUE)
#the_maps_threenote.projected_2014_and_earlier <- the_maps_threenote.projected[which(the_maps_threenote.projected$Year <= 2014),]

# points(the_maps.projected_2014_and_earlier, pch = 16, cex = 2, col = rgb(red = 1- the_maps.projected_2014_and_earlier$min_max_ratio/max(the_maps.projected_2014_and_earlier$min_max_ratio), green = 0, blue = the_maps.projected_2014_and_earlier$min_max_ratio/max(the_maps.projected_2014_and_earlier$min_max_ratio)))


setwd("/Users/mcentee_lab_2/Documents/GitHub/White-throated-sparrow/figures/")
par(mar = c(5.1,2.1,4.1,2.1))
#dev.new(width=14, height=7, unit="in", noRStudioGD = TRUE)
#layout(matrix(1:3,ncol=3), width = c(3,3,1), height = c(1,1,1))


png(filename = "threenote-pre-2015", width = 7, height = 7, units = "in", res = 300)
plot(mapExtentPr, pch=NA)
plot(can1Pr, border="black", col="white", add=TRUE)
plot(us1Pr, border="black", col="white", add=TRUE)
points(the_maps_threenote.projected_2014_and_earlier, pch = 16, cex = 2, col = rgb(red = the_maps_threenote.projected_2014_and_earlier$mid_to_long_ratio - .42, green = 0, blue = .58 - (the_maps_threenote.projected_2014_and_earlier$mid_to_long_ratio - .42), maxColorValue = .58))
dev.off()

png(filename = "threenote-post-2014", width = 7, height = 7, units = "in", res = 300)
plot(mapExtentPr, pch=NA)
plot(can1Pr, border="black", col="white", add=TRUE)
plot(us1Pr, border="black", col="white", add=TRUE)
points(the_maps_threenote.projected_2015on, pch = 16, cex = 2, col = rgb(red = the_maps_threenote.projected_2015on$mid_to_long_ratio - .42, green = 0, blue = .58 - (the_maps_threenote.projected_2015on$mid_to_long_ratio - .42), maxColorValue = .58))
dev.off()

colfunc <- colorRampPalette(c("blue", "red"))
legend_image <- as.raster(matrix(colfunc(20), ncol=1))
png(filename = "map_legend.png", width = 3, height = 7, units = "in", res = 300)
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Onset interval ratio')
text(x=1.5, y = seq(0,1,l=5), labels = round(seq(1,max(the_maps.projected_2015on$max_min_ratio),l=5), digits = 1))
rasterImage(legend_image, 0, 0, 1,1)
dev.off()
