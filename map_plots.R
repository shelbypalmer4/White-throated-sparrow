install.packages("sp")
install.packages("geos")
install.packages("rgdal")

library(sp)
library(rgdal)

install.packages(c("maps", "mapproj", "mapdata", "rgeos", "maptools", "raster"))
library(maps)
library(mapproj)
library(mapdata)
library(rgeos)
library(maptools)
library(raster)


# can0<-getData('GADM', country="CAN", level=0) # Canada
can1<-getData('GADM', country="CAN", level=1) # provinces
# can2<-getData('GADM', country="CAN", level=2) # counties

us1 <- getData('GADM', country="USA", level=1)
# us2 <- getData('GADM', country="USA", level=2)


## Specify a geographic extent for the map
## by defining the top-left and bottom-right geographic coordinates
mapExtent <- rbind(c(-135, 70), c(-64, 21))

## Specify the required projection using a proj4 string
## Use http://www.spatialreference.org/ to find the required string
## Polyconic for North America
newProj <- CRS("+proj=poly +lat_0=0 +lon_0=-100 +x_0=0 
            +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## Project the map extent (first need to specify that it is longlat) 
mapExtentPr <- spTransform(SpatialPoints(mapExtent, 
                                         proj4string=CRS("+proj=longlat")),
                           newProj)

## Project other layers
can1Pr <- spTransform(can1, newProj)
us1Pr <- spTransform(us1, newProj) 

plot(mapExtentPr, pch=NA)
plot(can1Pr, border="black", col="white", add=TRUE)
plot(us1Pr, border="black", col="white", add=TRUE)

#coordinates(the_truth) <- ~Longitude+Latitude
crs(the_truth) <- CRS("+proj=longlat +datum=WGS84")

the_truth.projected <- spTransform(the_truth, newProj)

the_truth.projected_2015on <- the_truth.projected[which(the_truth.projected$Year > 2014),]
#pal = colorRampPalette(c("blue", "red"))

points(the_truth.projected_2015on, pch = 16, cex = 2, col = rgb(red = 1- the_truth.projected_2015on$dists_from_line/max(the_truth.projected_2015on$dists_from_line), green = 0, blue = the_truth.projected_2015on$dists_from_line/max(the_truth.projected_2015on$dists_from_line)))

plot(mapExtentPr, pch=NA)
plot(can1Pr, border="black", col="white", add=TRUE)
plot(us1Pr, border="black", col="white", add=TRUE)
the_truth.projected_2014_and_earlier <- the_truth.projected[which(the_truth.projected$Year <= 2014),]
  

