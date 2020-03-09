#install the packages and load libraries for activity
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Students\\lbonin\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
str(g1966)
plot(g1966,col="lightblue", axes=TRUE)
g1966@polygons[1]
g1998 <- readOGR("Y:\\Students\\lbonin\\a06\\GNPglaciers\\GNPglaciers_1998.shp")
g2005 <- readOGR("Y:\\Students\\lbonin\\a06\\GNPglaciers\\GNPglaciers_2005.shp")
g2015 <- readOGR("Y:\\Students\\lbonin\\a06\\GNPglaciers\\GNPglaciers_2015.shp")
spplot(g1966, "GLACNAME")

#working with the raster data
redL <- raster("Y:\\Students\\lbonin\\a06\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("Y:\\Students\\lbonin\\a06\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("Y:\\Students\\lbonin\\a06\\glacier_09_05_14\\l08_blue.tif")
plot(blueL)
#brick and stack are similar functions, brick works faster
#if working from a server
redL@crs
rgbL <- brick(redL, greenL, blueL)
#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
#stretch adds contrast to image
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#reading NDV data
#set up sequence of years to see what year we are looking at
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("Y:\\Students\\lbonin\\a06\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}
str(NDVIraster[[1]])
