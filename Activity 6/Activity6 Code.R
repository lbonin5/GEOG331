#install the packages and load libraries for activity
install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("/Users/lucybonin/Documents/Github/GEOG331/GNPglaciers/GNPglaciers_1966.shp")
str(g1966)
plot(g1966,col="lightblue", axes=TRUE)
g1966@polygons[1]
g1998 <- readOGR("/Users/lucybonin/Documents/Github/GEOG331/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("/Users/lucybonin/Documents/Github/GEOG331/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("/Users/lucybonin/Documents/Github/GEOG331/GNPglaciers/GNPglaciers_2015.shp")
spplot(g1966, "GLACNAME")

#working with the raster data
redL <- raster("/Users/lucybonin/Documents/Github/GEOG331/glacier_09_05_14/l08_red.tif")
greenL <- raster("/Users/lucybonin/Documents/Github/GEOG331/glacier_09_05_14/l08_green.tif")
blueL <- raster("/Users/lucybonin/Documents/Github/GEOG331/glacier_09_05_14/l08_blue.tif")
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
  NDVIraster[[i]] <- raster(paste0("/Users/lucybonin/Documents/Github/GEOG331/NDVI/NDVI_",ndviYear[i],".tif"))
  
}
str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs
#HIGHER NDVI REPRESENTS MORE VEGETATION ON THE GROUND
#plotting 2003
plot(NDVIraster[[1]])

par(mfrow=c(1,2))
plot(NDVIraster[[1]])
plot(g1966,axes=TRUE)

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#QUESTION 4
plot(NDVIraster[[13]],axes=FALSE)
plot(g2015p,col=NA,add=TRUE,border="black")

#QUESTION 5

g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
} 

#calculating the percent change
percent_change<-list()
for (i in 1:39){
  percent_change[[i]]<-((gAll$a2015m.sq[i]-gAll$a1966m.sq[i])/gAll$a1966m.sq[i])*100
}
percent_change
g2015@data$percent_change<-percent_change
spplot(g2015, "percent_change")

percent_change<-as.numeric(percent_change)
min(percent_change,na.rm=TRUE)

g2015@data$GLACNAME[5]

#find where she zooms in and change
#trial and error

#QUESTION 7
#extract NDVI values
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}
#buffer glaciers
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)

#QUESTION 8
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)
meanChange<-meanChange[-1,]
g2015@data$meanChange<-meanChange[,2]
spplot(g2015, "meanChange")
#looking at how it may change over 15 years
mean15<-meanChange[,2]*15

#Question 11
#calculating
NDVImean<-calc(NDVIstack,mean,na.rm=TRUE)
avgZones<-zonal(NDVImean,glacZones,"mean")
head(avgZones) #average vegetation for that zone over all the years
avgZones<-avgZones[-1,]
g2015p@data$avgZones<-avgZones[,2]

quantile(avgZones[,2])
NDVIcol<-rep(NA, length(avgZones[,2]))
for (i in 1:39){
  if (g2015p@data$avgZones[i]<0.25) {
    NDVIcol[i]<-"lightblue";
  } else if (g2015p@data$avgZones[i]< 0.33) {
    NDVIcol[i]<-"purple";
  } else if (g2015p@data$avgZones[i]< 0.38) {
    NDVIcol[i]<-"red";
  } else {
    NDVIcol[i]<-"blue";
  }
}
g2015p@data$NDVIcol<-NDVIcol
plot(NDVImean)
plot(g2015p,add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)
legend("topright", c("avg NDVI <0.25","avg NDVI <0.33","avg NDVI <0.38","avg NDVI>0.38"), #legend items
       col=c("lightblue","purple","red","blue"),#colors
       bty="n")


