library(lubridate)
#reading in the streamflow data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)
#reading in the precipitation data
#hourly precip is in milimeters
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)
#creating a seperate data frame with only the approved measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))  

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#Question 5
#plotting average discharge with standard deviation
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")
#plot window
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot((aveF$doy),aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",
     axes=FALSE)
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), 
        border=NA#no border
)
#las lets you change the direction of the axis ticks
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
#adding a legend to the graph
legend("topright", c("mean","1 standard deviation","2017 Data"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"green"),#colors
       pch=c(NA,15,NA),#symbols
       bty="n")
#pulling data from 2017
dat2017<-datD$discharge[which(datD$year==2017)]
#adding a line on the plot
lines(datD$decDay[which(datD$year==2017)], dat2017, col="green")

#QUestion 7
#creating a data frame for days with full 24 hours of precip
prec.agg<-aggregate(datP$hour, by=list(datP$doy,datP$year), length)
colnames(prec.agg) <- c("doy","year","hourtotal")
prec.24<-prec.agg[which(prec.agg$hourtotal==24),]
dev.new(width=8,height=8)
par(mai=c(1,1,1,1))
#plotting discharge
plot(datD$decYear,datD$discharge,     
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)
#creating decimal year category for all observations with 24 horus to add them to plots
prec.24$decYear <- ifelse(leap_year(prec.24$year),prec.24$year + ((prec.24$doy-1)/366),
                       prec.24$year + ((prec.24$doy-1)/365))
#adding points for the days with full 24 precip
points(prec.24$decYear, y=rep(0,length(prec.24$decYear)), pch=19, col="blue")
legend(2013,410, c("discharge","days with 24 hours precip"), #legend items
       lwd=c(2,NA),#lines
       col=c("black","blue"),#colors
       pch=c(NA,19),#symbols
       bty="n")


#Question 8
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
#looking at the minimum day
min(hydroD$discharge)
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
#creating a hydrograph for a new day! Choosing Day 13 & 14 of 2012
hydroD.14 <- datD[datD$doy >= 13 & datD$doy < 15 & datD$year == 2012,]
hydroP.14 <- datP[datP$doy >= 13 & datP$doy < 15 & datP$year == 2012,]
#checking the minimum
min(hydroD.14$discharge)
#creating a floor for plotting
yl.14 <- floor(min(hydroD.14$discharge))-1
#celing rounds up to the integer
yh.14 <- ceiling(max(hydroD.14$discharge))+1
#minimum and maximum range of precipitation to plot
pl.14 <- 0
pm.14 <-  ceiling(max(hydroP.14$HPCP))+.5
#scaling
hydroP.14$pscale <- (((yh.14-yl.14)/(pm.14-pl.14)) * hydroP.14$HPCP) + yl.14
par(mai=c(1,1,1,1))
#make plot of discharge  for day 13 and 14 of 2012
plot(hydroD.14$decDay,
     hydroD.14$discharge, 
     type="l", 
     ylim=c(yl.14,yh.14), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP.14)){
        polygon(c(hydroP.14$decDay[i]-0.017,hydroP.14$decDay[i]-0.017,
                  hydroP.14$decDay[i]+0.017,hydroP.14$decDay[i]+0.017),
                c(yl.14,hydroP.14$pscale[i],hydroP.14$pscale[i],yl.14),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
#Question 9

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()
#violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()
#plots for streamflow by season for 2016 and 2017
year16<-datD[datD$year=="2016",]
year16$season<-rep(NA,35132)
#Spring begins on the 80th day (March 20th)
#Summer begins on the 173rd day (June 21)
#fall begins on the 266th day (Sept 22)
#Winter begins on the 357th day (December 22nd)
for (i in 1: length(year16$doy)){
if (year16$doy[i]<80 | year16$doy[i]>=357) {
        year16$season[i]<-"winter"
} else if (year16$doy[i]>=80 & year16$doy[i]<173) {
        year16$season[i]<-"spring"
} else if (year16$doy[i]>=173 & year16$doy[i]<266) {
        year16$season[i]<-"summer"
} else {
        year16$season[i]<-"fall"
}
}
year16$season<-as.factor(year16$season)
is.factor(year16$season)
#plotting for 2016
ggplot(data= year16, aes(season,discharge)) + 
        geom_violin(fill="lightblue")+ggtitle("Discharge by Season 2016")

#making the plot for the 2017 season
year17<-datD[datD$year=="2017",]
year17$season<-rep(NA,35018)
#Spring begins on the 79th day (March 20th)
#Summer begins on the 173rd day (June 22)
#fall begins on the 265th day (Sept 22)
#Winter begins on the 356th day (December 22nd)
for (i in 1: length(year17$doy)){
        if (year17$doy[i]<79 | year17$doy[i]>=356) {
                year17$season[i]<-"winter"
        } else if (year17$doy[i]>=79 & year17$doy[i]<173) {
                year17$season[i]<-"spring"
        } else if (year17$doy[i]>=173 & year17$doy[i]<265) {
                year17$season[i]<-"summer"
        } else {
                year17$season[i]<-"fall"
        }
}
year17$season<-as.factor(year17$season)
is.factor(year17$season)
#plotting for 2017
ggplot(data= year17, aes(season,discharge)) + 
        geom_violin(fill="lightblue")+ggtitle("Discharge by Season 2017")


