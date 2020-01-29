#vector examples
heights <- c(30,41,20,22)
#CONVERT TO cm
heights_cm<-heights*100

#question 2 examples
#charcter vector

#read in weather station file from the data folder
datW <- read.csv("y:\\Students\\lbonin\\a02\\2011124.csv")
#gives data frame information
str(datW)
#change to a different date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#create a date column to only include years, treat as numeric factor
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#list all site names
levels(datW$NAME)
#mean maximum temperature for Aberdeen with na.rm argument to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculating daily avergage temp at sites
datW$TAVE<-datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#finding average daily temp on all sites
averageTemp<-aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
#list average temp for all 5 sites
averageTemp

#now list it with column names
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
datW$siteN <- as.numeric(datW$NAME)

#histrogram creation for Aberdeen, WA
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color, thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)





