#vector examples
heights <- c(30,41,20,22)
#CONVERT TO cm
heights_cm<-heights*100

#question 2 examples
#character vector
char_vec<-c("one","two","three","four","five")
#numerical vector
num_vec<-c(5.5,6.5,7.5,3.5,2.5)
#integer vector
int_data<-c(1,2,3,4,5)
int_vec<-as.integer(int_vec)
is.integer(int_vec)
#factorvector
factor_data<-c("female","male","male","female","male")
factor_vec<- factor(factor_data)

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

#same window for the four histograms
par(mfrow=c(2,2))

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

#histrogram creation for Livermore, CA
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color, thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histrogram creation for MOrmon Flat AZ
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color, thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histrogram creation for MOrrisville NY
hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color, thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#probability of temperatures below freezing in Aberdeen
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#probability temps between 0 and 5 in Aberdeen
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#temps above 20 degrees in Abderdeen
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#extreme temp calcuation in Aberdeen
Aberdeen_increase<-mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4
extreme_aberdeen<-qnorm(0.95,
                      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
1 - pnorm(extreme_aberdeen,
          Aberdeen_increase,
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#aberdeen parcipitation data
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#precip for each year/site in data
yearly_precp<-aggregate(datW$PRCP,by=list(datW$NAME,datW$year),FUN="sum",na.rm=TRUE)
#representing annual precip for aberdeen as a histogram
hist(yearly_precp$x[yearly_precp$Group.1=="ABERDEEN, WA US"],
     freq=FALSE, 
     main = "Precipitation for Aberdeen",
     xlab = "Average daily precipitation", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#mean of annual precipitation across all sites
averagePRCP<-aggregate(yearly_precp$x, by=list(yearly_precp$Group.1), FUN="mean", na.rm=TRUE)
averagePRCP
averageTemp
