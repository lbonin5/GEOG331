#downloaded lubridate package
#install.packages(c("lubridate"))
library(lubridate)

#creation of a function that test whether the code does what we expect (assert)
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}
#now checking if statement works
assert(1 == 2, "error: unequal values")
#true statement
assert(2 == 2, "error: unequal values")
a<-c(3,4,5,6)
b<-c(3,4,5)
#testing if vectors are the same length
assert(length(a) == length(b), "error: unequal lengths")


#reading in the data
#na.strings=c("#N/A") changes to NA
#skip 3 skips the first 3 rows and begins reading data at row 4
datW <- read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
print(datW[1,])
sensorInfo <-   read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)
#all the information all together
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#convert to standardized format of m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#air temp
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))
#we see that there are measures missing from the soil sensor
#no missing data from the weather station sensors

#make a plot with filled in points (using pch)
#help us figure out what might be going on with the missing data
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#nothing unusual on the plot

#QUESTION 4
#new column for QA/QC of air temp
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)
#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)
#days with low air temp
datW[datW$air.tempQ1 < 8,]
#days with high air temp
datW[datW$air.tempQ1 > 33,]

#potential for sensor unreliability during thunderstorms
#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#QUESTION 5
assert(length(datW$DD) == length(lightscale), "error: unequal lengths")
assert(length(datW$precipitation) == length(lightscale), "error: unequal lengths")

#QUESTION 6
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
