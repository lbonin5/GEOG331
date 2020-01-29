#vector examples
heights <- c(30,41,20,22)
#CONVERT TO cm
heights_cm<-heights*100

#read in weather station file from the data folder
datW <- read.csv("y:\\Students\\lbonin\\a02\\2011124.csv")
str(datW)
