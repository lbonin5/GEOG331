#use built in iris dataset
#take a look at it 
install.packages(c("dplyr","ggplot2"))
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length
#creating a dataframe for iris versicolor info
versicolor<-iris[iris$Species == "versicolor", ]
head(versicolor)
#creating a linear model (y~x)
#dependent var is a function of independent var
lm.out<-lm(versicolor$Sepal.Width~versicolor$Sepal.Length)
#another way to subset
#lm.out<-lm(versicolor[,Sepal$Width]~versicolor[,Sepal$Length])
summary(lm.out)

#iteration through vectors
x<-c("Sepal.Length","Petal.Length","Sepal.Length")
y<-c("Sepal.Width","Petal.Width","Petal.Length")
#empty list to set up dummy var
lm.out<-list()
#lists are indexed with double brackets
for (i in 1:3){
  lm.out[[i]]<-lm(versicolor[,paste(y[i])]~versicolor[,paste(x[i])])
}

lm.out[[1]]
lm.out[[2]]
lm.out[[3]]


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#using join to combine this to our original iris dataset
#iris is left table
#height is right table
iris2<-left_join(iris,height, by="Species")
head(iris2)
iris2$Petal.Width/iris2$Height.cm



#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = iris, aes(Sepal.Length,Sepal.Width)) +
  geom_point()


#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = iris, aes(Sepal.Length,Sepal.Width)) +
  geom_point() +
  theme_classic()

#3c.make a scatter plot with ggplot and get rid of grid lines
#and show species by color increasing the point size
ggplot(data = iris, aes(Sepal.Length,Sepal.Width, color=Species)) +
  geom_point(size=4) +
  theme_classic() 

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################
#ggplot requires different declaration of the arguements
#example: declaring data frame first,
#including the geomtry of the data you are plotting.
#ggplot also tends to be a lot easier to change different features
#of the data plot