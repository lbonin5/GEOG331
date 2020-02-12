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
