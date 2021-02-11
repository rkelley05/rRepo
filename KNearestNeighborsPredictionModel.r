## Using K-nearest neighbors, a machine learning model, I will analyze a set of credit
## data and build a model to predict the likelihood of an applicant defaulting on a loan 
## due to certain attributes as described in the data set.

library(kknn)
set.seed(42)

myData <- read.table("C:/Users/DELL/Documents/R/R Projects/credit_card_data-headers.txt", header=TRUE)

## initialize variables/vectors
y=1
rows <- nrow(myData)
check <- rep(1,20)

## create vector with zero placeholders in which to store predicted values
## that is the length of the data set
storage <- rep(0, rows)

## create function with k values represented by the variable y
## k is what we are testing, so it should be the function variable
funk = function(y){
  
  ## for loop that loops through row i in myData
  for (i in 1:rows){
    
    ## exclude response row R1 and row i 
    kknnModel = kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15, 
                     myData[-i,], myData[i,], 
                     k=y, 
                     distance = 2, 
                     kernel = "optimal", 
                     scale=TRUE)
    
    ## fit and round predicted values for i and store them 
    storage[i] <- as.integer(round(fitted.values(kknnModel)))
  }
  
  ## calculate accuracy by comparing predicted values from storage
  ## and dividing by number of rows
  percent = sum(storage == myData[,11]) / rows
  
  return(percent)
  
}

##check each value 1-20 as K and store in vector
for (y in 1:20){
  check[y] = funk(y)
}

max(check)
which.max(check)