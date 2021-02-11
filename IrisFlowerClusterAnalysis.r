### Question 4.2

library(ggplot2)
irisData <- iris

## plot a few diagrams: it appears Petal dimensions are the most relevant in determining clusters
## so I will refer to the last model and use petal dimensions to fit my cluster model
ggplot(irisData, aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
ggplot(irisData, aes(Sepal.Length, Petal.Width, color=Species)) + geom_point()
ggplot(irisData, aes(Petal.Length, Sepal.Width, color=Species)) + geom_point()
ggplot(irisData, aes(Petal.Width, Petal.Length, color=Species)) + geom_point()

storage3 <- rep(0,5)

## fit cluster1 using the petal dimensions (columns 3,4), and test for various 
## values of C. I store the results of each test in storage3
for (c in 1:5){
  cluster1 <- kmeans(irisData[3:4], centers=c, nstart=10)
  storage3[c] <- cluster1$tot.withinss
}  

## by plotting the results of different centers(k) values, from storage3
## we can see the sharp decrease in effectiveness at point 3,
## so I will set centers=3

plot(storage3, type="l")

## calculate final cluster using petal dimensions and k=3,
## I store the results of the cluster2 model to the actual data set,
## and find 6 flowers were classified incorrectly
set.seed(100)
cluster2 <- kmeans(irisData[3:4], centers=3, nstart=10)
results <- table(cluster2$cluster, irisData$Species)

## I take the max of each row and sum it to get 144 flowers classified correctly,
## and I divide that by 150, the total number of flowers in the data set,
## to get accuracy = 0.96

accuracy = (sum(max(results[1,]) + max(results[2,]) + max(results[3,])))/nrow(irisData)
accuracy 