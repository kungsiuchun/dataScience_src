library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# set.seed(2) # if using R 3.5 or earlie
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index, ]
train <- iris[-test_index, ]

iris %>% group_by(Species) %>% summarize(mean(Sepal.Length),
                                         sd(Sepal.Length),
                                         mean(Sepal.Width),
                                         sd(Sepal.Width),
                                         mean(Petal.Length),
                                         sd(Petal.Length),
                                         mean(Petal.Width),
                                         sd(Petal.Width),)
foo <- function(x) {
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i) {
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    accuracy <- mean(y_hat == train$Species)

  })
}

predictions <- apply(test[,-5], 2, foo)
sapply(predictions, max)



# best cutoffs for Petal.Length
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1], range(train[,3])[2], by=0.1)
cutoffs <- rangedValues[which(predictions==max(predictions))]
cutoffs
# best cutoffs for Petal.Width
p1 <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1], range(train[,4])[2], by=0.1)
c1 <- rangedValues[which(predictions==max(predictions))]
c1

y_hat <- ifelse(test$Petal.Length > cutoffs | test$Petal.Width > c1, 'virginica', 'versicolor')
mean(y_hat == test$Species)

plot(iris,pch=21,bg=iris$Species)



petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)]

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)]

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)
