# 505-Week-6-Activity
SVM-Algorthm
install.packages("e1071")
install.packages("GGally")
install.packages("ggplot2")

library(e1071)
library(GGally)
library(ggplot2)

data(iris)
str(iris)
head(iris,3)


svm_model <- svm(Species ~ ., data=iris,
                 kernel="radial")
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

plot(svm_model, data=iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4)
     
pred = predict(svm_model,iris)
     tab = table(Predicted=pred, Actual = iris$Species)

1-sum(diag(tab)/sum(tab))

#How did the model do?SVM is one of the most popular Supervised Learning algorithms, which is used for Classification as well as Regression problems.
#What is the accuracy rate: 0.02666667

....................................................................
  K mean algoritm
  require("datasets")
data("iris")
str(iris)
summary(iris)
head(iris, 3)

iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new, 3)

head(iris.class, 3)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

result<- kmeans(iris.new,3)
result$size
result$centers
result$cluster

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)
plot(iris.new[c(1,2)], col=iris.class)
plot(iris.new[c(3,4)], col=result$cluster)
plot(iris.new[c(3,4)], col=iris.class)
table(result$cluster,iris.class)

#Total number of correctly classified instances are: 36 + 47 + 50= 133
#Total number of incorrectly classified instances are: 3 + 14= 17

#How did the model do? algorithm that tries to partition the dataset into K pre-defined distinct non-overlapping subgroups (clusters) where each data point belongs to only one group. It tries to make the intra-cluster data points as similar as possible while also keeping the clusters as different (far) as possible.
#TASK: Accuracy = number of correctly classified/(total classified) = 133/150
#i.e our model has achieved 0.8866% accuracy!

...........................................
C50 Algorithm

install.packages("C50")
install.packages("dplyr")
library(dplyr)
iris

head(iris,4)
dim(iris)

library(C50)

iris_setosa <- iris[iris$Species == "setosa", ]
iris_versicolor <- iris[iris$Species == "versicolor",]
iris_virginica <- iris[iris$Species == "virginica",]

iris_train <- rbind(iris_setosa[1:35,], iris_versicolor[1:35,], iris_virginica[1:35,])
iris_test <- rbind(iris_setosa[36:50,], iris_versicolor[36:50,], iris_virginica[36:50,])

install.packages("caret")
library(caret)

install.packages("lattice")
install.packages("ggplot2")

library(caret)

attach(iris)
summary(iris)

attach(iris)
inTrainingData<- createDataPartition (y= Species, p=0.70, list = FALSE)
trainData <- iris[inTrainingData,]
testData <- iris[-inTrainingData,]

dtModel <- C5.0(trainData[,-5], trainData$Species)
plot(dtModel)


predict(dtModel, testData)

summary(testData)

(testData$Species == predict(dtModel, testData))

mean(testData$Species == predict(dtModel, testData))

install.packages("gmodels")
library(gmodels)
CrossTable(testData$Species,predict(dtModel, testData))

CrossTable(testData$Species == predict(dtModel, testData))

#How did the model do? I think this is simple algorithm in which decision tree help us to find the solution in easy way.
#TASK: What percent of cases were correctly classified? 0.911

...................


SVM accuracy: 26.66%

K Means accuracy: 88.66%

C50 accuracy: 91.11%
