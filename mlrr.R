d<-read.csv("C:/Users/Sravanthi/Desktop/murder.csv")
d

x<-c(d[,2])
y<-c(d[,4])

# Apply the lm() function.
relation <- lm(x~y)

print(relation)
summary(relation)

a <- data.frame(y = 30)
result <-  predict(relation,a)
print(result)

plot(y,x,col = "blue",main = "murder & literacy Regression",
     abline(lm(y~x)),cex = 1.3,pch = 16,xlab = "POVERTY",ylab = "murders")


install.packages("e1071")
install.packages("caTools")
install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)

# Loading data
data(d)

head(d)

# Splitting data into train
# and test data
split <- sample.split(women, SplitRatio = 0.7)
train_cl <- subset(women, split == "TRUE")
test_cl <- subset(women, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Height,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Height, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Height)
print(paste('Accuracy =', 1-misClassError))
# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$STATE,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$Height)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Height,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$Height)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Height,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$Height)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$MURDER)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$MURDER,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$MURDER)
print(paste('Accuracy =', 1-misClassError))


