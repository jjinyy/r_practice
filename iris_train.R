data(iris)
library(MASS)
set.seed(1)
train <- sample(1:150, 100)
train_data <- iris[train, ]

iris_lda <- lda(Species ~ ., data = train_data)
iris_lda

> iris_lda
Call:
lda(Species ~ ., data = train_data)

Prior probabilities of groups:
    setosa versicolor  virginica 
      0.34       0.31       0.35 

Group means:
           Sepal.Length Sepal.Width Petal.Length Petal.Width
setosa         5.020588    3.438235     1.467647    0.250000
versicolor     5.954839    2.767742     4.303226    1.319355
virginica      6.685714    2.965714     5.591429    2.022857

Coefficients of linear discriminants:
                    LD1        LD2
Sepal.Length  0.9262525 -0.3105616
Sepal.Width   1.3900272 -1.8636222
Petal.Length -2.2019390  1.1597429
Petal.Width  -2.8290056 -3.0585818

Proportion of trace:
   LD1    LD2 
0.9912 0.0088


apply(iris_lda$means%*%iris_lda$scaling,2,mean)
> apply(iris_lda$means%*%iris_lda$scaling,2,mean)
      LD1       LD2 
-2.024635 -6.795722 

Species = -2.024635+0.9262525*Sepal.Length+1.3900272*Sepal.Width-2.2019390*Petal.Length-2.8290056*Petal.Width

Species = -6.795722-0.3105616*Sepal.Length-1.8636222*Sepal.Width+1.1597429*Petal.Length-3.0585818*Petal.Width

predict(iris_lda, iris[-train,])$class
predict(iris_lda, iris[-train,])$posterior

cm <- table(iris$Species[-train], predict(iris_lda, iris[-train,])$class)

sum(cm[row(cm) == col(cm)])/sum(cm) #정분류율


1-sum(cm[row(cm) == col(cm)])/sum(cm)








