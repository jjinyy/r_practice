#타이타닉 데이터로 모델비교
#랜덤포레스트,로짓회귀분석,의사결정나무
#install.packages("caret") # caret 명칭은 "Classification And REgression Training" 의 앞글자
#install.packages("party") 
#install.packages("e1071")
#install.packages("randomForest")
library(caret)
library(party)
library(randomForest)

titanicData <- read.csv("./Data/rd_titanic.csv")

class(titanicData)
str(titanicData)
titanicData$Survived = factor(titanicData$Survived)
titanicData$Sex = factor(titanicData$Sex)
titanicData$Pclass = factor(titanicData$Pclass)

set.seed(1234)
splitData <- createDataPartition(y = titanicData$Survived, p = 0.7, list = FALSE)
#splitData <- sample(1:nrow(titanicData), nrow(titanicData)*0.7)
titanic_train <- titanicData[splitData, ]
titanic_test <- titanicData[-splitData, ]

#a. 의사결정나무
#####  Train and create ctree
train_ctree = ctree(Survived ~ Pclass + Sex + Age + Siblings.Spouses.Aboard , data=titanic_train)
train_ctree
#####  Plot the tree
plot(train_ctree)


#b. 의사결정나무 Confusion matrix
#####  Use predict function to run prediction model
ctree_predict = predict(train_ctree, titanic_test)
require(caret)
#####  use a confusion matrix to generate the statistics of the output matrix
confusionMatrix(ctree_predict, titanic_test$Survived)
# 의사결정나무 Accuracy : 0.7736


#c. 랜덤포레스트
# Random Forest model
rfModel <- randomForest(Survived ~ Pclass + Sex + Siblings.Spouses.Aboard + Age, data = titanic_train)
# importance
rfInpo <- randomForest(Survived ~ Pclass + Sex + Siblings.Spouses.Aboard + Age , data = titanic_train, importance = T)
importance(rfInpo)
rf_predict = predict(rfInpo, titanic_test)
confusionMatrix(rf_predict, titanic_test$Survived)
#랜덤포레스트 Accuracy : 0.7962
#따라서, Random Forest가 Decision Tree 보다 높은 정확성을 보여준다.
#의사결정나무 대비 랜덤포레스트가 더 좋다.


#d. 로짓회귀분석
glmModel <- train(Survived ~ Pclass + Sex + Siblings.Spouses.Aboard + Age, titanic_train, method = "glm")
glm_predict <- predict(glmModel, newdata = titanic_test)
confusionMatrix(glm_predict, titanic_test$Survived)

#의사결정나무 Accuracy : 0.7736
#랜덤포레스트 Accuracy : 0.7962
#로짓회귀분석  Accuracy : 0.7925

#정확도 비교 : 랜덤포레스트 > 로짓회귀분석 > 의사결정나무
#랜덤포레스트가 가장 좋은 결과를 나타냈다.

