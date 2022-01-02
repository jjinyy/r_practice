# 우리가 많이 다룬 iris를 데이터를 훈련용 데이터로 분석모델을 만들고,
# 만들어진 모델의 적합성을 테스트 데이터로 검정해 보겠습니다.
# iris 데이터에는 모두 150개의 관측치와 5개의 변수가 있습니다.


http://blog.naver.com/PostView.nhn?blogId=pmw9440&logNo=221574212339&parentCategoryNo=&categoryNo=7&viewDate=&isShowPopularPosts=false&from=postView
# Q1. [20점] 150개 관측치에서 random하게 100개를 추출해 train 데이터를 만듭니다.
# 임의로 추출할 때 set.seed(1)로 하고, 100개의 관측치를 갖는 train_data를 만드십시오.

data(iris)
library(MASS)
set.seed(1)
train <- sample(1:150, 100)
train_data <- iris[train, ]

# Q2.[40점] Q1에서 임의로 추출된 100개의 데이터로 판별분석을 하고 분석 결과의 다음 용어가 의미하는 바를 설명하시오.
# a. Prior probabilities of groups:
# b. Group means:
# c. Coefficients of linear discriminants:

10점
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

# a. Prior probabilities of groups: 
#	 LDA 분석 이전 확률은 setosa, versicolor, virginca는 학습 데이터에 존재하는 비율로
#	 각각 0.29, 0.35, 0.36으로 산정됨을 알 수 있습니다.

# b. Group means: 집단별 독립변수의 평균을 의미합니다.

# c. Coefficients of linear discriminants:
#	 LDA분석으로 계산된 판별함수식의 계수가 되겠습니다.
#	 LD1, LD2 총 2개의 판별함수식이 만들어짐을 확인할 수 있습니다.

# Q3.[20점] 판별함수식 2개를 구하시오.
# 판별함수식의 상수항은 아래와 같이 구할 수 있습니다.
 판별함수의 상수항은 apply() 함수을 이용하여 LDA 판별함수식의 계수에 평균값을 대입하면 산정할 수 있으며 그 결과,  LD1 은 -2.132868, LD2는 6.635509로 산정되었습니다.

apply(iris_lda$means%*%iris_lda$scaling,2,mean)
> apply(iris_lda$means%*%iris_lda$scaling,2,mean)
      LD1       LD2 
-2.024635 -6.795722 

10점
Species = -2.024635+0.9262525*Sepal.Length+1.3900272*Sepal.Width-2.2019390*Petal.Length-2.8290056*Petal.Width

10점
Species = -6.795722-0.3105616*Sepal.Length-1.8636222*Sepal.Width+1.1597429*Petal.Length-3.0585818*Petal.Width

# Q4.[20점] 실제값과 예측값이 맞게 분류된 비율을 산출하는 R코드를 제시하시오.
predict(iris_lda, iris[-train,])$class
predict(iris_lda, iris[-train,])$posterior

cm <- table(iris$Species[-train], predict(iris_lda, iris[-train,])$class)

sum(cm[row(cm) == col(cm)])/sum(cm) #정분류율


1-sum(cm[row(cm) == col(cm)])/sum(cm)








