# 다음 data set에는 유방암 수술을받은 환자의 생존에 대해 시카고 대학 Billing's 병원에서
# 1958년에서 1970년 사이에 수행된 실제 연구 사례가 포함되어 있고, 아래 URL에서 데이터를 읽어올 수 있고,
# 이는 UCI Repository에 저장되어 있어 관련 정보를 얻을 수 있습니다.

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data"

# Q1.[20점] 주어진 URL로부터 데이터를 읽어드리고, 각 변수에 age, op_year, no_nodes, survival 변수명을 부여하고
# 변수의 의미를 설명하고, 독립변수와 종속변수를 구분하십시오.

# 5점
# read.csv로 읽어들이면 header가 없어 header을 FALSE로 반드시 해야한다. 
# 데이터를 살펴보지 않아 header=FALSE로 하지 않은 답안이 다수 있을 것으로 생각합니다.
# 그렇지 않으면 첫 행의 관측치를 잃게되므로 나중에 분석 결과가 다르게 도출될 것임. 
# 그러나, 관측치 수가 306개이므로 평균과 분산에 미치는 영향은 미미해 분석결과 차이 역시 미미할 것으로 추정함.
haberman <- read.csv(URL, header = FALSE)

# read.table로 데이터를 읽어들이면 header = FALSE가 필요하지 않음.
# haberman <- read.table(URL, sep=",")

# 10점
age: 수술받을 당시 나이
op_year: 수술연도
no_nodes: 양성 림프샘 개수
survival: 수술 후 생존 연수 (5년 이상 생존은 1, 5년 이내 사망은 2)

독립변수: age, op_year, no_nodes
종속변수: survival

# 5점; 변수명 지정을 문제에서 요구하고 있으므로, 변수명 지정.
names(haberman) = c('age', 'op_year', 'no_nodes', 'survival')


# Q2.[20점] 주어진 문제에 적절한 회귀모형을 구하시오.

# 혼동 방지를 위해 1-> 0, 2 -> 1로 변환, 반드시 필요한 것은 아님, 그대로 두고 해석을 적절히 해도 됨.
# 그러나, 이를    1 -> 1, 2 ->0 으로 변환하는 것은 틀린 것은 아니지만,
# 원천 데이터에서 적은 숫자가(1) 생존확률, 큰 숫자가(2)가 사망 확률로 정의되어 있는데,
# 이를 역으로 변환하는 것은 모델링이 적절하다고 볼 수 없습니다. 감정대상입니다.
# haberman[haberman$survival == 1, "survival"] = 0  # 5년 이상 생존확률
# haberman[haberman$survival == 2, "survival"] = 1  # 5년 이내 사망확률

# 10점: 종속변수를 factor variable로 전환, 종속변수를 범주형 변수를 변환이 glm() 함수 적용을 위해 필요.
haberman$survival <- factor(haberman$survival)

str(haberman$survival)

# 5점, 적정함수를 사용하였는가? glm() 사용. survival 변수가 범주형이어야 함.
haber_glm <- glm(survival ~ age + op_year + no_nodes, data = haberman, family = binomial)

# generalized linear model의 계수들을 살펴봄.
summary(haber_glm)

> summary(haber_glm)

Call:
glm(formula = survival ~ age + op_year + no_nodes, family = binomial, 
    data = haberman)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3219  -0.7297  -0.6552   0.9230   1.9600  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.861625   2.675197  -0.696    0.487    
age          0.019899   0.012735   1.563    0.118    
op_year     -0.009784   0.042013  -0.233    0.816    
no_nodes     0.088442   0.019849   4.456 8.36e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 353.69  on 305  degrees of freedom
Residual deviance: 328.26  on 302  degrees of freedom
AIC: 336.26

Number of Fisher Scoring iterations: 4

# 5점: full model; 회귀모형을(회귀식) 제시하였는가? 아래와 같이 제시하여야 함.
# glm() 결과 제시는 분석을 한 것이지, 회귀모형 제시로 볼 수 없으나 여기서는 모든 독립변수를
# 모두 포함하는 full model 이므로, summary(glm 결과) 식으로 대체할 수 있음.

# 아래와 같은 답안을 제시해야 함.
# survival = -1.861625 + 0.019899*age -0.009784*op_year + 0.088442*no_nodes


# Q3.[20점] Q2에서 작성한 회귀모델을 사용해, 57세, op_year=1958, no_nodes=5 환자와 66세, op_year=1960, no_nodes=32
# 인 환자가 입원했을 때, 두 명의 환자에 대해 survival을 예측하고 결과를 설명하시오.


# 10점; 환자 데이터 정의, 모델에 새 환자 데이터를 적용해 환자1, 환자2의 사망확률 예측
new_patients <- data.frame(age = c(57, 66), op_year = c(58, 60), no_nodes = c(5, 32)) 
predict(haber_glm, newdata = new_patients, type = 'response')

> predict(haber_glm, newdata = new_patients, type = 'response')
        1         2  # <- 환자1, 환자2
0.2988848 0.8448620  # <- 사망확률

# 예측값이 0, 1이 아니어서 의아할 수 있으나, type="response" 옵션으로 예측한 결과는 [0, 1]사이 확률을 출력합니다.
# 예측값이 q라면 0에 속할 확률은(5년 이상 생존확률) 1-q라고 해석해야 한다.
# 또는 1에 속할 확률은(5년 이내 사망) q라고 말할 수 있다.

# 10점: 해석
# 그러므로, 첫 번째 환자는 5년 이상 생존할 확률이 100-29.89 = 70.11%이고, (혹은 5년 이내 사망확률은 29.89%)
# 두 번째 환자는 5년 이상 생존할 확률이 100-84.49= 15.51% 이다. (혹은 5년 이내 사망확률은 84.49%)
# glm(...family=binomial,...)을 적용하면, glm은 반응변수가 범주형이면 두 가지 값을 0과 1로 간주하여
# 모델링한다. 앞의 예제 코드의 경우 반응변수 survival 1과 2를 가진 범주형이므로 1을 0, 2를 1로 간주하여 모델링한다.

# 이렇게 하면 0과 1 사이의 확률을 출력할 수 있기 때문입니다.


# Q4.[20점] 모델링에 사용한 모든 독립변수가 필요한지 분석하고, 필요하지 않은 독립변수가 발견되면, reduced model을 제시하시오.

# 10점 step()를 사용하였는가? step() 함수를 사용하지 않고 근거 없이 직관에 의해 독립변수 숫자를 줄이면 점수를 받지 못합니다.
# full model의 회귀계수의 유믜미성으로 판단하여 독립변수의 사용채택 유무를 결정하는 것은 적절하지 않습니다.
# summary() 에서 회귀계수에 대한 유의미성은 각 계수에 대한 통계적 해석 결과를 보여줄 뿐입니다.
# 이를 근거로 독립변수를 줄이면 절편 역시 통계적으로 유의미하지 않는데 왜 제거하지 않았는지요????

haber_rmglm <- step(haber_glm)
summary(haber_rmglm)

> haber_rmglm <- step(haber_glm)
Start:  AIC=336.26
survival ~ age + op_year + no_nodes

           Df Deviance    AIC
- op_year   1   328.31 334.31
<none>          328.26 336.26
- age       1   330.71 336.71
- no_nodes  1   352.24 358.24

Step:  AIC=334.31
survival ~ age + no_nodes

           Df Deviance    AIC
<none>          328.31 334.31
- age       1   330.72 334.72
- no_nodes  1   352.28 356.28

> summary(haber_rmglm)

Call:
glm(formula = survival ~ age + no_nodes, family = binomial, data = haberman)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3361  -0.7302  -0.6547   0.9178   1.9741  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -2.46290    0.70643  -3.486  0.00049 ***
age          0.01965    0.01269   1.549  0.12144    
no_nodes     0.08832    0.01982   4.456 8.34e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 353.69  on 305  degrees of freedom
Residual deviance: 328.31  on 303  degrees of freedom
AIC: 334.31

Number of Fisher Scoring iterations: 4

# 10점: 분석결과를 보고 reduced model을 제시하였는가?
# survival = -2.46290 + 0.01965*age + 0.08832*no_nodes


# Q5.[20점] Q4에서 reduced model이 발견된다면 두 모델의 설명력이 차이가 있는지 검정하시오.

# full model을 근거로 생존율과 사망율를 
# 우선 주어진 데이터의 5년 이내 생존 및 사망율을 확인해 봄.

haberman$survival
haber.actual <- haberman$survival
table(haber.actual)/sum(table(haber.actual))

> table(haber.actual)/sum(table(haber.actual))
haber.actual
        1         2 
0.7352941 0.2647059 
# 주어진 데이터에 의하면 사망률이 약 0.7353, 생존율이 0.2647.

haber.prob <- predict(haber_glm, newdata = haberman, type = 'response')
haber.pred <- as.numeric(haber.prob > 0.2647059) # TRUE 이면 1, 그러므로 5년 사망. FALSE이면 0, 그러므로 5년이상 생존.
haber.cm <- table(haber.actual, haber.pred)
print(haber.cm)
> print(haber.cm)
            haber.pred
haber.actual   0   1
           1 180  45
           2  40  41

# reduced model을 근거로 생존율과 사망율를 
haber.rmprob <- predict(haber_rmglm, newdata = haberman, type = 'response')
haber.rmpred <- as.numeric(haber.rmprob > 0.2647059) # TRUE 이면 1, 그러므로 5년 사망. FALSE이면 0, 그러므로 5년이상 생존.
haber.rmcm <- table(haber.actual, haber.rmpred)
print(haber.rmcm)
> print(haber.rmcm)
            haber.rmpred
haber.actual   0   1
           1 183  42
           2  40  41

# 결과를 해석하자면, 5년내 사망율은 full 모델과 reduced 모델인 경우에 동일하나,
# 5년 이상 생존율은 full 모델 대비 reduced 모델인 경우에 다소 개선되었음을 알 수 있다.


# 두 모델을 비교 시, 이탈도가 작아짐.
AIC=336.26 에서 AIC: 334.31로 작아져 이탈도가 작아짐을 알 수 있다.

# 독립변수의 후보가 k개 있으면 가능한 독립변수의 조합은 2^k 개
# 독립변수의 후보가 많으면 모든 조합으로 회귀분석을 실시하는 것은 현실적으로 불가능
# 단계적 회귀분석(stepwise regression): 독립변수를 하나씩 추가/제거 하여 종속변수를 잘 예측하는 변수들을 선택하는 기법
# (forward, backward 방법 등이 있음. default는 backward. Q4에서도 backward)
#	예측력이 (통계적으로) 유의미한 예측변수들만을 골라줌
#	오직 자료만으로 변수를 선택하기 때문에 이상한 결과가 생길 수 있음. ==> validation 데이터에서 검증 필요.
#	다중공선성 등의 문제에 대한 대처 불가 ==> 추후 확인해야 함

# AIC가 낮을수록 설명력이 좋은 모형임은 알고 있는 것으로 간주합니다. 
# 위 Q4번에서 사용한 step() 함수 결과 해석은
# 모든 독립변수를 포함하면 AIC=336.26, 여기서 op_year를 제거하면 AIC=334.31
# age를 제거하면 AIC=334.72, no_nodes를 제거하면 AIC=356.28로 증가함므로
# 설명력이 저하합니다. 그러므로, op_year만 제거하는 것이 결과치를 해석하는 방법입니다

# full 모델과 reduced 모델로 위 두 환자 사망율을 예측하여 보았는데, 
# full model에서 환자1 사망율은 29.89%, reduced model에서 환자 1의 28.88%로 큰 차이가 없다.
# 생존, 사망의 범주형으로 답하자면
# 환자1은 full 모델, reduced 모델에 사용여부에 상관없이 생존, 환자2는 생존할 것으로 추정한다.

predict(haber_glm, newdata = new_patients, type = 'response')
> predict(haber_glm, newdata = new_patients, type = 'response')
        1         2 
0.2988848 0.8448620  

predict(haber_rmglm, newdata = new_patients, type = 'response')
> predict(haber_rmglm, newdata = new_patients, type = 'response')
        1         2 
0.2888020 0.8402924 

# 이런 결과를 검증하기 위해 full model과 reduced 모델의 deviance를 비교할 있는데,
deviance(haber_glm)/deviance(haber_rmglm) ==> 0.9998347로
# 잔차제곱합은 미세하게 증가하였음을 알 수 있으나, 99.98%에 이므로 있으므로 거의 동일한 잔차제곱합
# 결과를 산출하므로 변수 개수가 적은 모델을 이 경우 다수 낫다고 할 수 있다. 
