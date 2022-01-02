

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data"

haberman <- read.csv(URL, header = FALSE)

names(haberman) = c('age', 'op_year', 'no_nodes', 'survival')

haberman$survival <- factor(haberman$survival)

str(haberman$survival)


haber_glm <- glm(survival ~ age + op_year + no_nodes, data = haberman, family = binomial)


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

new_patients <- data.frame(age = c(57, 66), op_year = c(58, 60), no_nodes = c(5, 32)) 
predict(haber_glm, newdata = new_patients, type = 'response')

> predict(haber_glm, newdata = new_patients, type = 'response')
        1         2  # <- 환자1, 환자2
0.2988848 0.8448620  # <- 사망확률


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


# 두 모델을 비교 시, 이탈도가 작아짐.
AIC=336.26 에서 AIC: 334.31로 작아져 이탈도가 작아짐을 알 수 있다.


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
