## 단계적 회귀분석

## 단계적 회귀분석의 개념

# 독립변수를 모두 투입하기는 하지만,
# 연구자의 설정에 따라 종속변수에 영향을 미치는 요인과 영향을 미치지 않는 요인을 구분하여 영향이 있는 독립변수만 나타내는 방법





rm(list=ls())

library(dplyr)
library(haven)
library(psych)


setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/실습자료2")

df <- read_spss("5. 단계적 회귀분석.sav")
df <- mutate(df, 외관 = (외관1+외관2+외관3)/3, 편의성 = (편의성1+편의성2+편의성3+편의성4)/4,
               유용성 = (유용성1+유용성2+유용성3+유용성4+유용성5)/5, 만족감 = (만족감1+만족감2+만족감3)/3,
               브랜드 = (브랜드1+브랜드2+브랜드3+브랜드4+브랜드5)/5)
df.a <- df[,c(21:25)]

describe(df.a)

df.a.reg <- lm(만족감 ~ 외관 + 편의성 + 유용성 + 브랜드,data = df.a)  #입력방식(전 독립변수를 모두 투입)
summary(df.a.reg)

# 오차의 독립성
# 회귀분석에서 종속변수는 독립성의 가정이 존재함
# 종속변수 자체에 상관관계가 존재하면 독립성 가정에 위배되어 회귀분석을
# 이를 확인하기 위해 회귀분석에서는 종속변수 오차항의 자기상관(autocorrelation)을 사용함.
# 자기상관을 측정하는 대표적인 방법: Durbin-Watson (값의 범위: 0~4)

# Durbin-Watson 지수의 해석
# 0에 가까우면 양의 자기상관
# 4에 가까우면 음의 자기상관
# 2에 가까우면 회귀방정식에 예측된 종속변수의 오차항은 자기상관이 없이 서로 독립적. 
# 구체적으로 1.8 < Durbin-Watson < 2.2 경우 독립적 자기상관을 갖는다고 볼 수 있다.

library(car)
durbinWatsonTest(df.a.reg)   # Durbin-Watson 값은 1.831로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.

anova(df.a.reg)

# 분산분석은 회귀식 자체가 유의한지 판단함. 


reg.res <- 16.084 + 14.764 + 5.310 + 0.007  # 회귀계수. 회귀식에 의해 설명되는 분산 (SSR)
reg.sumsq <- 116.445 # 회귀잔차. 회귀식에 의해 설명되지 않는 분산(SSE)
reg.res / (reg.res+ reg.sumsq) # R²

# F = 16.5414 / 0.558 = 29.64409

meansq <- (16.0842 + 14.7638 + 5.3102 + 0.0068)/4  #평균제곱 값
((16.0842 + 14.7638 + 5.3102 + 0.0068)/4)/0.3639 # F값
(44.2005 + 40.5720 + 14.5929 + 0.0187)/4   # 동일하게 F값

summary(df.a.reg) # 비표준화 계수에 의한 회귀분석 결과

# 회귀식에서의 표준화 계수와 비표준화 계수
# 회귀방정식을 구하기 위해서는 표준화 계수와 비표준화 계수를 사용할 수 있다.
# 표준화 계수는 다중회귀 분석에서 사용된다.  예를 들어 측정 데이터가 cm 과 kg 와 같은 다른 단위의 데이터가 사용될 경우
# 각 단위를 통일시킬 필요가 있다.  그러므로 단위를 통일시킨 표준화 계수가 필요하다.



# 다중공선성(multicol) 진단
# 고유값 (eigenvalue) : vif() 사용
# 상태지수 (condition number) : kappa() 사용

vif(df.a.reg)  #  고유값

# VIF : 10 이하, 공차 : 0.1 이상인 경우에는 다중공선성에 문제가 없는 것으로 판단

library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.


ols_coll_diag(df.a.reg)


# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄


# 회귀 표준화 잔차는 회귀 표준화 예측값과 회귀 표준화 잔차를 비교함으로서 확인할 수 있음.
# 위 Residuals vs Fitted 도표에서 보면 잔차가 0을 중심으로 대체로 무작위로 분포되어 있으며 특정 패턴이 나타나지 않고 있다. 
# 따라서 오차의 독립성 가정과 등분산 가정을 충족시키고 있다.


## 잔차 통계량 표 구하기

# 잔차 통계량 표는 종속변수를 기준으로 종속변수의 예측값(통계량), 잔차, 표준화예측값, 표준화 잔차를 순차적으로 보여주는 도표임.
# 먼저 종속변수의 예측 값을 re.a 로 두고

re.a <- describe(df.a.reg$fitted.values)

# 다음은 잔차 통계량을 구하기 위해 잔차 통례량을 re.b 로 두고 잔차를 구함
# 잔차 구하기
re.b <- describe(resid(df.a.reg))

# 다음은 표준화 예측값 통계량을 구하기 위해 잔차 통례량을 re.c 로 두고 표준화 예측 값 통계량을 구함

re.c <- describe(scale(df.a.reg$fitted.values))


#표준화 잔차(re.d) 구하기.  표준화 잔차를 구하기 위해서는 아래 함수를 사용할 수 있다.
re.d <- describe(rstudent(df.a.reg))

#표준화 잔차 표 구하기
plot(rstudent(df.a.reg), main = "산점도")

# 표준화 잔차 표 구하기

re.table <- rbind(re.a, re.b, re.c, re.d)
re.table  # 표준화 잔차 표. 순서대로, 예측값, 잔차, 표준화 예측값, 표준화 잔차 통계량임. (SPSS 표준화 잔차표)



library(lm.beta)
df.a.beta <- lm.beta(df.a.reg)
summary(df.a.beta)

# 입력방식을 사용한 회귀분석 결과 만족감(Y) = 1.481782 + 0.144240*외관  + 0.284266*편의성 + 0.174021*유용성으로 나타낼 수
# 있으며, 만족감에 영향을 미치는 변수는 (편의성 > 유용성 > 외관) 의 순서로 나타남.





## Stepwise regression model
library(MASS)


## 단계선택법 적용
stepAIC(df.a.reg, direction = "both", trace = TRUE)  # 단계선택법
step.model.stepwise <- stepAIC(df.a.reg, direction = "both", trace = TRUE)  # 단계선택법
step.model.stepwise
summary(step.model.stepwise)

## leaps 패키지를 사용한 단계선택법 적용

# library(leaps)
# step.model.step2 <- regsubsets(만족감~.,data = df.a, nvmax = 4, method = "seqrep")
# summary(step.model.step2)


durbinWatsonTest(step.model.stepwise)   # Durbin-Watson 값은 1.830으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.

step.model1 <- stepAIC(lm(만족감~편의성, data = df.a), direction = "both", trace = TRUE)
step.model2 <- stepAIC(lm(만족감~편의성 + 외관, data = df.a), direction = "both", trace = TRUE)
step.model3 <- stepAIC(lm(만족감~편의성 + 외관 + 유용성, data = df.a), direction = "both", trace = TRUE)


anova(step.model1) # step.model1 에 대한 분산분석 결과
anova(step.model2) # step.model2 에 대한 분산분석 결과
anova(step.model3) # step.model3 에 대한 분산분석 결과


summary(step.model1) # step.model1 에 대한 비표준화 계수에 의한 회귀분석 결과

# 독립변수가 하나 밖에 없으므로 다중공선성 진단이 필요가 없음. (공선성 통계량을 모두 1)

# 회귀식에서의 표준화 계수와 비표준화 계수
# 회귀방정식을 구하기 위해서는 표준화 계수와 비표준화 계수를 사용할 수 있다.
# 표준화 계수는 다중회귀 분석에서 사용된다.  예를 들어 측정 데이터가 cm 과 kg 와 같은 다른 단위의 데이터가 사용될 경우
# 각 단위를 통일시킬 필요가 있다.  그러므로 단위를 통일시킨 표준화 계수가 필요하다.

step.model1.beta <- lm.beta(step.model1)
summary(step.model1.beta)


summary(step.model2) # step.model2 에 대한 비표준화 계수에 의한 회귀분석 결과

step.model2.beta <- lm.beta(step.model2) # 표준화 계수 결과 보기기
summary(step.model2.beta)

# 다중공선성(multicol) 진단
# 고유값 (eigenvalue) : vif() 사용
library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.
ols_coll_diag(step.model2)


summary(step.model3) # step.model3 에 대한 비표준화 계수에 의한 회귀분석 결과

step.model3.beta <- lm.beta(step.model3) # 표준화 계수 결과 보기기
summary(step.model3.beta)

# 다중공선성(multicol) 진단
ols_coll_diag(step.model3)


# VIF 는 10 미만이면 다중공선성에 문제가 없다고 판단.
# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄



# 회귀 표준화 잔차는 회귀 표준화 예측값과 회귀 표준화 잔차를 비교함으로서 확인할 수 있음.
# 위 Residuals vs Fitted 도표에서 보면 잔차가 0을 중심으로 대체로 무작위로 분포되어 있으며 특정 패턴이 나타나지 않고 있다. 
# 따라서 오차의 독립성 가정과 등분산 가정을 충족시키고 있다.


## 잔차 통계량 표 구하기 (여기에서는 step.model3에 대한 잔차 통계량표를 구함)

# 잔차 통계량 표는 종속변수를 기준으로 종속변수의 예측값(통계량), 잔차, 표준화예측값, 표준화 잔차를 순차적으로 보여주는 도표임.
# 먼저 종속변수의 예측 값을 re.a 로 두고

re.a <- describe(step.model3$fitted.values)

# 다음은 잔차 통계량을 구하기 위해 잔차 통례량을 re.b 로 두고 잔차를 구함
# 잔차 구하기
re.b <- describe(resid(step.model3))

# 다음은 표준화 예측값 통계량을 구하기 위해 잔차 통례량을 re.c 로 두고 표준화 예측 값 통계량을 구함

re.c <- describe(scale(step.model3$fitted.values))


#표준화 잔차(re.d) 구하기.  표준화 잔차를 구하기 위해서는 아래 함수를 사용할 수 있다.
re.d <- describe(rstudent(step.model3))

#표준화 잔차 표 구하기
plot(rstudent(step.model3), main = "산점도")

# 표준화 잔차 표 구하기

re.table <- rbind(re.a, re.b, re.c, re.d) #
re.table  # 표준화 잔차 표. 순서대로, 예측값, 잔차, 표준화 예측값, 표준화 잔차 통계량임. (SPSS 표준화 잔차표)


library(lm.beta)
df.a.beta <- lm.beta(step.model3)
summary(df.a.beta)

# 입력방식을 사용한 회귀분석 결과 만족감(Y) = 1.45847 + 0.14443*외관  + 0.28385*편의성 + 0.17370*유용성으로 나타낼 수
# 있으며, 만족감에 영향을 미치는 변수는 (편의성 > 유용성 > 외관) 의 순서로 나타남.




## 후진방식 적용

step.model.backward <- stepAIC(df.a.reg, direction = "backward", 
                               trace = TRUE)   # 후진방식
summary(step.model.backward)
summary(df.a.reg) # 모든 독립변수 입력된 회귀분석 결과와 비교

durbinWatsonTest(step.model.backward)   # Durbin-Watson 값은 1.830로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.

anova(df.a.reg)  # 모든 독립변수 입력된 회귀분석에 대한 분산분석식

(44.2005 + 40.5720 + 14.5929 + 0.0187)/4   # F값

anova(step.model.backward)

(44.336 + 40.696 + 14.638)/3  # F값


summary(df.a.reg)  # 모든 독립변수 입력된 회귀분석 결과
summary(step.model.backward) # 비표준화 계수에 의한 후진 회귀분석 결과

# 회귀식에서의 표준화 계수와 비표준화 계수
# 회귀방정식을 구하기 위해서는 표준화 계수와 비표준화 계수를 사용할 수 있다.
# 표준화 계수는 다중회귀 분석에서 사용된다.  예를 들어 측정 데이터가 cm 과 kg 와 같은 다른 단위의 데이터가 사용될 경우
# 각 단위를 통일시킬 필요가 있다.  그러므로 단위를 통일시킨 표준화 계수가 필요하다.


# 다중공선성(multicol) 진단
# 고유값 (eigenvalue) : vif() 사용
# VIF : 10 이하, 공차 : 0.1 이상인 경우에는 다중공선성에 문제가 없는 것으로 판단

library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.

ols_coll_diag(df.a.reg)
ols_coll_diag(step.model.backward)  
#  R은  SPSS와 달리 제외된 "브랜드"변수에 대한 다중공선성 값을 보여주지는 않음.


# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄


# 회귀 표준화 잔차는 회귀 표준화 예측값과 회귀 표준화 잔차를 비교함으로서 확인할 수 있음.
# 위 Residuals vs Fitted 도표에서 보면 잔차가 0을 중심으로 대체로 무작위로 분포되어 있으며 특정 패턴이 나타나지 않고 있다. 
# 따라서 오차의 독립성 가정과 등분산 가정을 충족시키고 있다.


## 잔차 통계량 표 구하기

# 잔차 통계량 표는 종속변수를 기준으로 종속변수의 예측값(통계량), 잔차, 표준화예측값, 표준화 잔차를 순차적으로 보여주는 도표임.
# 먼저 종속변수의 예측 값을 re.a 로 두고

re.a <- describe(step.model.backward$fitted.values)

# 다음은 잔차 통계량을 구하기 위해 잔차 통례량을 re.b 로 두고 잔차를 구함
# 잔차 구하기
re.b <- describe(resid(step.model.backward))

# 다음은 표준화 예측값 통계량을 구하기 위해 잔차 통례량을 re.c 로 두고 표준화 예측 값 통계량을 구함

re.c <- describe(scale(step.model.backward$fitted.values))


#표준화 잔차(re.d) 구하기.  표준화 잔차를 구하기 위해서는 아래 함수를 사용할 수 있다.
re.d <- describe(rstudent(step.model.backward))

#표준화 잔차 표 구하기
plot(rstudent(step.model.backward), main = "산점도")

# 표준화 잔차 표 구하기

re.table <- rbind(re.a, re.b, re.c, re.d)
re.table  # 표준화 잔차 표. 순서대로, 예측값, 잔차, 표준화 예측값, 표준화 잔차 통계량임. (SPSS 표준화 잔차표)



library(lm.beta)
df.a.beta <- lm.beta(step.model.backward)
summary(df.a.beta)

# 입력방식을 사용한 회귀분석 결과 만족감(Y) = 1.45847 + 0.14443*외관  + 0.28385*편의성 + 0.17370*유용성으로 나타낼 수
# 있으며, 만족감에 영향을 미치는 변수는 (편의성 > 유용성 > 외관) 의 순서로 나타남.
