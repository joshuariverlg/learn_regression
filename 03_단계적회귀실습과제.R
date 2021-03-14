## 단계적 회귀분석 과제

## 단계적 회귀분석의 개념

# 독립변수를 모두 투입하기는 하지만,
# 연구자의 설정에 따라 종속변수에 영향을 미치는 요인과 영향을 미치지 않는 요인을 구분하여 영향이 있는 독립변수만 나타내는 방법


# 조직 구성원들의 직무 스트레스가 직무탈진에 미치는 영향을 알아보고자 직무책임, 직무복잡성, 업무
# 속도, 관리복잡성, 대인갈등, 역할갈등 등 6개 직무스트레스 항목들을 각각 3문항씩 설문조사를 하였다. 
# 직무 스트레스에 해당하는 6개 항목들이 직무탈진에 어떤 영향을 미치는지 알아 보고자 한다.
# (data > 과제 > 3.단계적회귀분석과제.sav)
# ※ 동시입력 방식, 단계적 입력방식, 후진입력방식을 사용해서 비교 분석하시오



rm(list=ls())

library(dplyr)
library(haven)
library(psych)


setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/과제")

df <- read_spss("3. 단계적회귀분석 과제.sav")

df <- mutate(df, 직무책임 = (직무책임1 + 직무책임2 + 직무책임3)/3,
             직무복잡성 = (직무복잡성1 + 직무복잡성2 + 직무복잡성3)/3,
             업무속도 = (업무속도1 + 업무속도2 + 업무속도3)/3,
             관리복잡성 = (관리복잡성1 + 관리복잡성2 + 관리복잡성3)/3,
             대인갈등 = (대인갈등1 + 대인갈등2 + 대인갈등3)/3,
             역할갈등 = (역할갈등1 + 역할갈등2 + 역할갈등3)/3,
             직무탈진 = (직무탈진1 + 직무탈진2 + 직무탈진3 + 직무탈진4 + 직무탈진5)/5)
df.a <- df[,c(24:30)]

describe(df.a)

df.a.reg <- lm(직무탈진 ~ 직무책임 + 직무복잡성 + 업무속도 + 관리복잡성 + 대인갈등 + 역할갈등,data = df.a)  
#입력방식(전 독립변수를 모두 투입)

summary(df.a.reg)

# R² 구하기
reg.res <- 0.154 + 0.736 + 54.233 + 12.663 + 12.982 + 2.728  # 회귀계수. 회귀식에 의해 설명되는 분산 (SSR)
reg.sumsq <- 170.004 # 회귀잔차. 회귀식에 의해 설명되지 않는 분산(SSE)
reg.res / (reg.res+ reg.sumsq) # R²


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
durbinWatsonTest(df.a.reg)   # Durbin-Watson 값은 1.866로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.

anova(df.a.reg)

# 분산분석은 회귀식 자체가 유의한지 판단함. 

meansq <- (0.154 + 0.736 + 54.233 + 12.663 + 12.982 + 2.728)/6  #평균제곱 값
rsd <- 0.557
meansq/rsd # F값
(0.2763 + 1.3198 + 97.2972 + 22.7192 + 23.2901 + 4.8946)/6   # 동일하게 F값

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

# 입력방식을 사용한 회귀분석 결과 직무탈진(Y) = 0.85197 + 0.43819*업무속도 + 0.14745*관리복잡성 + 0.26050*대인갈등
# + 0.14886*역할갈등으로 나타낼 수 있으며, 참여지속의도에 영향을 미치는 변수는 
# (업무속도 > 대인갈등 > 역할갈등 > 관리복잡성) 의 순서로 나타남.





## Stepwise regression model
library(MASS)


## 단계선택법 적용
stepAIC(df.a.reg, direction = "both", trace = TRUE)  # 단계선택법
step.model.stepwise <- stepAIC(df.a.reg, direction = "both", trace = TRUE)  # 단계선택법
step.model.stepwise
summary(step.model.stepwise)


## leaps 패키지를 사용한 단계선택법 적용방법 확인

library(leaps)
step.model.step2 <- regsubsets(직무탈진~.,data = df.a, nvmax = 6, method = "seqrep")
summary(step.model.step2)


durbinWatsonTest(step.model.stepwise)   # Durbin-Watson 값은 1.830으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.


step.model1 <- stepAIC(lm(직무탈진~업무속도, data = df.a), direction = "both", trace = TRUE)
step.model2 <- stepAIC(lm(직무탈진~업무속도 + 대인갈등, data = df.a), direction = "both", trace = TRUE)
step.model3 <- stepAIC(lm(직무탈진~업무속도 + 대인갈등 + 관리복잡성, data = df.a), direction = "both", trace = TRUE)
step.model4 <- stepAIC(lm(직무탈진~업무속도 + 관리복잡성 + 직무책임 + 직무복잡성, data = df.a), direction = "both", trace = TRUE)
step.model5 <- stepAIC(lm(직무탈진~업무속도 + 관리복잡성 + 직무복잡성 + 역할갈등 + 대인갈등, data = df.a), direction = "both", trace = TRUE)
step.model6 <- stepAIC(lm(직무탈진~업무속도 + 관리복잡성 + 직무복잡성 + 역할갈등 + 대인갈등 + 직무책임, data = df.a), direction = "both", trace = TRUE)


anova(step.model1) # step.model1 에 대한 분산분석 결과
anova(step.model2) # step.model2 에 대한 분산분석 결과
anova(step.model3) # step.model3 에 대한 분산분석 결과
anova(step.model4) # step.model3 에 대한 분산분석 결과
anova(step.model5) # step.model3 에 대한 분산분석 결과
anova(step.model6) # step.model3 에 대한 분산분석 결과


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


summary(step.model4) # step.model4 에 대한 비표준화 계수에 의한 회귀분석 결과

step.model4.beta <- lm.beta(step.model4) # 표준화 계수 결과 보기기
summary(step.model4.beta)

# 다중공선성(multicol) 진단
ols_coll_diag(step.model4)


summary(step.model5) # step.model5 에 대한 비표준화 계수에 의한 회귀분석 결과

step.model5.beta <- lm.beta(step.model5) # 표준화 계수 결과 보기기
summary(step.model5.beta)

# 다중공선성(multicol) 진단
ols_coll_diag(step.model5)


summary(step.model6) # step.model6 에 대한 비표준화 계수에 의한 회귀분석 결과

step.model6.beta <- lm.beta(step.model6) # 표준화 계수 결과 보기기
summary(step.model6.beta)

# 다중공선성(multicol) 진단
ols_coll_diag(step.model6)


# VIF 는 10 미만이면 다중공선성에 문제가 없다고 판단.
# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄



# 회귀 표준화 잔차는 회귀 표준화 예측값과 회귀 표준화 잔차를 비교함으로서 확인할 수 있음.
# 위 Residuals vs Fitted 도표에서 보면 잔차가 0을 중심으로 대체로 무작위로 분포되어 있으며 특정 패턴이 나타나지 않고 있다. 
# 따라서 오차의 독립성 가정과 등분산 가정을 충족시키고 있다.



## 잔차 통계량 표 구하기 (여기에서는 step.model5에 대한 잔차 통계량표를 구함)

# 잔차 통계량 표는 종속변수를 기준으로 종속변수의 예측값(통계량), 잔차, 표준화예측값, 표준화 잔차를 순차적으로 보여주는 도표임.
# 먼저 종속변수의 예측 값을 re.a 로 두고

re.a <- describe(step.model5$fitted.values)

# 다음은 잔차 통계량을 구하기 위해 잔차 통례량을 re.b 로 두고 잔차를 구함
# 잔차 구하기
re.b <- describe(resid(step.model5))

# 다음은 표준화 예측값 통계량을 구하기 위해 잔차 통례량을 re.c 로 두고 표준화 예측 값 통계량을 구함

re.c <- describe(scale(step.model5$fitted.values))


#표준화 잔차(re.d) 구하기.  표준화 잔차를 구하기 위해서는 아래 함수를 사용할 수 있다.
re.d <- describe(rstudent(step.model5))

#표준화 잔차 표 구하기
plot(rstudent(step.model5), main = "산점도")

# 표준화 잔차 표 구하기

re.table <- rbind(re.a, re.b, re.c, re.d) #
re.table  # 표준화 잔차 표. 순서대로, 예측값, 잔차, 표준화 예측값, 표준화 잔차 통계량임. (SPSS 표준화 잔차표)


library(lm.beta)
df.a.beta <- lm.beta(step.model5)
summary(df.a.beta)

# 단계선택방식을 사용한 회귀분석 결과 직무탈진(Y) = 4.7516 + 1.7470*대인갈등  + 2.0807*관리복잡성으로 나타낼 수
# 있으며, 직무탈진에 영향을 미치는 변수는 (관리복잡성 > 대인갈등) 의 순서로 나타남.




## 후진방식 적용

step.model.backward <- stepAIC(df.a.reg, direction = "backward", 
                               trace = TRUE)   # 후진방식
summary(step.model.backward)
summary(df.a.reg) # 모든 독립변수 입력된 회귀분석 결과와 비교

durbinWatsonTest(step.model.backward)   # Durbin-Watson 값은 1.830로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.

anova(df.a.reg)  # 모든 독립변수 입력된 회귀분석에 대한 분산분석식

(97.2972 + 22.7192 + 23.2901 + 4.8946)/4   # F값

anova(step.model.backward)

(1.5937 + 95.9059 + 21+0774 + 27.1150 + 5.4904)/5  # F값


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

# 후진방식을 사용한 회귀분석 결과 직무탈진(Y) = 0.63846 - 0.14002*직무복잡성 + 0.26224*대인갈등  + 0.13696*관리복잡성
# + 0.43192*업무속도 + 0.15696*역할갈등으로 나타낼 수
# 있으며, 직무탈진에 영향을 미치는 변수는 (업무속도  > 대인갈등 > 역할갈등 > 직무복잡성 > 관리복잡성) 의 순서로 나타남.
