# 다변량 회귀분석 과제2
# 후진방식을 사용

# 
# 조직 구성원들의 직무 스트레스가 직무탈진에 미치는 영향을 알아보고자 직무책임, 직무복잡성, 업무속도, 관리복잡성, 대인갈등, 
# 역할갈등 등 6개 직무스트레스 항목들을 각각 3문항씩 설문조사를 하였다. 직무 스트레스에 해당하는 6개 항목들이 직무탈진에 
# 어떤 영향을 미치는지 알아 보고자 한다. (data > 과제 > 2.다중회귀분석과제.sav) 
# ※ 후진 방식을 사용해서 분석하시오.


rm(list=ls())

library(dplyr)
library(haven)
library(psych)



setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/과제")

df <- read_spss("2. 다중회귀분석 과제.sav")
df <- mutate(df, 직무책임 = (직무책임1 + 직무책임2 + 직무책임3)/3, 직무복잡성 = (직무복잡성1 + 직무복잡성2 + 직무복잡성3)/3,
             업무속도 = (업무속도1 + 업무속도2 + 업무속도3)/3,관리복잡성 = (관리복잡성1 + 관리복잡성2 + 관리복잡성3)/3,
             대인갈등 = (대인갈등1 + 대인갈등2 + 대인갈등3)/3, 역할갈등 = (역할갈등1 + 역할갈등2 + 역할갈등3)/3,
             직무탈진 = (직무탈진1 + 직무탈진2 + 직무탈진3 + 직무탈진4 + 직무탈진5)/5)

df.a <- df[,c(24:30)]
df.a <- na.omit(df.a)
describe(df.a)

df.a.reg <- lm(직무탈진 ~ 직무책임 + 직무복잡성 + 업무속도 + 관리복잡성 + 대인갈등 + 역할갈등,data = df.a)
summary(df.a.reg)


# Stepwise regression model
library(MASS)
step.model <- stepAIC(df.a.reg, direction = "backward", 
                      trace = TRUE)
summary(step.model)


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
durbinWatsonTest(step.model)

  # 위 식의 결과값에서 D-W Statistic 이 더빈왓슨 값임. (1.781414). 더빈왓슨값이 2에 가까우므로 독립적이라 할 수 있음.


# df.a.aov <- anova(step.model)
anova(step.model)

# 분산분석은 회귀식 자체가 유의한지 판단함. 
# 직무책임과, 직무복잡성은 P값이 0.05보다 크므로 회귀분석 독립변수에서 제외하고 다시 회귀식을 구성

reg.res <- 53.529 + 11.764 + 13.460 + 3.064 + 0.890  # 회귀계수. 회귀식에 의해 설명되는 분산 (SSR)
reg.sumsq <- 170.792 # 회귀잔차. 회귀식에 의해 설명되지 않는 분산(SSE)
reg.res / (reg.res+ reg.sumsq) # R²

# F = 16.5414 / 0.558 = 29.64409

((0.890 + 53.529 + 11.764 + 13.460 + 3.064)/5)/0.558 # F값

summary(step.model) # 비표준화 계수에 의한 회귀분석 결과

# 회귀식에서의 표준화 계수와 비표준화 계수
# 회귀방정식을 구하기 위해서는 표준화 계수와 비표준화 계수를 사용할 수 있다.
# 표준화 계수는 다중회귀 분석에서 사용된다.  예를 들어 측정 데이터가 cm 과 kg 와 같은 다른 단위의 데이터가 사용될 경우
# 각 단위를 통일시킬 필요가 있다.  그러므로 단위를 통일시킨 표준화 계수가 필요하다.

library(lm.beta)
df.a.beta <- lm.beta(step.model)
summary(df.a.beta)


# 분석결과 비표준화계수(B)에 의해 다음과 같은 회귀식이 도출
# Y(직무탈진) = 0.63846 + - 0.14002X(직무복잡성) + 0.43192X(업무속도) + 0.13696X(관리복잡성) + 0.26224X(대인갈등) 
# + 0.15696X(역할갈등)
# 직무복잡성,업무속도, 관리복잡성, 대인갈등, 역할갈등 t값은 각각 -2.310, 6.574, 2.360, 3.922, 2.343 이고 p값은 모두 0.05 보다 
# 작으므로 귀무가설은 기각되고 연구가설은 지지된다. 따라서 직무복잡성,업무속도, 관리복잡성, 대인갈등, 역할갈등은 직무탈진에 
# 정의 영향을 미친다고 볼 수 있다.   
# 독립변수 영향력의 상대적 크기는 표준화 계수를 활용한다. (독립변수들의 단위가 다르기 때문에 표준화를 시켜줌)
# 표준화 계수의 절대값이 큰 순서대로 영향력을 미친다. (업무속도 > 대인갈등 > 역할갈등 > 관리복잡성 > 직무복잡성  순)



# 다중공선성(multicol) 진단
# 고유값 (eigenvalue) : vif() 사용
# 상태지수 (condition number) : kappa() 사용

vif(step.model)  #  고유값

# VIF : 10 이하, 공차 : 0.1 이상인 경우에는 다중공선성에 문제가 없는 것으로 판단

library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.


ols_coll_diag(step.model)


# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄







# 03. 회귀분석 가정 검정
# 등분산성: Scale-Location, ncvTest
# 정규성: Nomal Q-Q, shapiro.test
# 선형성: Residuals vs Fitted, 
# 독립성: durbinWatsonTest
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1
# 그림으로 가정 검정
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(step.model)

par(opar)  #원상태로 돌림


# Residuals vs Fitted : 점들이 좌우로 등분산을 이루고 있으면 좋은 형태. 만일 잔차에 일정한 패턴이 있다면 모델을 수정해야 함.
#                        (잔차의 등분산성 가정 충족)
# Normal Q-Q :  잔차가 대각선상에 직선형태에 수렴하면 좋은 형태 (정규성 가정 충족)
# Scales-Location : 기울기가 0이면 이상적
# Residuals vs Leverage : 왼쪽 가운데 몰려 있으면 이상적.
# 단순회귀에서는 통상 잔차의 정규성과 등분산성 가정만 확인하면 됨.


# p-p (probability plot graph) 도표 그리기
# https://stat.ethz.ch/pipermail/r-help/2007-September/141873.html
# 현재에는 PP 그래프보다 QQ그래프를 더 많이 사용한다. QQ그래프가 각 포인트들에 대한 더 많은 흥미있는 점들을 제공하기 때문. 

tmp1 <- resid(step.model)
tmp2 <- pnorm( tmp1, 0, summary(step.model)$sigma )

par(mfrow=c(2,1))
qqnorm(tmp1)
qqline(tmp1)

plot( ppoints(length(tmp1)), sort(tmp2), xlab='Theoretical Percentiles(관측누적확률)',
      ylab='Sample Percentiles(기대 누적 확률)', main = "PP Plot(회귀 표준화 잔차의 정규 PP 도표)")
abline(0,1)

# 정규분포는 대각선 형태로 그려지고, 점들의 분포가 대각선에 가까울 수록 정규성 가정을 충족 시킨다. 
# 따라서 오차의 정규성 가정을 충족시킨다고 볼 수 있다.




# 회귀 표준화 잔차는 회귀 표준화 예측값과 회귀 표준화 잔차를 비교함으로서 확인할 수 있음.
# 위 Residuals vs Fitted 도표에서 보면 잔차가 0을 중심으로 대체로 무작위로 분포되어 있으며 특정 패턴이 나타나지 않고 있다. 
# 따라서 오차의 독립성 가정과 등분산 가정을 충족시키고 있다.


# 잔차, 표준화 잔차를 구하기 위해서는 아래 함수를 사용할 수 있다.
# 잔차 구하기
resid(step.model)

#표준화 잔차 구하기
rstudent(step.model)

#표준화 잔차 표 구하기
plot(rstudent(step.model), main = "산점도")






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

durbinWatsonTest(step.model)

# 위 식의 결과값에서 D-W Statistic 이 더빈왓슨 값임. (1.781414). 



# 잔차의 정규분포 검정 

shapiro.test(step.model$residuals)

# Shapiro Wilks 검정의 유의 확률 (p-value) 이 0.05보다 커서 (0.2311) 영가설을 기각하므로, 잔차는 정규성을 가진다고
# 가정할 수 있음.



# 수치로 가정 검정
# 잔차의 등분산성 검정 .  p값이 0.05보다 크면 등분산성 인정
ncvTest(step.model)


#이상치 검정, sd, hat, d 통합검정
influencePlot(step.model, id.method="identify")





