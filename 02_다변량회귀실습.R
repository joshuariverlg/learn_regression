# 다변량 회귀 실습
# 
# 한 방송국에서 65세 이상 시청자들을 위한 TV 프로그램을 개발하기로 하였다. 이를 위한 기초정보를 획득하기 위해 
# 25명의 시청자들을 대상으로 설문조사를 하여 다음의 4가지 변수에 관한 자료를 다음과 같이 수집하였다. 
# (data file. 4. 다중회귀분석.sav)
# Q: 동거여부, 연령, 교육기간은 TV 시청시간에 영향을 미치는가?

rm(list=ls())

library(dplyr)
library(haven)
library(psych)



setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/실습자료")

df.a <- read_spss("2. 다중회귀분석 실습.sav")

df.a.m <- df.a[,c(3:6)]
df.a.m <- na.omit(df.a.m)
describe(df.a.m)


library(Hmisc)

df.cor <- cor(df.a.m)
round(df.cor, 3)

df.rcorr <- rcorr(as.matrix(df.a.m))
df.rcorr

# 독립변수와 상관관계가 높은 것을 검증한 결과 교육기간과 연령이 -0.501로 다소 높은 상관관계를 보였다.
# 보통 상관관계가 0.8을 넘으면 다중공선성을 의심해 보아야 한다.


# SPSS 회귀분석과 비교하면, 1. 모형요약,  2. ANOVA  3.비표준화 계수에 의한 회귀식 도출
# 더불어 회귀 표준화 잔차의 정규성과 산점도를 검증하면 됨.

df.a.reg <- lm(시청시간 ~ 동거여부 + 연령 + 교육기간,data = df.a)
summary(df.a.reg)



# R : R²의 제곱근으로 상관계수(Correlation coefficient)임.
# R² : 결정계수(coefficient of determination)라고 불림.   종속변수의 분산 중 몇 %가 독립변수에 의해 설명되는가를 
# 나타내며, 0과 1사이의 값을 지님.  교육기간, 동거여부, 연령 3개의 변수를 투입한 결과 R² 이 .6256으로 종속변수를 
# 62.6%설명하고 있다.  adj R² (수정된 R²)은 독립변수의 수와 표본의 크기를 반영한 것으로 .572로 나타났다. 
# R²은 독립변수가 증가할수록 커지나, 설명력이 작으면 adj R²는 오히려 줄어든다.



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
durbinWatsonTest(df.a.reg)

# 위 식의 결과값에서 D-W Statistic 이 더빈왓슨 값임. (1.310978). 더빈왓슨값이 2에 가까우므로 독립적이라 할 수 있음.


# df.a.aov <- anova(df.a.reg)
anova(df.a.reg)

# 분산분석은 회귀식 자체가 유의한지 판단함. F=367.64,
# p = 0.001로 이 회귀분석은 통계적으로 유의하다고 판단할 수 있음.
# R² = 회귀계수의 합 (16.084 + 14.764 + 5.310) / 회귀계수의 합 + 회귀잔차 (= 16.084 + 14.764 + 5.310 + 116.445) = 0.2369416
# 브랜드(brand)는 p값이 0.05보다 크므로 계산에서 제외

reg.res <- 8.6400 + 6.0468 + 5.2459  # 회귀계수. 회귀식에 의해 설명되는 분산 (SSR)
reg.sumsq <- 11.9273 # 회귀잔차. 회귀식에 의해 설명되지 않는 분산(SSE)
reg.res / (reg.res+ reg.sumsq) # R²

# F = 6.644233 / 0.5684 = 11.68936

((8.6400 + 6.0468 + 5.2459)/3)/0.5684 # F값

summary(df.a.reg) # 비표준화 계수에 의한 회귀분석 결과

# 회귀식에서의 표준화 계수와 비표준화 계수
# 회귀방정식을 구하기 위해서는 표준화 계수와 비표준화 계수를 사용할 수 있다.
# 표준화 계수는 다중회귀 분석에서 사용된다.  예를 들어 측정 데이터가 cm 과 kg 와 같은 다른 단위의 데이터가 사용될 경우
# 각 단위를 통일시킬 필요가 있다.  그러므로 단위를 통일시킨 표준화 계수가 필요하다.

df.a.beta <- lm.beta(df.a.reg)
summary(df.a.beta)


# 분석결과 비표준화계수(B)에 의해 다음과 같은 회귀식이 도출
# Y(시청시간) = 1.49526 - 1.17573X(동거여부) + 0.03876X(연령) - 0.15228X(교육기간)
# 동거여부, 연령, 교육기간에 대한 t값은 각각 -3.726, 1.214, -3.039 이고 p값은 연령을 제외하고 둘 다 0.05 보다 작으므로
# 귀무가설은 기각되고 연구가설은 지지된다. 따라서 사람의 동거여부와 교육기간은 시청시간제에 부의 영향을 미친다고
# 볼 수 있다.   
# 독립변수 영향력의 상대적 크기는 표준화 계수를 활용한다. (독립변수들의 단위가 다르기 때문에 표준화를 시켜줌)
# 표준화 계수의 절대값이 큰 순서대로 영향력을 미친다. (동거여부 > 교육기간 > 연령 순)

# 동거여부는 다른 두 변수(연령, 교육기간)가 회귀식에 포함되어 있는 경우 유의미하다 (t=-3.726, p=.001).
# 동거여부는 시청시간에 부적 영향을 미친다.

# 연령은 다른 두 변수가 회귀식에 포함되어 있는 경우 유의미하지 않다 (t=1.214, p=.238).
# 연령은 시청시간에 영향을 미치지 못한다.

# 교육기간은 다른 두 변수가 회귀식에 포함되어 있는 경우 유의미하다 (t=-3.039, p=.006).
# 교육기간은 시청시간에 부적 영향을 미친다


# 다중공선성(multicol) 진단
# 고유값 (eigenvalue) : vif() 사용
# 상태지수 (condition number) : kappa() 사용

vif(df.a.reg)  #  고유값


library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.

ols_coll_diag(df.a.reg)


# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄







# 03. 회귀분석 가정 검정
# 등분산성: Scale-Location, ncvTest
# 정규성: Nomal Q-Q, shapiro.test
# 선형성: Residuals vs Fitted, 
# 독립성: durbinWatsonTest
# 이상치검정 : Residuals vs Leverage(cook's distance) 4/n-k-1
# 그림으로 가정 검정
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(df.a.reg)

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

tmp1 <- resid(df.a.reg)
tmp2 <- pnorm( tmp1, 0, summary(df.a.reg)$sigma )

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
resid(df.a.reg)

#표준화 잔차 구하기
rstudent(df.a.reg)

#표준화 잔차 표 구하기
plot(rstudent(df.a.reg), main = "산점도")






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

durbinWatsonTest(df.a.reg)

# 위 식의 결과값에서 D-W Statistic 이 더빈왓슨 값임. (1.310978). 



# 잔차의 정규분포 검정 

shapiro.test(df.a.reg$residuals)

# Shapiro Wilks 검정의 유의 확률 (p-value) 이 0.05보다 커서 (0.1074) 영가설을 기각하므로, 잔차는 정규성을 가진다고
# 가정할 수 있음.



# 수치로 가정 검정
# 잔차의 등분산성 검정 
ncvTest(df.a.reg)


#이상치 검정, sd, hat, d 통합검정
influencePlot(df.a.reg, id.method="identify")




