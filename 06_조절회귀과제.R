# 조절 회귀분석 (Moderated Regression Analysis) 과제


# 단계적 입력방식과 같은 방식으로 진행

#  1단계 : 종속변수 ← 독립변수
#  2단계 : 종속변수 ← 독립변수, 조절변수
#  3단계 : 종속변수 ← 독립변수, 조절변수, 상호작용변수
# (상호작용변수는 변수계산 메뉴에서 ‘독립변수 x 조절변수로 계산한다)

# 확인은 1. Durbin Watson 값 확인
#        2. 모형1,2,3의 전이에 따라 R2값이 늘어나는가
#        3. 유의확률 F값(Anova 를 하였을 때 추가된 변수에 대한 F value)의 변화량이 유의한가 (0.05보다 작은가)
# 세 가지 조건을 모두 충족시키면 조절효과가 있는 것으로 분석됨.

# 만일 독립변수와 조절변수를 곱한 상호작용 값의 다중 공선성이 우려될 경우에는  Mean Centering 을 실시함.
# Mean Centering 을 하는 방법은 moderate.lm(x, z, y, data, mc = TRUE) 로 mc 옵션을 TRUE 로 주면 됨.



# 관리자의 코칭행동과 경력계획 실천행동 관계에서 조직풍토의 자율성, 지원, 보상 등에 대한 조절효과를 
# 검증하려고 한다. 위계적 회귀분석을 사용해 조절효과를 검증하고, 마지막 장표에 논문 표현방식으로 
# 표를 작성하고 기술하시오. (data>과제>5.조절회귀분석 과제.sav)


rm(list=ls())

library(dplyr)
library(haven)
library(psych)


setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/과제")

df <- read_spss("5. 조절회귀분석 과제.sav")


# MC_코칭, MC_자율 브랜드의 상호작용효과 회귀분석 검증
### QuantPsyc 패키지를 사용한 조절회귀분석


df.m1 <- lm(경력계획실천행동~MC_코칭, data = df)
df.m2 <- lm(경력계획실천행동~MC_코칭 + MC_자율, data = df)
library(QuantPsyc)  #조절효과를 계산해 주는 함수 moderate.lm()을 포함한 패키지
df.m3 <- moderate.lm(MC_코칭, MC_자율, 경력계획실천행동, df, mc = FALSE)

summary(df.m1)
summary(df.m2)
summary(df.m3)

anova(df.m1)
anova(df.m2)
anova(df.m3)

# 더빈왓슨 테스트트
library(car)
durbinWatsonTest(df.m1)   # Durbin-Watson 값은 1.991으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.
durbinWatsonTest(df.m2)   # Durbin-Watson 값은 1.929으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.
durbinWatsonTest(df.m3)   # Durbin-Watson 값은 1.865으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.


# [모양 요약] : 모형 1, 모형 2, 모형 3에 따라 독립변수가 투입되며, R 제곱값으로 회귀식의 설명력을 확인. 
# Durbin-Watson의 값이 2.106으로 2에 근접 독립적 
# 모형 1, 모형 2, 모형 3은 갈수록 설명력이 향상되는 것을 볼 수 있다. 특히 조절효과를 확인하는 상호작용변수의 
# 투입에도 미세한 값이지만 R2값이 늘어나는 것을 확인할 수 있다. 
# 또한 유의확률 F의 변화량을 확인하면 MC_코칭, MC_자율, MC_코칭MC_자율 상호작용효과는 각각 p=.000, p=.000, p=.002로 
# MC_코칭는 경력계획실천행동에 미치는 영향이 유의한 것으로 나타났다.
#  따라서 MC_코칭이 경력계획실천행동에 미치는 영향에 MC_자율의 조절효과가 있다고 볼수 있다.



library(lm.beta)   # 표준화 계수 구하기
df.b1 <- lm.beta(df.m1)
df.b2 <- lm.beta(df.m2)
df.b3 <- lm.beta(df.m3)
summary(df.b1)
summary(df.b2)
summary(df.b3)

# VIF : 10 이하, 공차 : 0.1 이상인 경우에는 다중공선성에 문제가 없는 것으로 판단
library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.
ols_coll_diag(df.m2)
ols_coll_diag(df.m3)

# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄


#################################################################
#### 수작업으로 하는 조절회귀분석 Moderation "By Hand"
library(gvlma)
fitMod <- lm(경력계획실천행동 ~ MC_코칭 + MC_자율 + MC_코칭*MC_자율, df) #Model interacts IV & moderator
summary(fitMod)
coef(summary(fitMod))
gvlma(fitMod)

# 데이터 요약 (Data Summary)
library(stargazer)
stargazer(fitMod,type="text", title = "MC_코칭이 경력계획실천행동에 끼치는 MC_자율의 조절효과")

#Plotting
library(rockchalk)
ps  <- plotSlopes(fitMod, plotx="MC_코칭", modx="MC_자율", xlab = "MC_코칭", ylab = "경력계획실천행동", modxVals = "std.dev")
#################################################################





# MC_지원_MC_자율의 상호작용효과 회귀분석 검증


df.m4 <- lm(경력계획실천행동~MC_코칭, data = df)
df.m5 <- lm(경력계획실천행동~MC_코칭+MC_지원, data = df)
# library(QuantPsyc)  #조절효과를 계산해 주는 함수 moderate.lm()을 포함한 패키지
df.m6 <- moderate.lm(MC_코칭, MC_지원, 경력계획실천행동, df, mc = FALSE)

summary(df.m4)
summary(df.m5)
summary(df.m6)

anova(df.m4)
anova(df.m5)
anova(df.m6)

# 더빈왓슨 테스트트
# library(car)
durbinWatsonTest(df.m4)   # Durbin-Watson 값은 1.991으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.
durbinWatsonTest(df.m5)   # Durbin-Watson 값은 1.984으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.
durbinWatsonTest(df.m6)   # Durbin-Watson 값은 1.977으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.


# [모양 요약] : 모형 1, 모형 2, 모형 3에 따라 독립변수가 투입되며, R 제곱값으로 회귀식의 설명력을 확인. 
# Durbin-Watson의 값이 2.116로 2에 근접 독립적
#  모형 3은 설명력이 향상되지 않았다. 즉, 조절효과를 확인하는 상호작용변수의 투입에도 R2값이 늘어나지 않은 것을 
# 확인할 수 있다. 
# 또한 유의확률 F의 변화량을 확인하면 MC_코칭과 MC_지원은 각각 p=.000, p=.393으로 지원은 경력계획실천행동에 미치는 영향이 
# 유의하지 않으며, 코칭과 지원의 상호작용의 경우는 p=.006으로 유의한 것으로 나타났다.
#  따라서 MC_지원이 경력계획실천행동에 미치는 영향에 MC_지원의 조절효과가 있다고 볼 수 없다.


# library(lm.beta)   # 표준화 계수 구하기
df.b4 <- lm.beta(df.m4)
df.b5 <- lm.beta(df.m5)
df.b6 <- lm.beta(df.m6)
summary(df.b4)
summary(df.b5)
summary(df.b6)

# VIF : 10 이하, 공차 : 0.1 이상인 경우에는 다중공선성에 문제가 없는 것으로 판단
library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.
ols_coll_diag(df.m5)
ols_coll_diag(df.m6)

# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄



# MC_코칭_MC_자율의 상호작용효과 회귀분석 검증


df.m7 <- lm(경력계획실천행동~MC_코칭, data = df)
df.m8 <- lm(경력계획실천행동~MC_코칭+MC_보상, data = df)
# library(QuantPsyc)  #조절효과를 계산해 주는 함수 moderate.lm()을 포함한 패키지
df.m9 <- moderate.lm(MC_코칭, MC_보상, 경력계획실천행동, df, mc = FALSE)

summary(df.m7)
summary(df.m8)
summary(df.m9)

anova(df.m7)
anova(df.m8)
anova(df.m9)

# 더빈왓슨 테스트트
# library(car)
durbinWatsonTest(df.m7)   # Durbin-Watson 값은 1.991으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.
durbinWatsonTest(df.m8)   # Durbin-Watson 값은 1.955으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.
durbinWatsonTest(df.m9)   # Durbin-Watson 값은 1.984으로 2에 가까워 독립적 자기상관을 가지고 있다고 볼 수 있음.

# [모양 요약] : 모형 1, 모형 2, 모형 3에 따라 독립변수가 투입되며, R 제곱값으로 회귀식의 설명력을 확인. 
# Durbin-Watson의 값이 1.786로 2에 근접 독립적 
# 모형 1, 모형 2, 모형 3은 갈수록 설명력이 향상되는 것을 볼 수 있다. 특히 조절효과를 확인하는 상호작용변수의 
# 투입에도 미세한 값이지만 R2값이 늘어나는 것을 확인할 수 있다. 
# 또한 유의확률 F의 변화량을 확인하면 MC_코칭, MC_보상, MC_코칭MC_보상의 상호작용효과는 각각 p=.000, p=.038, p=.015로 
# 경력계획실천행동에 미치는 영향이 유의한 것으로 나타났다.
#  따라서 MC_코칭이 경력계획실천행동에 미치는 영향에 MC_보상이 정(+)의 조절효과가 있다고 볼수 있다



# library(lm.beta)   # 표준화 계수 구하기
df.b7 <- lm.beta(df.m7)
df.b8 <- lm.beta(df.m8)
df.b9 <- lm.beta(df.m9)
summary(df.b7)
summary(df.b8)
summary(df.b9)

# VIF : 10 이하, 공차 : 0.1 이상인 경우에는 다중공선성에 문제가 없는 것으로 판단
library(olsrr) # vif() 함수가 단순히 다중공선성에 대한 고유값만 나타내 주는데 비해서 olsrr 패키지 내의 
# ols_coll_diag() 함수는 SPSS 에서 도출하는 모든 다중공선성 검사에서 나오는 지수들을 모두 표현해 줌.
ols_coll_diag(df.m8)
ols_coll_diag(df.m9)

# Tolerance 는 공차
# Condition Index 는 Condition Number 와 동일하게 상태지수. 고유값을 변형한 값으로, 15보다 작아야 다중공선성의 문제가 없다 판단
# intercept 는 상수
# 이후에 feature, comfort, usefulness  에서 나오는 모든 수치는 분산비율을 나타냄
# 각 항목 아래의 숫자는 분산비율 = 각 차원에서 독립변수들의 설명력을 나타냄


