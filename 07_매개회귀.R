# 매개 회귀분석 (Mediated Regression Analysis)


# 매개 회귀분석 성립조건

# a. ‘독립변수 → 매개변수’ 간의 유의성 검정(유의미한 영향관계여야 한다)
# b. ‘독립변수 → 종속변수’ 간의 유의성 검정(유의미한 영향관계여야 한다)
# c. ‘독립변수, 매개변수 → 종속변수’ 간의 유의성 검정(유의미한 영향관계여야 한다)
# a,b,c 의 값 비교(‘b > a’가 되어야 매개효과가 인정된다)

# 실습문제
# 스마트폰의 사용자가 만족을 느끼는 요인이 외관, 유용성, 편의성 중 어떤 것인지 알아보기 위해 인과관계를 알아 보았다. 
# 이때 브랜드가 매개역할을 하는지 알아보고자 한다. 본 연구의 설문은 5가지 변수에 대해 총 13문항으로 설문을 실시했다.
# (data > 실습파일 > 9.매개 회귀분석.sav)


rm(list=ls())

library(dplyr)
library(haven)
library(psych)
library(lm.beta)
library(mediation)


setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/실습자료2")

df <- read_spss("9. 매개 회귀분석.sav")
df <- mutate(df, 외관 = (외관1+외관2+외관3)/3, 편의성 = (편의성1+편의성2)/2,
             유용성 = (유용성1+유용성2+유용성3)/3, 브랜드 = (브랜드1+브랜드2+브랜드3)/3,
             만족감 = (만족감1+만족감2)/2)

########################
# mediation package 를 활용한 매개효과 분석
# med.fit <- lm(만족감 ~ 외관 + 편의성 + 유용성, data = df)
# out.fit <- glm(브랜드 ~ 만족감 + 외관 + 편의성 + 유용성, data = df)
# med.out <- mediate(med.fit, out.fit, treat = "외관", mediator = "브랜드",robustSE = TRUE, sims = 100)
# summary(med.out)
########################

# 외관,브랜드→만족감에 대해
# 1단계: 독립변수→매개변수로 하는 회귀분석 실시

## 외관(X)을 독립변수로 브랜드(M)를 종속변수로 하는 회귀분석
df.m1 <- lm(브랜드~외관, data = df)
summary(df.m1)   # [모형요약] R2를 통한 모형의 설명력을 확인
anova(df.m1)     # 유의확률을 통한 회귀식의 유의성을 판단
df.b1 <- lm.beta(df.m1)  #표준화계수에 의한 β값 구하기
summary(df.b1)   #회귀식의 계수와 p값을 확인


# 2단계-1: 독립변수(X)→종속변수(Y)로 하는 회귀분석 실시
# 2단계-2: 독립변수,매개변수→종속변수로 하는 회귀분석 실시 
# 외관, 브랜드→만족감에 대해

## 외관(X)을 독립변수로 만족감(Y)를 종속변수로 하는 회귀분석
df.m2 <- lm(만족감~외관, data = df)
summary(df.m2)   # [모형요약] R2를 통한 모형의 설명력을 확인
## 외관(X), 브랜드(M)을 독립변수로 만족감(Y)를 종속변수로 하는 회귀분석
df.m3 <- lm(만족감~외관+브랜드, data = df)
summary(df.m3)   # [모형요약] R2를 통한 모형의 설명력을 확인

# [모형 요약]: 모형 1(df.m4)은 독립변수 -> 종속변수 의 분석 결과,
#              모형 2(df.m5)는 독립변수, 매개변수 -> 종속변수 의 분석 결과

anova(df.m2)     # 유의확률을 통한 회귀식의 유의성을 판단
anova(df.m3)     # 유의확률을 통한 회귀식의 유의성을 판단

# [분산분석]: 모형 1과 모형 2의 두 가지 분석 결과 모두 유의미한 관계 (회귀식 성립)

df.b2 <- lm.beta(df.m2)  #표준화계수에 의한 β값 구하기
summary(df.b2)   #회귀식의 계수와 p값을 확인
df.b3 <- lm.beta(df.m3)  #표준화계수에 의한 β값 구하기
summary(df.b3)   #회귀식의 계수와 p값을 확인

# [계수]: 모형 2의 외관과 브랜드의 유의확률이 .05보다 커서 유의하지 않은것으로 나타났다.
# 따라서 브랜드는 외관과 만족감 사이에 매개역할을 하지 않은것으로 판단된다

# Sobel test 사후검정
# Sobel Test 는 간접효과가 통계적으로 유의한지를 검증하는 도구임
library(bda)  # Sobel Test 를 위한 패키지
mediation.test(df$브랜드,df$외관,df$만족감)
# Zab = 0.6439 (p = 0.520) 으로 1.96보다 작음. 따라서 간접효과는 유의하지 않다는 것을 의미한다


# mediation package를 활용하여 간략하게 매개효과를 볼 때
df.med1 <- mediate(df.m1, df.m3,  treat = "외관", mediator = "브랜드",robustSE = TRUE, sims = 100)
summary(df.med1)

# ACME(average causal mediation effects)는 독립변수가 종속변수에 미치는 간접효과로서 이 경우에는 0.0363을 나타낸다.
# ADE(average direct effects)는 독립변수가 종속변수에 미치는 직접효과로서 이 경우에는 0.0751을 나타낸다.
# Total Effct 는 총효과로서 위의 ACME 와 ADE를 합한 값이다.
# Prop. Mediated 는 매개효과비중으로서 ACME를 Total Effect로 나눈 값이다.


# 편의성,브랜드→만족감에 대해
# 1단계: 독립변수→매개변수로 하는 회귀분석 실시

## 편의성(X)을 독립변수로 브랜드(M)를 종속변수로 하는 회귀분석
df.m4 <- lm(브랜드~편의성, data = df)
summary(df.m4)   # [모형요약] R2를 통한 모형의 설명력을 확인
anova(df.m4)     # 유의확률을 통한 회귀식의 유의성을 판단
df.b4 <- lm.beta(df.m4)  #표준화계수에 의한 β값 구하기
summary(df.b4)   #회귀식의 계수와 p값을 확인


# 2단계-1: 독립변수(X)→종속변수(Y)로 하는 회귀분석 실시
# 2단계-2: 독립변수,매개변수→종속변수로 하는 회귀분석 실시 
# 편의성, 브랜드→만족감에 대해

## 편의성(X)을 독립변수로 만족감(Y)를 종속변수로 하는 회귀분석
df.m5 <- lm(만족감~편의성, data = df)
summary(df.m5)   # [모형요약] R2를 통한 모형의 설명력을 확인
## 편의성(X), 브랜드(M)을 독립변수로 만족감(Y)를 종속변수로 하는 회귀분석
df.m6 <- lm(만족감~편의성+브랜드, data = df)
summary(df.m6)   # [모형요약] R2를 통한 모형의 설명력을 확인

anova(df.m5)     # 유의확률을 통한 회귀식의 유의성을 판단
anova(df.m6)     # 유의확률을 통한 회귀식의 유의성을 판단

df.b5 <- lm.beta(df.m5)  #표준화계수에 의한 β값 구하기
summary(df.b5)   #회귀식의 계수와 p값을 확인
df.b6 <- lm.beta(df.m6)  #표준화계수에 의한 β값 구하기
summary(df.b6)   #회귀식의 계수와 p값을 확인

# Sobel test 사후검정
# library(bda)  # Sobel Test 를 위한 패키지
mediation.test(df$브랜드,df$편의성,df$만족감)
# Zab = 1.549 (p = 0.121) 으로 1.96보다 작음. 따라서 간접효과는 유의하지 않다는 것을 의미한다

# mediation package를 활용하여 간략하게 매개효과를 볼 때
df.med2 <- mediate(df.m4, df.m6,  treat = "편의성", mediator = "브랜드",robustSE = TRUE, sims = 100)
summary(df.med2)

# ACME(average causal mediation effects)는 독립변수가 종속변수에 미치는 간접효과로서 이 경우에는 0.019을 나타낸다. (p>.05)
# ADE(average direct effects)는 독립변수가 종속변수에 미치는 직접효과로서 이 경우에는 0.198을 나타낸다. (p<.000)
# Total Effct 는 총효과로서 위의 ACME 와 ADE를 합한 값이다. (p<.000)
# Prop. Mediated 는 매개효과비중으로서 ACME를 Total Effect로 나눈 값이다. (.000)
# 이 경우 매개변수의 간접효과가 없다는 것을 알 수 있다.


# 유용성,브랜드→만족감에 대해
# 1단계: 독립변수→매개변수로 하는 회귀분석 실시


## 유용성(X)을 독립변수로 브랜드(M)를 종속변수로 하는 회귀분석
df.m7 <- lm(브랜드~유용성, data = df)
summary(df.m7)   # [모형요약] R2를 통한 모형의 설명력을 확인
anova(df.m7)     # 유의확률을 통한 회귀식의 유의성을 판단
df.b7 <- lm.beta(df.m7)  #표준화계수에 의한 β값 구하기
summary(df.b7)   #회귀식의 계수와 p값을 확인


# 2단계-1: 독립변수(X)→종속변수(Y)로 하는 회귀분석 실시
# 2단계-2: 독립변수,매개변수→종속변수로 하는 회귀분석 실시 
# 유용성, 브랜드→만족감에 대해


## 편의성(X)을 독립변수로 만족감(Y)를 종속변수로 하는 회귀분석
df.m8 <- lm(만족감~유용성, data = df)
summary(df.m8)   # [모형요약] R2를 통한 모형의 설명력을 확인
## 유용성(X), 브랜드(M)을 독립변수로 만족감(Y)를 종속변수로 하는 회귀분석
df.m9 <- lm(만족감~유용성+브랜드, data = df)
summary(df.m9)   # [모형요약] R2를 통한 모형의 설명력을 확인

# [모형 요약]: 모형 1(df.m4)은 독립변수 -> 종속변수 의 분석 결과,
#              모형 2(df.m5)는 독립변수, 매개변수 -> 종속변수 의 분석 결과


anova(df.m8)     # 유의확률을 통한 회귀식의 유의성을 판단
anova(df.m9)     # 유의확률을 통한 회귀식의 유의성을 판단

# [분산분석]: 모형 1과 모형 2의 두 가지 분석 결과 모두 유의미한 관계 (회귀식 성립)

df.b8 <- lm.beta(df.m8)  #표준화계수에 의한 β값 구하기
summary(df.b8)   #회귀식의 계수와 p값을 확인
df.b9 <- lm.beta(df.m9)  #표준화계수에 의한 β값 구하기
summary(df.b9)   #회귀식의 계수와 p값을 확인

# [계수]: 모형 2의 외관과 브랜드의 유의확률이 .05보다 작아서 유의한 것으로 나타났다.
# 또한 두 값을 비교해 보면‘(❷ =.239) > (❸ =.210)’(모형1의 유용성 베타값이 모형2보다 커야함)
# 따라서 브랜드는 유용성과 만족감 사이에 정(+)의 매개역할을 하는 것으로 판단된다.


# Sobel test 사후검정
# library(bda)  # Sobel Test 를 위한 패키지
mediation.test(df$브랜드,df$유용성,df$만족감)
# Zab = 2.0419 (p = 0.041) 으로 1.96보다 큼. 따라서 간접효과는 유의하다는 것을 의미한다

# mediation package를 활용하여 간략하게 매개효과를 볼 때
df.med3 <- mediate(df.m7, df.m9,  treat = "유용성", mediator = "브랜드",robustSE = TRUE, sims = 100)
summary(df.med3)

# ACME(average causal mediation effects)는 독립변수가 종속변수에 미치는 간접효과로서 이 경우에는 0.029을 나타낸다. (p<.000)
# ADE(average direct effects)는 독립변수가 종속변수에 미치는 직접효과로서 이 경우에는 0.199을 나타낸다. (p<.000)
# Total Effct 는 총효과로서 위의 ACME 와 ADE를 합한 값이다. (p<.000)
# Prop. Mediated 는 매개효과비중으로서 ACME를 Total Effect로 나눈 값이다. (.000)
# 이 경우 매개변수의 간접효과가 있다는 것을 알 수 있다.
