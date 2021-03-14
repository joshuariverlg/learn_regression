# 다변량 회귀분석 - Stepwise 분석실습


library(tidyverse)
library(haven)
library(caret)
library(leaps)
library(MASS)
library(lmtest)
library(car)
library(lm.beta)


setwd("~/R/Regression/SPSS_다변량회귀분석/01.상반부 자료/실습자료2")

df.a <- read_spss("5. 단계적 회귀분석.sav")
df.a <- mutate(df.a, feature = (외관1+외관2+외관3)/3, comfort = (편의성1+편의성2+편의성3+편의성4)/4,
               usefulness = (유용성1+유용성2+유용성3+유용성4+유용성5)/5, satisfaction = (만족감1+만족감2+만족감3)/3,
               brand = (브랜드1+브랜드2+브랜드3+브랜드4+브랜드5)/5)
df.b <- df.a[,c(21:25)]


# Fit the full model 
full.model <- lm(satisfaction ~ feature + comfort + usefulness + brand, data = df.b)

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# 모형요약: 만족감 (satisfaction) 이 종속변수로 사용됨.
# 이 모형의 경우
# R-Square 값이 0.2369  조정된 R-Square 값이 0.2298로서 종속변수에 대한 독립변수의 설명력이 22.98%인 것을 알 수 있다.

dwtest(satisfaction ~., data = df.b)

# 더빈-왓슨 테스트는 회귀분석 후 잔차의 독립성을 확인할 때 쓰이는 테스트로써, 잔차끼리 자기상관성이 있는지 없는지를 판단한다
# DW값이 0에 가깝다면 양의 자기상관이 있다고 판단할 수 있고
# DW값이 2에 가깝다면 자기상관성이 거의 없다고 판단할 수 있고
# DW값이 4에 가깝다면 음의 자기상관성이 있다고 판단할 수 있음.

anova(step.model)
aov(step.model)

# ANOVA(분산분석) 회귀식 각각의 F 값과 유의확률을 보여준다. F값이 충분히 크고 p < 0.05 이므로 회귀분석에 적합한 모형임을 
# 알 수 있다.


vif(step.model)

# vif()함수는 car 패키지에 포함된 함수이며, 변수의 다중공선성을 판단하는 수치이다. 
# vif 값이 10 미만이면 다중공선성에 문제가 없는 것으로 판단한다.


models <- regsubsets(satisfaction~., data = df.b, nvmax = 5,
                     method = "seqrep")

summary(step.model)

# regsubsets() 함수를 이용해 여러가지 변수의 투입방법을 적용하여 회귀분석을 해 볼 수 있다. (method = 에서 옵션선택)
# exhaustive (입력) : 모든 변수를 한꺼번에 투입하여 유의미한 결과와 아닌 결과를 산출한다.
# backward (후진) : 독립변수 모두를 포함시킨 모형에서 분석을 시작한다.  가장 영향력이 적은 변수부터 제거하면서 
#                   더 이상 제거할 변수가 없을 때 제거를 중단하고 모형을 최종적으로 선택하여 분석한다.
# forward (전진): 종속변수에 가장 큰 영향을 주는 독립변수부터 모형에 포함시키며, 더 이상 추가될 독립변수가 없을 때 
#                 변수의 선택을 중단하고 분석을 시작한다.
# seqrep (순차적 단계선택): 전진과 후진을 결합한 방식이다. 독립변수의 기여도를 평가한 후 전지의 방법으로 가장 높은 기여도의 
#                   변수를 먼저 투입하고, 변수를 단계별로 검토하여 제거한다.  더 이상 추가되거나 제거할 변수가 없을 때의 
#                   모형을 기준으로 최적의 회귀식을 산출하여 분석을 시작한다.
# nvmax 옵션은 테스트 할 서브셋의 최대 수를 선택하는 것이다.  독립,종속변수의 숫자를 합한 수를 입력하면 된다.

model.beta <- lm.beta(full.model)
summary(model.beta)

# 회귀식에 대한 계수 B값을 Estimate로 나타내며, 표준화된 β값을 Standardized로, 표준 오타를 Std. Error 로,
# t값을 t value로, P값을 value Pr(>|t\)로 나타낸다.

vif(model.beta)


# 단계적 회귀분석 (후진방식)


step(lm(satisfaction~comfort,data=df.b),direction="backward")  # 후진방식
step(lm(satisfaction~comfort+feature,data=df.b),direction="backward")  # 후진방식
step(lm(satisfaction~comfort+feature+usefulness,data=df.b),direction="backward")  # 후진방식
step(lm(satisfaction~comfort+feature+usefulness+brand,data=df.b),direction="backward")  # 후진방식


step.forward <- step(lm(satisfaction~.,data=df.b),direction="forward",scope=~feature+comfort+usefulness+brand)  #전진방식
step.both <- step(lm(satisfaction~.,data=df.b),direction="both") # 입력력

# MASS 패키지를 이용하는 방법 (stepAIC()함수 사용)
res.lm <- lm(satisfaction ~., data = df.b)
step <- stepAIC(res.lm, direction = "both", trace = FALSE)
step

# stepAIC() 함수는 회귀분석에 있어서 변수의 투입방법중 전진과 후진 중 더욱 적절한 방식을 찾아주는 함수임.


anova(step.backward)
anova(step.forward)
anova(step.both)



# Set seed for reproducibility
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model

step.model <- train(satisfaction ~., data = df.b,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results


step.model$bestTune


summary(step.model$finalModel)

coef(step.model$finalModel, 3)

lm(satisfaction ~ feature + comfort + usefulness + brand, 
   data = df.b)




# Train the model
step.model <- train(satisfaction ~., data = df.b,
                    method = "lmStepAIC", 
                    trControl = train.control,
                    trace = FALSE
)
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)



