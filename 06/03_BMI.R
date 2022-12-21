getwd()
setwd("C:/Users/user/git/Rwork/training/06")

df1 <- read.csv("./06_국민건강보험공단500.csv", 
                header = T, 
                stringsAsFactors = F, 
                fileEncoding = 'euc-kr')
names(df1)

df11 <- df1[,c("시도코드","성별코드",
               "신장.5Cm단위.","체중.5Kg.단위.")]

names(df11) <- c('시도코드','성별코드','신장','체중')
names(df11)

summary(df11)

df11$BMI <- round(df11$체중 / ((df11$신장/100)^2), 2)
head(df11)

df11$비만도 <- ifelse(df11$BMI < 20, "저체중",
                   ifelse(df11$BMI < 24, "정상",
                          ifelse(df11$BMI < 29, "과체중", "비만")))

df11$비만도 <- as.factor(df11$비만도)

#학습데이터와 테스트데이터 분리
x <- sample(1:nrow(df11), 0.7*nrow(df11))
x

# BMI 예측
# 색으로 보는 상관계수
library(corrgram)
corrgram(df11, upper.panel = panel.conf)
# 상관계수 차트
library(PerformanceAnalytics)
chart.Correlation(df11)

train <- df11[x, ]
test <- df11[-x, ]

nrow(train)
nrow(test)

#회귀모델
names(df11)

#학습 (= 모델만들기 = 규칙 찾기)
library(randomForest)
model <- lm(formula = BMI ~ 신장+체중, data=train)
model

#예측 
pred <- predict(model, test)
pred

#평가 (오차 : 적을수록 좋음)
#RMSE : sqrt((실제 - 예측)^2의 평균)
RMSE <- sqrt(mean((test$BMI - pred)^2))
RMSE



# 비만도 예측

#트리 모델 
library(party)
names(df11)

model <- ctree(비만도 ~ 신장 + 체중, data = train)
model

#예측
pred <- predict(model, test)
pred

#혼돈행렬
t <- table(test$비만도, pred) 
t

#accuracy
acc <- (t[1,1]+t[2,2]+t[3,3]+t[4,4]) / sum(t)
acc
