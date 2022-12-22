getwd()
setwd("C:/Users/user/git/Rwork/training/08")

#데이터 불러오기
df <- read.csv("./07_출생아수.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr') 
df
names(df) <- c('시점','출생아수')

library(dplyr)
train <- df %>% filter(시점 > 2000 & 시점 <= 2010)
test <- df %>% filter(시점 > 2010)

#1. 데이터준비
temp <- train$출생아수
temp

#2. 시계열 데이터 생성 
temp_ts <- ts(temp, frequency = 1, start = c(2000,1))
temp_ts

library(forecast)
#auto.arima() : 시계열 모형을 식별하는 알고리즘에 의해
#최적의 모형과 파라미터를 추정하여 제공
arima <- auto.arima(temp_ts)
arima

#5.모형생성
model <- arima(temp_ts, order=c(0,1,0))
model

#6.모형타당성 검사 
#자기 상관함수에 의한 모형 진단
tsdiag(model)

#box-Ljungn잔차항 모형 진단
#p-value >= 0.05 통계적으로 적절 
Box.test(model$residuals, lag=1, type="Ljung")

#7.예측
fore <- forecast(model, h=11)
fore

plot(fore) 

test$출생아수
class(fore$mean)
pred = as.vector(fore$mean)
pred
result <- data.frame(test = test$출생아수, pred=pred)
result

plot(result$test, type="o", col="red")
par(new=T)
plot(result$pred, type="o", col="blue")

