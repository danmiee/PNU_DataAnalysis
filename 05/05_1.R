
#R 내장 데이터 가져오기
data(iris)
head(iris)

# 분류, 회귀 구분
  # 꽃잎으로 꽃받침 길이 예측 > 회귀
  # 꽃잎, 꽃받침으로 품종 구분 > 분류

# 학습시키기
  # 타겟설정

#iris 데이터 확인
str(iris)




# 회귀모델

#iris : 꽃받침(sepal), 꽃잎(petal) 데이터 추출
iris1 <- iris[, -5]
# iris1 <- iris[1:4]
# library(dplyr)
# iris1 <- iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
names(iris1)

#기술통계량
summary(iris1)

#상관계수(+:정비례 / -:반비례)
# 일반적으로 절대값 0.7을 기준으로 상관관계의 고저 판단
# method에 따라 다른 상관관계값 도출출
cor(iris1, method="pearson")

#색의 농도로 상관계수 
install.packages("corrgram")
library(corrgram)

corrgram(iris1, upper.panel = panel.conf)

#상관계수 챠트
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(iris1)

#학습데이터와 테스트데이터 분리
# 분리시키는 이유? 불안해서! 맞는지 확인하려고
# 일반적으로 7:3 분리
x <- sample(1:nrow(iris1), 0.7*nrow(iris1)) # iris1의 전체 행 중 70%의 데이터 샘플로 추출
x  # 행번호

train <- iris1[x, ]
test <- iris1[-x, ]

nrow(train)
nrow(test)

#회귀모델 : 꽃받침 길이(Sepal.Length) 예측 
names(iris1)

#학습 (= 모델만들기 = 규칙 찾기)
model1 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train)
model2 <- lm(formula = Sepal.Length ~ Petal.Length + Petal.Width, data=train)
model3 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data=train)
model4 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data=train)

#예측 
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)
pred4 <- predict(model4, test)

#평가 (오차 : 적을수록 좋음)
#RMSE : sqrt((실제 - 예측)^2의 평균)
RMSE1 <- sqrt(mean((test$Sepal.Length - pred1)^2))
RMSE2 <- sqrt(mean((test$Sepal.Length - pred2)^2))
RMSE3 <- sqrt(mean((test$Sepal.Length - pred3)^2))
RMSE4 <- sqrt(mean((test$Sepal.Length - pred4)^2))

cat("예측1: ",RMSE1)
cat("예측2: ",RMSE2)
cat("예측3: ",RMSE3)
cat("예측4: ",RMSE4)




#분류모델 
library(ggplot2)

# 꽃받침으로 품종 구분
ggplot(iris, aes(Sepal.Length, Sepal.Width))  + 
  geom_point(aes(colour = Species))

# 꽃잎으로 품종 구분
ggplot(iris, aes(Petal.Length , Petal.Width))  + 
  geom_point(aes(colour = Species))

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(iris), 0.7*nrow(iris)) # 품종 포함하는 iris에서 데이터 가져오기
x

train <- iris[x, ]
test <- iris[-x, ]

names(iris)

#트리 모델 
install.packages("party")
library(party)
names(iris)
model1 <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train)
model2 <- ctree(Species ~ Sepal.Length + Sepal.Width, data = train)
model3 <- ctree(Species ~ Petal.Length + Petal.Width, data = train)

plot(model1)
plot(model2)
plot(model3)

#예측
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)

#혼돈행렬
t1 <- table(test$Species, pred1) 
t2 <- table(test$Species, pred2)
t3 <- table(test$Species, pred3)

# 정확도
acc1 <- (t1[1,1]+t1[2,2]+t1[3,3]) / sum(t1)
acc2 <- (t2[1,1]+t2[2,2]+t2[3,3]) / sum(t2)
acc3 <- (t3[1,1]+t3[2,2]+t3[3,3]) / sum(t3)

acc1
acc2
acc3

#iris data 저장
 
