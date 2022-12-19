#타이타닉 데이터
#https://www.kaggle.com/c/titanic/overview

getwd()
setwd("C:/Users/user/git/Rwork/training/05")

#데이터불러오기
df <- read.csv("./05_titanic.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr') 
head(df)

#데이터구조
# PassengerID	승객을 구별하는 고유 ID number	Int
# Survived	승객의 생존 여부를 나타내며 생존은 1, 사망은 0 입니다.	Factor
df$Survived <- as.factor(df$Survived)
# Pclass	선실의 등급으로서 1등급(1)부터 3등급(3)까지 3개 범주입니다.	Ord.Factor
df$Pclass <- as.factor(df$Pclass)
# Name	승객의 이름	Factor
# Sex	승객의 성별	Factor
# Age	승객의 나이	Numeric
# SibSp	각 승객과 동반하는 형제 또는 배우자의 수를 설명하는 변수이며 0부터 8까지 존재합니다.	Integer
# Parch	각 승객과 동반하는 부모님 또는 자녀의 수를 설명하는 변수이며 0부터 9까지 존재합니다.	Integer
# Ticket	승객이 탑승한 티켓에 대한 문자열 변수	Factor
# Fare	승객이 지금까지 여행하면서 지불한 금액에 대한 변수	Numeric
# Cabin	각 승객의 선실을 구분하는 변수이며 범주와 결측치가 너무 많습니다.	Factor
# Embarked	승선항, 출항지를 나타내며 C, Q, S 3개 범주이다.	Factor

### 생존율 예측하기
# feature : Pclass, Sex, Age
# target : Survived
names(df)
head(df)

# 결측치 확인
summary(df)
str(df)

# 결측치 처리 (평균값으로 대체)
df$Age <- ifelse(df$Age, mean(df$Age, na.rm=T), df$Age)
# df$Age[is.na(df$Age)] <- mean(df$Age, na.rm=TRUE)
summary(df)

# 성별에 따른 생존여부
library(ggplot2)
library(ggthemes)

ggplot(df, aes(x = Survived, fill = Sex)) +
  geom_bar()

ggplot(df, aes(x = Survived, fill = Sex)) +
  geom_bar(position = "dodge")


class(df$Survived)
mode(df$Survived)

#Pclass	선실의 등급에 따른 생존여부
ggplot(df, aes(x = Survived, fill = Pclass)) +
  geom_bar()

ggplot(df, aes(x = Survived, fill = Pclass)) +
  geom_bar(position="dodge")


#분류모델
names(df)
ta <- df[, c('Pclass', 'Sex', 'Age', 'Survived')]
head(ta)
 
#수치데이터로 변환
ta$Sex <- ifelse(ta$Sex=='male',1,2)

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(ta), 0.7*nrow(ta))
x

train <- ta[x, ]
test <- ta[-x, ]

#트리 모델 
library(party)
names(ta)
model <- ctree(Survived ~ Pclass + Sex + Age, data = train)
model

#예측
pred <- predict(model, test)
pred

#혼돈행렬
t <- table(test$Survived, pred) 
t 
 
#accuracy
acc <- (t[1,1]+t[2,2]) / sum(t)
acc
