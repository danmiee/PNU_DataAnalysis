getwd()
setwd("C:/Users/user/git/Rwork/training/06")

df1 <- read.csv("./06_국민건강보험공단500.csv", 
                header = T, 
                stringsAsFactors = F, 
                fileEncoding = 'euc-kr')
head(df1)

# 행 수, 열 수 확인
nrow(df1)
ncol(df1)

# 열 추출
names(df1)
df11 <- df1[,c("시도코드","성별코드","수축기.혈압","이완기.혈압",
               "식전혈당.공복혈당.","트리글리세라이드","HDL.콜레스테롤","허리둘레")]
head(df11)

# 열이름 변경
names(df11) <- c('시도코드','성별코드','수축기혈압','이완기혈압',
                '공복혈당','트리글리세라이드','HDL콜레스테롤','허리둘레')
names(df11)

# 결측치 확인
summary(df11)

# 결측치 제거
df11 <- na.omit(df11)
summary(df11)
1:nrow(df11)

# 대사증후군 구분

# 고혈압 여부
df11$고혈압 <- ifelse((df11$수축기혈압 >=130)&(df11$이완기혈압 >= 85),1,0)

# 고혈당 여부
df11$고혈당 <- ifelse(df11$공복혈당 >= 100,1,0)

# 고중성지방 여부
df11$고중성지방 <- ifelse(df11$트리글리세라이드 >= 150,1,0)

# 저HDL콜레스테롤 여부
df11$저HDL콜레스테롤 <- ifelse(df11$성별코드==1,
                         ifelse(df11$HDL콜레스테롤 < 40, 1, 0),
                         ifelse(df11$HDL콜레스테롤 < 50, 1, 0))

# 복부비만 여부
df11$복부비만 <- ifelse(df11$성별코드==1,
                    ifelse(df11$HDL콜레스테롤 >= 90, 1, 0),
                    ifelse(df11$HDL콜레스테롤 >= 85, 1, 0))
head(df11)

# 대사증후군 구분(0:정상, 1:주의군, 2:위험군)
# df11$대사증후군 <- df11$고혈압 + df11$고혈당 + df11$고중성지방 + df11$저HDL콜레스테롤 + df11$복부비만
df11$대사증후군 <- rowSums(df11[, 9:13])
df11$판별 <- ifelse(df11$대사증후군 == 0, "정상",
                       ifelse(df11$대사증후군 <= 2, "주의군","위험군"))
head(df11)

# factor화하기
is.factor(df11$판별)
df11$판별 <- as.factor(df11$판별)
df11$성별코드 <- as.factor(df11$성별코드)

# 지도학습: 학습데이터(train)와 테스트데이터(test)로 분리
x <- sample(1:nrow(df11), 0.7*nrow(df11))
x

train <- df11[x, ]
test <- df11[-x, ]

nrow(train)
nrow(test)
nrow(df11)

# 지도학습: 모델 결정(분류모델) 후 학습
library(party)
# ctree(target ~ features, data=df)
model <- ctree(판별 ~ 성별코드 + 수축기혈압 + 이완기혈압 + 공복혈당 +
                 트리글리세라이드 + HDL콜레스테롤 + 허리둘레, data = train)

# 지도학습: 예측
# test의 features로 target 예측
pred <- predict(model, test)

# 지도학습: 성능지표 확인
t <- table(test$판별, pred) 
t 

# accuracy
acc <- (t[1,1]+t[2,2]+t[3,3]) / sum(t)
acc

