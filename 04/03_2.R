#암종류별 성별 분석
install.packages("dplyr")
library(dplyr)

#데이터 불러오기(암발생자수)
getwd()
setwd("C:/Rwork/training")
df1 <- read.csv("./03/03_암발생자수.csv",
                  header=T,
                  stringsAsFactors = F,
                  fileEncoding = "euc-kr")
head(df1) 

# 열명 변경
# "암종별", "성별", "연령별", "발생자수", "조발생률"
names(df1) <- c("암종별", "성별", "연령별", "발생자수", "조발생률") 

# 데이터셋 조회
unique(df1$암종별)

# 1) 특정 변수 조회
t1 <- df1$암종별
mode(t1)      # character
class(t1)     # character
is.vector(t1) # TRUE

# 2) 특정 열명을 사용하여 조회
t2 <- df1['암종별'] 
mode(t2)      # list
class(t2)     # data.frame : vector 형태로 변경 필요
is.vector(t2) # FALSE

# 3) 특정 행 조회 :1행 조회
df1[1,]
df1[c(2,4),]

# 4) 특정행 제거 : 1행제거
df <- df1[-1,]
head(df)


# 5) 특정행 열 조회
df[1,1]
df[1,'암종별']
df[1:3,'암종별']
df[1:3,c('암종별','발생자수')]

# 열 데이터타입 확인
str(df)

# 값 변경 : - => 0
df$발생자수 <- ifelse(df$발생자수=='-','0',df$발생자수)
df$발생자수
df$조발생률 <- ifelse(df$조발생률=='-','0',df$조발생률)
df$조발생률

# 열 데이터타입 변경
df$발생자수 <- as.numeric(df$발생자수)
df$조발생률 <- as.numeric(df$조발생률)
str(df)

# 모든암 제거하고 연령별이 계인 데이터 
unique(df2$암종별)
# %>%(파이프연산자) : sql의 select와 유사
# filter : sql의 where와 유사
df2 <- df %>% 
          filter(암종별!="모든 암(C00-C96)") %>%
          filter(연령별=="계")
df2

df21 <- df2 %>%
  filter(성별=="계")
df21

df22 <- df2 %>% 
  filter(성별!="계")

df22


# 특정 열 가져오기
df21 <- df21[, c('암종별', '발생자수')]
df21
df22 <- df22[, c('암종별', '성별', '발생자수')]
df22

unique(df2$암종별)

# 그래프 
install.packages("ggthemes")
library(ggthemes)
library(ggplot2)

ggplot(mapping =aes(x=암종별, y=발생자수, fill=성별), data=df22) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("암종별 성별 발생자수 분석")+
  theme_wsj() +
  # x축, y축 change
  coord_flip()

plot(df21$발생자수, type="o", col="black", xlab='', ylab='')
plot(df22$발생자수, type="o", col="red", xlab='', ylab='')
