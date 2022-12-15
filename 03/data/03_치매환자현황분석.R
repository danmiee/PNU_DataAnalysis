# 치매환자현황분석
getwd()
setwd("C:/Rwork/training")

df_alzh <- read.csv("./03/03_치매환자현황.csv",
                    header=T,
                    stringsAsFactors = F,
                    fileEncoding = "euc-kr")
head(df_alzh)

## 거주지역별 치매환자 빈도표를 구하고 그래프 작성
tb_alzh_loca <- table(df_alzh$거주지역)
tb_alzh_loca
tb_alzh_sex <- table(df_alzh$성별)
tb_alzh_sex
tb_alzh_ls <- table(df_alzh$거주지역, df_alzh$성별)
tb_alzh_ls
df_alzh_ls <- data.frame(tb_alzh_ls)
head(df_alzh_ls)
names(df_alzh_ls) <- c("지역","성별","빈도수")
names(df_alzh_ls)

qplot(지역, 빈도수, data=df_alzh_ls, fill=성별) + 
  ggtitle("거주지역별 치매환자") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

### qplot()
library(ggplot2)
qplot(거주지역, data=df_alzh, fill=거주지역) + 
  ggtitle("거주지역별 치매환자") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

## 기준일자와 진단일자를 이용하여 진단일수를 계산하고 평균 진단일수 산출
names(df_alzh)
class(df_alzh$진단일자)
class(df_alzh$데이터기준일자)

### 날짜형으로 변환 : as.Date()
df_alzh$진단일자 <- as.Date(df_alzh$진단일자)
df_alzh$데이터기준일자 <- as.Date(df_alzh$데이터기준일자)

head(df_alzh)
### 날짜처리 패키지
install.packages("lubridate")
library(lubridate)

year(df_alzh$진단일자)

### 날짜사이 간격 : difftime(시작일, 종료일, units = ＇days’)
#### units = 'mins’ 분단위, units = 'secs’ 초단위
df_alzh$진단일수 <- difftime(df_alzh$데이터기준일자, df_alzh$진단일자, units = 'days')
head(df_alzh)

## 연령대별 빈도수 그래프를 그려서 치매환자가 많은 연령대 분석
### ◦ 열 내용에 문자열 붙이기 : paste(열, “대“, sep=“”) 
### ◦ 100세 이상은 90대에 포함
current_year <- as.numeric(format(df_alzh$데이터기준일자,"%Y"))
class(current_year)
df_alzh$연령 <- year(df_alzh$데이터기준일자)-df_alzh$출생년도
# df_alzh$연령 <- current_year-df_alzh$출생년도
head(df_alzh)

df_alzh$연령대 <- paste(df_alzh$연령%/%10*10,"대",sep="")
subset(df_alzh, df_alzh$연령대=="100대")
df_alzh$연령대 <- ifelse(df_alzh$연령대=="100대","90대",df_alzh$연령대)
# min(df_alzh$연령)
# df_alzh$연령대 <- ifelse(df_alzh$연령>=90,"90",
#                   ifelse(df_alzh$연령>=80,"80",
#                          ifelse(df_alzh$연령>=70,"70",
#                                 ifelse(df_alzh$연령>=60,"60",
#                                        ifelse(df_alzh$연령>=50,"50",
#                                               ifelse(df_alzh$연령>=40,"40"))))))
# df_alzh$연령대 <- paste(df_alzh$연령대,"대",sep="")

tb_alzh_age <- table(df_alzh$연령대)
tb_alzh_age

qplot(연령대, data=df_alzh, fill=연령대) + 
  ggtitle("연령대별 빈도수") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

qplot(성별, data=df_alzh, fill=성별) + 
  ggtitle("성별별 빈도수") + 
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
k