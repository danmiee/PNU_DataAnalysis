#인구 동향
library(dplyr)

getwd()
setwd("C:/Users/user/git/Rwork/training/04")
#데이터 불러오기(인구동향)
df <- read.csv("./04_인구동향.csv",
                header=T,
                stringsAsFactors = F,
                fileEncoding = "euc-kr")
head(df)

# 열명 확인 및 변경
names(df) <- c("행정구역","시점","출생아수","사망자수","혼인건수","이혼건수")
names(df)

head(df)


#EDA
# 데이터 구조 확인
  # 범주형 vs 수치형 구분
  # 범주형 : 평균 의미 x
  # 기존 데이터로 새로운 데이터를 만들 수도 있음

str(df)    # variable = feature 변수

# 범주형 변경 : 시점 int -> category
  # 시점 : 수치로 표현되어있지만 평균이 의미가 없음
    # > 범주형 자료로 보는 것이 좋음
df$시점 <- as.factor(df$시점)
mode(df$시점)
class(df$시점)

#결측치 확인
summary(df)
library(dplyr)

#결측치 내용 확인 
is.na(df$출생아수)
df[is.na(df$출생아수),]
# is.na() 자체가 T/F이므로 비교연산자 불필요
# filter(df,is.na(df$출생아수))
df_na <- df %>% filter(is.na(df$출생아수))
unique(df_na$행정구역)
unique(df_na$시점)

#결측치 행 제거
df1 <- na.omit(df)

#결측치 확인
summary(df)
summary(df1) 

#결측치 값 대체 (특정 열 처리)
df2 <- df
df2$출생아수[is.na(df2$출생아수)]=0
df2$사망자수 <- ifelse(!is.na(df2$사망자수),df2$사망자수,0)
df2$혼인건수 <- ifelse(is.na(df2$혼인건수),0,df2$혼인건수)

#결측치 값 대체 (모든 열 일괄 처리)
df2 <- df2 %>% replace(is.na(df2),0)
summary(df2)

# na값 0으로 변경 : 반복문
df3 <- df
col <- names(df3)[3:6]
for(c in col) {
  # df3$c 불가 / 열이름으로 임시 벡터 생성
  temp <- df3[,c]
  temp <- ifelse(is.na(temp),0,temp)
  df3[,c] <- temp
}
summary(df3)

# 자연증가수 열 추가
df$자연증가수 <- df$출생아수 - df$사망자수
head(df)

# 전국적으로 자연증가수가 0보다 작아지는 경우 찾기
  # case1
df[df$행정구역=='전국' & df$자연증가수<0,]['시점']

  # case2
df[which(df$행정구역=='전국'&df$자연증가수<0),]['시점']

  # case3 (권장)
df %>% 
  filter(df$자연증가수<0 & df$행정구역=="전국") %>% 
  select('시점')

df_minus_all <- df %>% filter((df$자연증가수<0)&(df$행정구역=="전국"))
df_minus_all
df_minus_all['시점']

# 기술통계분석 - 범주형자료 - 빈도분석
  # 빈도표(table)는 카테고리컬 data(범주형자료)에 사용
table(df$행정구역)
table(df$시점)

# 기술통계분석 - 연속형자료 - 요약통계량
summary(df$출생아수)

# 기술통계분석 - 연속형자료 - 산점도 그래프
plot(df$출생아수,df$혼인건수)

# 자료 나누기
  # 전국 자료와 지역자료 분리
df_nation <- df %>% filter(df$행정구역=="전국")
df_region <- df %>% filter(df$행정구역!="전국")
df_nation
df_region

# 데이터 분석


# 전국데이터 자연 증가수 그래프
library(ggplot2)
library(ggthemes)

head(df_nation)

qplot(시점, 자연증가수, data=df_nation, fill=시점) + 
  ggtitle("전국 시점별 자연증가수 변화") + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face='bold'))

ggplot(mapping =aes(x=시점, y=자연증가수, fill=시점), data=df_nation) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("전국 시점별 자연증가수 변화")+
  theme_wsj()

plot(df$출생아수, type="o", col="black", xlab='', ylab='')
plot(df$혼인건수, type="o", col="red", xlab='', ylab='')
