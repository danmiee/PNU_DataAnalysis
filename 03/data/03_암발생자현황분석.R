# 연령대별 성별 암발생자 현황분석
getwd()
setwd("c:/Rwork/training")
df_cc <- read.csv("./03/03_암발생자수.csv",
                header=T,
                stringsAsFactors = F,
                fileEncoding = "euc-kr")
head(df_cc)

## 칼럼명 변경
names(df_cc) <- c("암종별","성별","연령별","y2019","y2019_1")

## 모든 암에 대한 자료만 추출 
### dplyr 패키지 filter
library(dplyr)
# df_allcc <- subset(df_cc, df_cc$암종별=="모든 암(C00-C96)")
df_allcc <- filter(df_cc, df_cc$암종별=="모든 암(C00-C96)")
# df_allcc <- df_cc %>% filter(df2$암종별=="모든 암(C00-C96)")
head(df_allcc)
names(df_allcc) <- c("암종별","성별","연령별","y2019","y2019_1")

## in 연산자
df_allcc %>% filter(!(연령별 %in% c("계","연령미상")))

## 연령대 별 그룹핑
### 연령별 값이 "계","연령미상"이 아닌 경우만 확인
df_allcc <- df_allcc %>% filter(!(연령별 %in% c("계","연령미상")))

### 연령대 칼럼 생성
head(df_allcc)
unique(df_allcc$연령별)
df_allcc$연령대 <-  ifelse(df_allcc$연령별 %in% c("0-4세","5-9세","10-14세","15-19세","20-24세","25-29세","30-34세","35-39세"),"30대이하",
                        ifelse(df_allcc$연령별 %in% c("40-44세","45-49세","50-54세","55-59세"),"40~50대",
                        ifelse(df_allcc$연령별 %in% c("60-64세","65-69세","70-74세","75-79세"),"60~70대","80대이상")))
unique(df_allcc$연령대)

### dplyr 패키지 group_by
class(df_allcc$y2019)
df_allcc$y2019 <- as.numeric(df_allcc$y2019)

df_allcc_g <- df_allcc %>% 
              group_by(연령대, 성별) %>%
              summarise(계 = sum(y2019))
df_allcc_g

library(ggplot2)
ggplot(mapping =aes(x=연령대, y=계, fill=성별), data=df_allcc_g) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("연령대별 성별 분석")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

## 행인덱스 초기화
### rownames(df) <- NULL

