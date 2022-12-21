getwd()
setwd("C:/Users/user/git/Rwork/training/06")

df1 <- read.csv("./06_지상관측.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr')
head(df1)

# 열명을 '지점', '지점명', '일시', '기온', '풍속', '상대습도'로 변경
names(df1) <- c('지점', '지점명', '일시', '기온', '풍속', '상대습도')
names(df1)

# 체감온도 = 13.12 + 0.6215*T – 11.37*V*0.16 + 0.3965*V*0.16*T
#  T : 기온(°C), V : 풍속(km/h) - 단위변환 필요

# 풍속환산 : 1m/x -> 3.6km/h
df1$풍속환산 <- round(df1$풍속 * 3.6, 2)
head(df1)
df1$체감온도 <- 13.12 + 0.6215*df1$기온 - 11.37*df1$풍속환산*0.16 + 0.3965*df1$풍속환산*0.16*df1$기온
df1$체감온도 <- round(df1$체감온도, 2)

# df1$체감온도 <- 13.12 + 0.6215*df1$기온 - 11.37*(df1$풍속*3.6)*0.16 + 0.3965*(df1$풍속*3.6)*0.16*df1$기온
head(df1)

# 겨울철 체감온도 : 기온 10°C 이하, 풍속 4.68km/h(1.3m/s) 이상인 날
# df1$겨울철체감온도 <- ifelse((df1$기온 <= 10)&(df1$풍속 >= 1.3), 1, 0)
# head(df1)

# 부산지역의 겨울철 체감온도에 해당되는 자료를 추출하여 그래프 그리기
library(dplyr)
# df_bs <- df1 %>% filter((df1$지점명=="부산")&(df1$겨울철체감온도==1))
df_bs <- df1 %>% filter(df1$지점명=="부산" & df1$기온 <= 10 & df1$풍속 >= 1.3)
df_bs

library(ggplot2)
library(ggthemes)
ggplot(mapping =aes(x=일시, y=체감온도, fill=체감온도), data=df_bs) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("부산지역 겨울철 체감온도")+
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold')) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

# 열지수 = HI = 0.5 * {T + 61.0 + [(T-68.0)*1.2] + (RH*0.094)}
# T : 기온(°C), RH : 상대습도(%)
df2 <- read.csv("./06_지상관측.csv", 
               header = T, 
               stringsAsFactors = F, 
               fileEncoding = 'euc-kr')
names(df2) <- c('지점', '지점명', '일시', '기온', '풍속', '상대습도')
head(df2)

df2$열지수 <- 0.5 * (df2$기온 + 61.0 + ((df2$기온-68.0)*1.2) + (df2$상대습도*0.094))
head(df2)

# 지점명이 ’서울‘,’부산‘,’제주‘인 자료를 추출
# df21 <- df2 %>% filter((df2$지점명=='서울')|(df2$지점명=='부산')|(df2$지점명=='제주'))
df21 <- df2 %>% filter(지점명 %in% c('서울','부산','제주'))  # in연산자 활용
df21

# 일자별 기온그래프 : 평균기온을 수평선으로 표시 
ggplot(mapping =aes(x=일시, y=기온, fill=지점명), data=df21) + 
  theme_wsj() +
  geom_hline(yintercept = mean(df21$기온), color = 'red', linetype = 2) +
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("일자별 기온 그래프")+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

# 일자별 열지수 그래프 : 열지수 5를 수평선으로 표시
ggplot(data=df21, mapping=aes(x=일시, y=열지수, group=지점명, color=지점명)) +
  theme_wsj()+
  geom_line()+ geom_point()+
  ggtitle("일자별 열지수 그래프")+
  geom_hline(yintercept = 5, color = 'red', linetype = 1)+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

# 일자별로 그룹핑하여 열지수가 5이하인 자료를 추출하시오.
# group_by() : 반드시 summarise와 함께 사용
df22 <- df21 %>% 
  group_by(일시) %>%
  summarise(평균열지수 = mean(열지수),
            평균기온 = mean(기온))
df22
df22 <- df22 %>%  filter(평균열지수<=5)
df22

  