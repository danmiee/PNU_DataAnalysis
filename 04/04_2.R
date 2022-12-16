# 기상개황 자료를 이용하여 월별 불쾌지수를 계산하고 불쾌지수가 높음이상인 월을 구하시오.
# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE
getwd()
setwd("C:/User/user/git/Rwork/training/04")
df <- read.csv("./04_기상개황.csv",
               header=T,
               stringsAsFactors = F,
               fileEncoding = "euc-kr")
head(df)

names(df) <- c("월별", "평균기온", "평균최고기온", "최고극값기온", "평균최저기온", "최저극값기온", "강수량", "평균상대습도", "최소상대습도", "평균해면기압", "이슬점온도", "평균운량", "일조시간", "최심신적설", "평균풍속바람", "최대풍속바람", "최대순간풍속")
names(df)

df_di <- df[, c("월별", "평균기온", "평균상대습도", "강수량", "이슬점온도", "평균운량", "일조시간")]
df_di

# 불쾌지수 공식
# DI = 0.81 * Ta + 0.01 * RH * (0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)
df_di$불쾌지수 <- 0.81 * df_di$평균기온 + 0.01 * df_di$평균상대습도 * (0.99 * df_di$평균기온 - 14.3) + 46.3
df_di

# 불쾌지수 단계
# 매우높음: 80이상
# 높음: 75이상 80미만
# 보통: 68이상 75미만
# 낮음: 68미만
df_di$불쾌지수단계 <- ifelse(df_di$불쾌지수 >= 80, "매우높음",
                       ifelse(df_di$불쾌지수 >= 75, "높음",
                              ifelse(df_di$불쾌지수 >= 68, "보통","낮음")))
df_di

# 월별 정렬을 위한 값 변경
df_di$월별 <- ifelse(df_di$월별=="연간",df_di$월별,
                   ifelse(nchar(df_di$월별)==2,paste("0",df_di$월별),df_di$월별))
df_di$월별 <- gsub(" ","",df_di$월별)
df_di

# 빈도표 구하기
tb <- table(df_di$불쾌지수단계)

# 그래프 그리기
  #  qplot(data=데이터, x=x축변수, y=y축변수, geom='그래프형태', colour='색구분기준')
library(ggplot2)
library(ggthemes)

  # 불쾌지수 단계별 빈도수
plot(tb)
qplot(df_di$불쾌지수단계)

  # 불쾌지수와의 관계 파악
plot(df_di$불쾌지수,df_di$강수량)       # 무관
plot(df_di$불쾌지수,df_di$이슬점온도)   # 정비례
plot(df_di$불쾌지수,df_di$평균운량)     # 무관
plot(df_di$불쾌지수,df_di$일조시간)     # 무관

  # 이슬점온도와 불쾌지수 관계 그래프
qplot(이슬점온도, 불쾌지수, data=df_di, fill=월별) + 
  ggtitle("이슬점온도와 불쾌지수 관계") + 
  theme(plot.title = element_text(hjust = 0.5,size=10,face='bold'))

ggplot(mapping =aes(x=이슬점온도, y=불쾌지수), data=df_di) + 
  geom_line(color="red", size=2)  + 
  geom_point(size=3) +
  ggtitle("이슬점온도와 불쾌지수 관계") + 
  theme_wsj()

  # 평균운량과 불쾌지수 관계 그래프
ggplot(mapping =aes(x=평균운량, y=불쾌지수), data=df_di) + 
  geom_line(color="red", size=2)  + 
  geom_point(size=3) +
  ggtitle("평균운량와 불쾌지수 관계") + 
  theme_wsj()

  # 월별 불쾌지수 분석 그래프
    # 막대그래프
g1 <- ggplot(mapping =aes(x=월별, y=불쾌지수, fill=월별, group=1), data=df_di) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("월별 불쾌지수 분석")+
  theme_wsj()
g1
    # 꺾은선그래프
g2 <- ggplot(data=df_di, aes(x=월별, y=불쾌지수, group=1)) +
  geom_line(linetype="dashed",color="red",size=1.2)+
  geom_point(size=2)
g2

# 그래프 합치기
install.packages(gridExtra)
