getwd()
setwd("C:/Users/user/git/Rwork/training/09")

#한국환경공단_에어코리아_대기오염통계 현황
#시도별 실시간 평균정보 조회 상세기능명세
#최근 한달간 지역별 일평균 대기오염 정보
#기준초과인경우 
#https://www.airkorea.or.kr/web/contents/contentView/?pMENU_NO=132&cntnts_no=6

#json처리
install.packages("jsonlite") 
library(jsonlite)

#자료처리
library(dplyr)
itemcode="O3"
scondition="MONTH"
gubun="DAILY"
svckey="4xNzh5YEW00McPCdnW2TlqesP8ihPHTraV8tsYgaucV7tSeG6S87J%2BGUmK99xI%2Bk6o2u4cIVldDFMYZOc9QX%2Fg%3D%3D"
url = paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?",
            "itemCode=",itemcode,
            "&dataGubun=",gubun,
            "&searchCondition=",scondition,
            "&pageNo=1&numOfRows=100&returnType=json&serviceKey=",svckey,
            sep="")

datas <- fromJSON(url)

dataList <- datas$response$body$items

head(dataList)

#데이터 가져오기함수

getData <- function(item, scondition, gubun) {
  svckey="4xNzh5YEW00McPCdnW2TlqesP8ihPHTraV8tsYgaucV7tSeG6S87J%2BGUmK99xI%2Bk6o2u4cIVldDFMYZOc9QX%2Fg%3D%3D"
  url = paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?",
              "itemCode=",item,
              "&dataGubun=",gubun,
              "&searchCondition=",scondition,
              "&pageNo=1&numOfRows=100&returnType=json&serviceKey=",svckey,
              sep="")
  
  datas <- fromJSON(url)
  
  dataList <- datas$response$body$items
  
}


#1. pm10, O3 데이터 추출하여 합치기
df <- data.frame()
df1 <- getData("PM10","MONTH","DAILY")
df2 <- getData("O3","MONTH","DAILY")
df <- bind_rows(df1, df2)

df


#2. 지역명 벡터
area <- c("seoul", "busan", "daegu","incheon","gwangju",
          "daejeon", "ulsan", "gyeonggi", "gangwon",
          "chungbuk", "chungnam", "jeonbuk", "jeonnam",
          "gyeongbuk", "gyeongnam", "jeju", "sejong")

areaname <- c("서울","부산","대구","인천","광주","대전",
              "울산","경기","강원","충북","충남","전북",
              "전남","경북","경남","제주","세종")
length(area)
length(areaname)
names(df)


#3. 통합데이터프레임 만들기

# dfa <- data.frame()
# for(i in 1:length(area)) {
#   temp <- c()
#   temp$dataTime <- unlist(df['dataTime'])
#   temp$itemCode <- unlist(df['itemCode'])
#   temp$area <- areaname[i]
#   temp$item <- unlist(df[area[i]])
#   
#   dfa <- bind_rows(dfa, temp)
# }
# dfa

dft <- data.frame()
for(i in 1:length(area)) {
  # area 인덱스를 기준으로 지역별 데이터프레임 생성
  t <- df[c('dataTime','itemCode',area[i])]
  # 지역명 추가
  t$area <- areaname[i]
  # 열이름 변경
  names(t) <- c('dataTime', 'itemCode','item','area')
  # 데이터프레임 결합
  dft <- bind_rows(dft, t)
}
dft

#4.주의보
#https://www.airkorea.or.kr/web/dustForecast?pMENU_NO=113
dft$item <- as.numeric(dft$item)

dft$기준 <- ifelse(dft$itemCode=='PM10',
                  ifelse(dft$item <= 30, "좋음",
                         ifelse(dft$item <= 80, "보통",
                                ifelse(dft$item <= 150, "나쁨","매우나쁨"))),
                  ifelse(dft$item <= 0.030, "좋음",
                         ifelse(dft$item <= 0.090, "보통",
                                ifelse(dft$item <= 0.150, "나쁨","매우나쁨"))))

head(dft)

#5.일자별 주의보정보
names(dft)
str(dft)
summary(dft)

is.factor(dft$기준)
dft$기준 <- as.factor(dft$기준)

# dfn2 <- dft %>%
#   group_by(c(dataTime,주의보)) %>%
#   summarise(
#     기준수 = n()
#   )

dfn <- table(dft$dataTime,dft$기준)
dfn
dfn1 <- as.data.frame.matrix(dfn)
dfn1
dfn2 <- as.data.frame(dfn)
dfn2

names(dfn2) <- c("일자", "기준","기준수")

library(ggplot2)
ggplot(dfn2, aes(x=일자, y =기준수, group=기준, color=기준)) +
  geom_line() +
  geom_point() +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#5.지역별 PM10이 좋음인 날 수 
names(dft)

# PM10이 좋음인 날의 데이터 선별
dfPM10 <- dft %>%
            filter(dft$itemCode=="PM10" & dft$기준=="좋음")

# 지역별 빈도표 작성하여 데이터프레임으로 변경
dfPM10 <- as.data.frame(table(dfPM10$area))
names(dfPM10) <- c('area','n')

ggplot(dfPM10, aes(x=area, y=n, fill=area)) +
  geom_bar(stat="identity") +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

