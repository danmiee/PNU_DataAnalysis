library(jsonlite)

getData <- function(year,month) {
  svckey <- "4xNzh5YEW00McPCdnW2TlqesP8ihPHTraV8tsYgaucV7tSeG6S87J%2BGUmK99xI%2Bk6o2u4cIVldDFMYZOc9QX%2Fg%3D%3D"
  
  url <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?",
               "ServiceKey=",svckey,
               "&type=json&page=1&rowNum=7&disYear=",year,"&disMonth=",month, sep="")
  
  datas <- fromJSON(url)
  
  dataList <- datas$data$list
  
  names(dataList) <- c('배출년','배출월','배출요일','요일갯수',
                 '배출량','일평균배출량','배출횟수','일평균배출횟수')
  
  # 데이터 열 생성
  disDay <- c('일','월','화','수','목','금','토')
  # df <- df %>%
  #   mutate(disweek = case_when(disDay == 1 ~ '일',
  #                              disDay == 2 ~ '월',
  #                              disDay == 3 ~ '화',
  #                              disDay == 4 ~ '수',
  #                              disDay == 5 ~ '목',
  #                              disDay == 6 ~ '금',
  #                              disDay == 7 ~ '토'))
  
  for (n in 1:7) {
    dataList$배출요일[n] <- disDay[n]
  }
  
  # 사용자 지정 정렬
  dataList$배출요일 <- factor(dataList$배출요일, levels=disDay)  # 기준칼럼 형변환, levels에 사용자 지정 정렬용 vector 지정
  dataList <- dataList[order(dataList$배출요일, decreasing=FALSE), ]   # 정렬
return (dataList)
}

# 그래프 합치기

# 2020~2022 8월 데이터 가져오기
# df <- getData('2020','08')
# df <- rbind(df, getData('2021','08'))
# df <- rbind(df, getData('2022','08'))
df202008 <- getData('2020','08')
df202108 <- getData('2021','08')
df202208 <- getData('2022','08')
df <- bind_rows(df202008, df202108, df202208)
df
plot(df)

library(dplyr)
# 요일별
df1 <- df %>% 
  group_by(배출요일) %>%
  summarise(평균배출량 = mean(배출량),
            평균배출횟수 = mean(배출횟수))
df1
plot(df1)

library(ggplot2)
library(ggthemes)
ggplot(data=df1, mapping=aes(x=배출요일, y=평균배출량, group=1)) +
  geom_line()+ geom_point()+
  ggtitle("2020~2022 8월 요일별 평균 배출량")

# 연도별 배출량 그래프
ggplot(mapping =aes(x=배출년, y=배출량, fill=배출년), data=df) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("2020~2022 8월 연도별 배출량")

# 연도별 배출횟수 그래프
ggplot(mapping =aes(x=배출년, y=배출횟수, fill=배출요일), data=df) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("2020~2022 8월 연도별 배출횟수")

# 반복문으로 1년치 데이터 가져오기
df2021 <- data.frame();

for(i in 1:12) {
  if(i<10) {m = paste("0",i,sep="")}
  else {m = as.character(i)}
  
  temp <- getData("2021", m)
  df2021 <- bind_rows(df2021, temp)
}

head(df2021)

ggplot(mapping =aes(x=배출월, y=배출횟수, group=배출요일, color=배출요일), data=df2021) + 
  geom_line()+ geom_point()+
  ggtitle("2021 월별 요일별 쓰레기 배출횟수")

ggplot(mapping =aes(x=배출월, y=배출량, fill=배출요일), data=df2021) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle("2021 월별 쓰레기 배출량")

  # 이중축 그래프 찾아보기
# ggplot(df2021, aes(x = 배출월))+
#   geom_line(aes(y = 배출횟수, color = "red"))+
#   geom_bar(aes(y = 배출량), fill = "darkgrey", stat='identity')+
#   scale_y_continuous(name="배출횟수",
#                      sec.axis = sec_axis(trans=~ ., name = "배출량"))

