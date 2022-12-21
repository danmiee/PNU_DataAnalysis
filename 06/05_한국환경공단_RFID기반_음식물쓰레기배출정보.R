library(jsonlite)

RfidFoodWasteService <- function(year,month) {
  svckey <- "4xNzh5YEW00McPCdnW2TlqesP8ihPHTraV8tsYgaucV7tSeG6S87J%2BGUmK99xI%2Bk6o2u4cIVldDFMYZOc9QX%2Fg%3D%3D"
  url <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?",
               "ServiceKey=",svckey,
               "&type=json&page=1&rowNum=7&disYear=",year,"&disMonth=",month, sep="")
  url
  datas <- fromJSON(url)
  
  dataList <- datas$data$list
  
  names(dataList) <- c('배출년','배출월','배출요일','요일갯수',
                 '배출량','일평균배출량','배출횟수','일평균배출횟수')
  
  disDay <- c('일','월','화','수','목','금','토')
  for (n in 1:7) {
    dataList$배출요일[n] <- disDay[n]
  }
return (dataList)
}

df <- RfidFoodWasteService('2020','08')
df <- rbind(df, RfidFoodWasteService('2021','08'))
df <- rbind(df, RfidFoodWasteService('2022','08'))
df
