# json 데이터 가져오기
install.packages("jsonlite")
library(jsonlite)

# 일일 박스오피스 자료 가져오기
box <- function(dt) {
  #문자열 연결하여 url 문자열 만들기
  apikey <- "f5eef3421c602c6cb7ea224104795888"
  #paste 사용 시 연결부위에 공백이 들어감 > sep로 공백 삭제
  url <- paste("https://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?",
              "key=", apikey, "&targetDt=", dt, sep="")
  
  # json 데이터를 list로 가져오기
  mv <- fromJSON(url)
  
  # 박스오피스 목록 추출
  BoxOfficeList <- mv$boxOfficeResult$dailyBoxOfficeList
  return (BoxOfficeList)
}

dt <- scan()
df <- box(dt)

# 데이터형변환 : 수치데이터로 변환
names(df)
str(df)

col <- c("rnum","rank","rankInten",
         "salesAmt","salesShare","salesInten",
         "salesChange","salesAcc",
         "audiCnt","audiInten","audiChange",
         "audiAcc","scrnCnt","showCnt")

# unlist(): list를 vector로 바꿈
for (c in col) {
  df[c] <- as.numeric(unlist(df[c]))
}

str(df)

# 일매출평균보다 매출이 높은 영화
# 매출액 : salesAmt
library(dplyr)

names(df)
highSalesMv <- df %>%
                  filter(salesAmt > mean(salesAmt)) %>%
                  select(rank, movieNm, salesAmt)
highSalesMv
