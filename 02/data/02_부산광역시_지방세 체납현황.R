# 함수 만들기
makedf <- function(i) {
  return(i*2)
}
makedf(3)

# 부산시 체납현황 분석
install.packages("readcsv")
library("readcsv")

dftax <- read.csv("./02_부산광역시_지방세 체납현황.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr')
head(dftax)
names(dftax)

dftax <- dftax[c("과세년도","세목명","체납액구간","누적체납건수","누적체납금액")]
head(dftax)

# df1 <- df[df$세목명=="등록면허세",]
# df2 <- df[df$세목명=="자동차세",]
# df3 <- df[df$세목명=="주민세",]
# df4 <- df[df$세목명=="지방소득세",]
# df5 <- df[df$세목명=="지역자원시설세",]
# df6 <- df[df$세목명=="취득세",]
# 
# head(df6)


# 세목명 확인
cols = unique(dftax$세목명)
cols

# 과세년도 범주형으로 변경
dftax$과세년도 <- as.factor(dftax$과세년도)

# 함수 생성
install.packages("ggplot2")
library("ggplot2")

makedf <- function (item) {
  dftax <- dftax[dftax$세목명==item,]
  ggplot(mapping =aes(x=과세년도, y=누적체납금액, fill=체납액구간), data=dftax) + 
    geom_bar(stat="identity", position=position_dodge())  + 
    ggtitle(item)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}
makedf(cols[5])

# solution
## 과세년도 범주형으로 변경
dftax$과세년도 <- as.factor(dftax$과세년도)

## 함수 생성
makedfS <- function(item) {
  temp <- subset(dftax, dftax$세목명 == item)
  ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill=과세년도), data=temp) + 
    geom_bar(stat="identity", position=position_dodge())  + 
    ggtitle(item)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}
makedfS("지방소득세")
