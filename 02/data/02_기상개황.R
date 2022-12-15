# 기상개황 자료를 분석하여 월별 불쾌지수와 단계
dfweather <- read.csv("./02_기상개황.csv",
                      header = T, 
                      stringsAsFactors = F, 
                      fileEncoding = 'euc-kr')
names(dfweather)

dfweather <- dfweather[c("월별.1.","평균기온....","평균상대습도....")]
names(dfweather) <- c("월별","평균기온","평균상대습도")
dfweather <- na.omit(dfweather)
dfweather

# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE

# 불쾌지수 공식
# DI = 0.81 * Ta + 0.01 * RH(0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)

dfweather$불쾌지수 <- round((0.81 * dfweather$평균기온) + (0.01 * dfweather$평균상대습도*(0.99 * dfweather$평균기온 - 14.3)) + 46.3,1)

# 불쾌지수 단계
# 매우높음: 80이상 
# 높음: 75이상 80미만 
# 보통: 68이상 75미만 
# 낮음: 68미만

dfweather$불쾌단계 <- ifelse(dfweather$불쾌지수>=80,"매우높음",
                        ifelse(dfweather$불쾌지수>=75,"높음",
                               ifelse(dfweather$불쾌지수>=68,"보통","낮음")))
dfweather

## 연간 데이터 제외
dfweather <- dfweather[2:13,]

## 불쾌지수 빈도표
tbl <- table(dfweather$불쾌단계)
tbl
class(tbl)

## barplot으로 그래프 그리기
barplot(tbl, col=rainbow(3), main="불쾌지수")


## ggplot으로 그래프 그리기
install.packages("ggplot2")
library("ggplot2")

dftbl <- as.data.frame(tbl)
class(dftbl)
dftbl

ggplot(mapping =aes(x=Var1, y=Freq), data=dftbl) + 
  geom_bar(stat="identity") + ggtitle("불쾌지수") +
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
