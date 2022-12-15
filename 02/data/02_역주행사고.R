# 연도별 치명률

## 엑셀파일 불러오기
install.packages("readxl")
library("readxl")

ac_excel <- read_excel(path = "./02_역주행사고.xlsx")
ac_excel

## 테이블 나누기(전체 / 역주행)
df <- data.frame(ac_excel)
df_entire <- subset(df, df$구분 == "전체")
df_entire
df_reverse <- df[df$구분 == "역주행", ] # 쉼표 반드시 있어야함
df_reverse

## 일반 교통사고 데이터 만들기
df_general <- df_entire
df_general$구분 <- "일반"
df_general[c("사고","사망")] <- df_entire[c("사고","사망")]-df_reverse[c("사고","사망")]
df_general

## 치명률 구하기 - (치명률) = (사망)/(사고)*100
df_entire$치명률 <- round(df_entire$사망/df_entire$사고*100,2)
df_reverse$치명률 <- round(df_reverse$사망/df_reverse$사고*100,2)
df_general$치명률 <- round(df_general$사망/df_general$사고*100,2)

## 기초통계값
summary(df_reverse)

## 치명률 평균 계산
mean(df_reverse$치명률)
mean(df_general$치명률)

# 3년 평균의 사고건수, 사망인원, 치명률
cat("최근 3년간 역주행 교통사고의 치명률이 ", 
    round(mean(df_reverse$치명률),1),
    "%로 일반 교통사고(",
    round(mean(df_general$치명률),1),
    "%)보다 ",
    round(mean(df_reverse$치명률)/mean(df_general$치명률),2),
    "배 높은 것으로 나타났다.")

# 시각화
install.packages("ggplot2")
library("ggplot2")

ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=df) + 
  geom_bar(stat="identity", position=position_dodge())  + 
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
