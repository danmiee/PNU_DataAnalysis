# 해결문제 
# BMI는 몸무게와 키를 이용하여 체지방율을 측정하는 지수이다. 
# 자신의 몸무게와 키를 각각 변수 weight와 height에 저장하고 BMI지수를 계산해 본다. 
# 단, 키는 cm로 입력 받아서 처리한다.

# BMI = 체중(kg) / (키(m) x키(m))

# 키와 몸무게 scala입력
height <- scan()
weight <- scan()

# 키와 몸무게 scala입력
print("키와 몸무게 입력")
data <- scan()
height <- data[1]
weight <- data[2]

# BMI 계산
BMI <- weight / (height /100)**2 


# 키와 몸무게 vector입력

# stringr 패키지 설치
install.packages("stringr")
library(stringr)
 
# 문자열 입력
data <- readline()
data

# 문자열 분리
data <- str_split(data, " ")

# 자료형 확인
mode(data)

# 벡터로 형변환
data <- unlist(data)
 
# 벡터 확인
# mode(data) 
# class(data)
is.vector(data)

# 숫자벡터로 변경
data <- as.numeric(data)
data

height <- data[1]
weight <- data[2]
BMI <- weight / (height /100)**2 


# 데이터프레임 입력
df <- data.frame()
df
mode(df)
class(df)

df <- edit(df)
df 

# 데이터프레임 열명 변경
names(df) <- c("height","weight")
df$BMI <- df$weight / (df$height / 100) ** 2
df

# 해결문제 
getwd()
setwd("C:/Rwork/training/01")
# 국민건강보험공단 자료를 이용하여 BMI와 비만도를 구하시오.
dfbmi <- read.csv("./01_국민건강보험공단500.csv", 
                  header = T, 
                  stringsAsFactors = F, 
                  fileEncoding = 'euc-kr')
dfbmi
head(dfbmi)

dfbmi$BMI <- dfbmi$체중 / (dfbmi$신장 / 100) ** 2

# 비만도 
# 저체중	20 미만
# 정상	20 - 24
# 과체중	25 - 29
# 비만	30 이상
dfbmi$비만도 <- ifelse(dfbmi$BMI<20, "저체중", 
                    ifelse(dfbmi$BMI<25, "정상", 
                           ifelse(dfbmi$BMI<30, "과체중", "비만")))

# 빈도 테이블
tab <- table(dfbmi$비만도, dfbmi$성별)
tab
 
# 빈도 테이블 저장
write.csv(dfbmi, "./data/건강검진비만도.csv", fileEncoding = 'euc-kr', quote = F)
write.csv(tab, "./data/빈도표.csv", fileEncoding = 'euc-kr', quote = F)

# 해결문제
# 국민건강보험 관리공단의 건강검진 자료를 이용하여 대사증후군을 판별하시오.
dfhealth <- read.csv("./대사증후군정보_20211229.CSV",
                     header = T,
                     stringsAsFactors = F, 
                     fileEncoding = 'euc-kr')
head(dfhealth)
is.na(dfhealth)
dfhealth <- na.omit(dfhealth)

# 높은 혈압(130/85mmHg 이상)
dfhealth$고혈압 <- ifelse(dfhealth$수축기.혈압>=130, 1, 0)

# 높은 혈당(공복 혈당 100mg/dL 이상)
dfhealth$고혈당 <- ifelse(dfhealth$식전혈당.공복혈당.>=100, 1, 0)

# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
dfhealth$고중성지방 <-ifelse(dfhealth$트리글리세라이드>=150,  1, 0)

# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
dfhealth$저HDL <- ifelse((dfhealth$성별코드==1 & dfhealth$HDL.콜레스테롤<40) | 
                          (dfhealth$성별코드==2 & dfhealth$HDL콜레스테롤<50), 1, 0)
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
dfhealth$복부비만 <- ifelse((dfhealth$성별코드==1 & dfhealth$허리둘레<40) | 
                          (dfhealth$성별코드==2 & dfhealth$허리둘레<50), 1, 0)
# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군

head(dfhealth)

# Solution
# 국민건강보험 관리공단의 건강검진 자료를 이용하여 대사증후군을 판별하시오.
dfcheck <- read.csv("./국민건강보험공단_건강검진정보_20211229.CSV",
                     header = T,
                     stringsAsFactors = F, 
                     fileEncoding = 'euc-kr')
head(dfhealth)

# column명 확인
names(dfcheck)

# 필요한 열 추출
df2 <- dfcheck[c("성별코드","허리둘레","수축기.혈압","이완기.혈압","식전혈당.공복혈당.","트리글리세라이드","HDL.콜레스테롤")]
head(df2)

# NA 값 제거
df2 <- na.omit(df2)

# column명 변경
names(df2) <- c("성별코드","허리둘레","수축기혈압","이완기혈압","공복혈당","트리글리세라이드","HDL콜레스테롤")
head(df2)

# 높은 혈압(130/85mmHg 이상)
df2$높은혈압 <- ((df2$수축기혈압 >= 130) | (df2$이완기혈압 >= 85))

# 높은 혈당(공복 혈당 100mg/dL 이상)
df2$높은혈당 <- df2$공복혈당 >= 100

# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
df2$높은중성지방 <- df2$트리글리세라이드 >= 150

# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
df2$낮은콜레스테롤 <- (((df2$성별코드 == 1) & (df2$HDL콜레스테롤 < 40)) | ((df2$성별코드 == 2) & (df2$HDL콜레스테롤 <50)))

# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
df2$복부비만 <- (((df2$성별코드 == 1) & (df2$허리둘레 >= 90)) | ((df2$성별코드 == 2) & (df2$허리둘레 >= 85)))

# 대사증후군 수
df2$대사증후군 <- (df2$높은혈압 + df2$높은혈당 + df2$높은중성지방 + df2$낮은콜레스테롤 + df2$복부비만)

# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군
df2$판별 <- ifelse(df2$대사증후군 == 0, "정상", ifelse(df2$대사증후군 <= 2, "주의군", "위험군"))

df2$성별 <- ifelse(df2$성별코드 == 1, "남", "여")

table(df2$판별, df2$성별)
