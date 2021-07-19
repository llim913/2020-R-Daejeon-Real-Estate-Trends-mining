library(ROSE)
library(caret)
library(rpart)
library(rpart.plot)
library(party)
library(e1071)
library(klaR)
library(mlbench)
library(class)
library(class)
library(kknn)
library(nnet)
library(devtools)
library(neuralnet)
library(NeuralNetTools)
library(ggplot2)
library(dplyr)

data1 <- read.csv(choose.files(),header = T)
pay2 <- read.csv(choose.files(),header = T)
pay1 <- read.csv(choose.files(),header = T)
bill <- read.csv(choose.files(),header = T)

# 분석 대상 데이터 할당
# 도시 재생 활성화 지역 데이터
home <- data1
head(home)
summary(home)


# 집값 데이터
# 아파트와 다세대 주택 실거래가 기준
apt <- pay1[c(5:64037),]
head(apt)

vilra <- pay2[c(5:8159),]
head(vilra)

# 국민카드 거래량

kb <- bill
head(kb)

### 동에서 구로 _ apt

apt$시군구 <- '대덕구'
apt$시군구 <- ifelse ((apt$법정동 %in%  c('원동', '정동', '중동', '소제동', '신안동', '인동', '신흥동',	'효동', '천동', '가오동',	'판암동', '삼정동', 
                                    '용운동' ,'대동'  ,'자양동' , '가양동', '용전동' ,'성남동', '홍도동', '삼성동' , '추동', '효평동', '직동', '비룡동', '주산동', '용계동', 
                                    '마산동', '세천동','신상동', '신하동', '신촌동', '사성동', '내탑동', '오동', '주촌동' ,'낭월동', '대별동', '이사동', '대성동', '장척동', 
                                    '소호동', '구도동', '삼괴동', '상소동', '하소동')), "동구",
                   ifelse ((apt$법정동 %in%   c('대사동','대흥동','목동','문창동','문화동','부사동','산성동', '사정동', 
                                             '안영동', '구완동', '무수동', '침산동', '목달동', '정생동', '어남동', '금동',
                                             '석교동', '호동', '옥계동','오류동','용두동','유천동','은행동', '선화동','중촌동','태평동')), "중구",
                           ifelse ((apt$법정동 %in%   c( '복수동' , '도마동' , '정림동', '괴곡동', '변동', '용문동', '탄방동', '둔산동',  
                                                      '괴정동', '가장동' , '내동','갈마동', '월평동',
                                                      '만년동', '가수원동', '도안동','관저동', '흑석동', '매노동', '산직동', '장안동', '평촌동', '오동', 
                                                      '우명동', '원정동', '용촌동', '봉곡동')), "서구", 
                                   ifelse ((apt$법정동 %in%   c('원내동', '대정동', '계산동', '교촌동', '용계동', '학하동', '송정동', '방동', '성북동', '세동', 
                                                             '원신흥동', '상대동', '봉명동' ,  '구암동', '덕명동', '복용동', '장대동', '궁동', '죽동', '어은동', 
                                                             '구성동','노은동', '지족동', '갑동', '죽동',  '죽동', '외삼동', '안산동', '수남동', '반석동', '신성동', 
                                                             '도룡동', '추목동', '신봉동', '화암동', '하기동', '가정동', '장동', '자운동', '덕진동', '방현동',	'전민동',
                                                             '문지동', '원촌동','송강동', '봉산동', '대동', '금탄동', '구룡동', '둔곡동', '금고동', '신동',	'관평동', '용산동',
                                                             '탑립동')), "유성구", "대덕구"))))
apt <- apt[,-c(9)]
tail(apt)

### 동에서 구 _ vilra
vilra$시군구 <- '대덕구'
vilra$시군구 <- ifelse ((vilra$법정동 %in%  c('원동', '정동', '중동', '소제동', '신안동', '인동', '신흥동',	'효동', '천동', '가오동',	'판암동', '삼정동', 
                                    '용운동' ,'대동'  ,'자양동' , '가양동', '용전동' ,'성남동', '홍도동', '삼성동' , '추동', '효평동', '직동', '비룡동', '주산동', '용계동', 
                                    '마산동', '세천동','신상동', '신하동', '신촌동', '사성동', '내탑동', '오동', '주촌동' ,'낭월동', '대별동', '이사동', '대성동', '장척동', 
                                    '소호동', '구도동', '삼괴동', '상소동', '하소동')), "동구",
                   ifelse ((vilra$법정동 %in%   c('대사동','대흥동','목동','문창동','문화동','부사동','산성동', '사정동', 
                                             '안영동', '구완동', '무수동', '침산동', '목달동', '정생동', '어남동', '금동',
                                             '석교동', '호동', '옥계동','오류동','용두동','유천동','은행동', '선화동','중촌동','태평동')), "중구",
                           ifelse ((vilra$법정동 %in%   c( '복수동' , '도마동' , '정림동', '괴곡동', '변동', '용문동', '탄방동', '둔산동',  
                                                      '괴정동', '가장동' , '내동','갈마동', '월평동',
                                                      '만년동', '가수원동', '도안동','관저동', '흑석동', '매노동', '산직동', '장안동', '평촌동', '오동', 
                                                      '우명동', '원정동', '용촌동', '봉곡동')), "서구", 
                                   ifelse ((vilra$법정동 %in%   c('원내동', '대정동', '계산동', '교촌동', '용계동', '학하동', '송정동', '방동', '성북동', '세동', 
                                                             '원신흥동', '상대동', '봉명동' ,  '구암동', '덕명동', '복용동', '장대동', '궁동', '죽동', '어은동', 
                                                             '구성동','노은동', '지족동', '갑동', '죽동',  '죽동', '외삼동', '안산동', '수남동', '반석동', '신성동', 
                                                             '도룡동', '추목동', '신봉동', '화암동', '하기동', '가정동', '장동', '자운동', '덕진동', '방현동',	'전민동',
                                                             '문지동', '원촌동','송강동', '봉산동', '대동', '금탄동', '구룡동', '둔곡동', '금고동', '신동',	'관평동', '용산동',
                                                             '탑립동')), "유성구", "대덕구"))))



vilra <- vilra[,-c(10)]
tail(vilra)
tail(apt)




### na를 0으로 변경
kb[is.na(kb$X3월.억원.),'X3월.억원.'] = 0 
kb[is.na(kb$X4월.억원.),'X4월.억원.'] = 0 
kb[is.na(kb$X5월.억원.),'X5월.억원.'] = 0
kb[is.na(kb$X6월.억원.),'X6월.억원.'] = 0
kb[is.na(kb$X7월.억원.),'X7월.억원.'] = 0
kb[is.na(kb$X7월.억원.),'X7월.억원.'] = 0

### kb 총합계 만들기
sumallkb = kb[,c(-1,-2,-3)]
kb$소비합계 <-rowSums(sumallkb)
kb[is.na(kb$소비합계),'소비합계'] = 0

### kb 총합계 만들기
# 확진자 1062명 3월 1일# 은혜의 강 교회 발 소금물 분사 사건
# 2020년 데이터와 2019년 데이터가 있음
kb19 <- subset(kb, 기준년도 == '2019년')
kb20 <- subset(kb, 기준년도 == '2020년')




kb19 %>% head()
kb20 %>% head()
apt %>% head()
vilra %>% head()

apt19 %>% head()
vilra19 %>% head()


#19년도 부터 데이터 가져오기 
# 데이터 기각은 15년 1월부터 20년 8월까지임 

apt19 <- subset(apt,기준년월 >= 201901)
vilra19 <- subset(vilra,기준년월 >= 201901)

summary(apt19)
summary(vilra19)



# 만들어진 데이터를 깔끔하게 저장한다.
write.csv(apt, "C:/Users/smcom/Desktop/마이닝/apt.csv")
write.csv(kb19, "C:/Users/smcom/Desktop/마이닝/kb19.csv")
write.csv(kb20, "C:/Users/smcom/Desktop/마이닝/kb20.csv")
write.csv(vilra, "C:/Users/smcom/Desktop/마이닝/vilra.csv")
write.csv(apt19, "C:/Users/smcom/Desktop/마이닝/apt19.csv")
write.csv(vilra19, "C:/Users/smcom/Desktop/마이닝/vilra19.csv")



library(arules)

### 분석 목적 : 대전시내 아파트 위치 선호도 시장 조사하기
# 목표 변수는 아파트와 빌라 모두 '법정동'으로 동일 
# 설명 변수는 이름, 층, 시군구, 거래금액(범주형)

apt19 %>% head()
vilra19 %>% head()


# 분석에서 제외할 데이터 일부 삭제
apt1920 <- apt19[,-c(2,4)]


# 결측값 제거
apt1 <- subset(apt1920, !is.na(법정동))
apt2 <- subset(apt1, !is.na(아파트명))
apt3 <- subset(apt2, !is.na(층))
apt4 <- subset(apt3, !is.na(거래금액.일부보기))
apt5 <- subset(apt4, !is.na(시군구))
apt5$층 <- as.numeric(apt5$층)

str(apt5)
str(vilra6)

sapply(apt5, class)

# 모형구축을 위한 훈련용 자료 생성.
#(training data)와 모형의 성능을 검증하기위한 검증용 자료(test data)를 70%와 30%로 구성한다.

set.seed(1234)
ind <- sample(2, nrow(apt5), replace=TRUE, prob=c(0.7, 0.3))
ind


train_apt <- apt5[ind==1, ] # n=2136개
test_apt <- apt5[ind==2, ] # n=797개

#따로 저장 

write.csv(train_apt, "C:/Users/smcom/Desktop/마이닝/train_apt.csv")
write.csv(test_apt, "C:/Users/smcom/Desktop/마이닝/test_apt.csv")


### 훈련자료: winequality_red_train.csv
data.train <- read.csv(file.choose(),header = T, stringsAsFactors = T)
### 검정자료: winequality_red_test.csv
data.test <- read.csv(file.choose(),header = T, stringsAsFactors = T)



# 수업에서 다루지 않은 워드 클라우드로 진행
# 패키지 설치
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

# 거래 빈도가 50 이하인 아파트는 제외한다 최대 거래량은 한계를 두지 않는다.
# 단어의 위치와 색은 빈도순이다.
# 
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt5$아파트명, apt5$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)



apt15 <- subset(apt,기준년월 >= 201501)
apt15 <- subset(apt15,기준년월 <= 201512)
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt15$아파트명, apt15$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)



apt16 <- subset(apt,기준년월 >= 201601)
apt16 <- subset(apt16,기준년월 <= 201612)
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt16$아파트명, apt16$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)


apt17 <- subset(apt,기준년월 >= 201701)
apt17 <- subset(apt17,기준년월 <= 201712)
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt17$아파트명, apt17$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)


apt18 <- subset(apt,기준년월 >= 201801)
apt18 <- subset(apt18,기준년월 <= 201812)
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt18$아파트명, apt18$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)


apt19 <- subset(apt,기준년월 >= 201901)
apt19 <- subset(apt19,기준년월 <= 201912)
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt19$아파트명, apt19$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)


apt2020 <- subset(apt,기준년월 >= 202001)
apt2020 <- subset(apt2020,기준년월 <= 202009)
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt2020$아파트명, apt2020$거래금액.일부보기, scale=c(4,0.5),min.freq=50,max.words=Inf, 
          rot.per=0.25,random.order=F, random.color=F, colors=palete)






# 거래 금액을 내림차순으로 정리
apt5_1_0 <- apt5[c(order(apt5$거래금액.일부보기,decreasing = T)),]
rownames(apt5_1_0) <- NULL
apt5_1_0 %>% head()
apt5_1_0 %>% tail()


# 거래금액 상위 50개에 대한 막대그래프
# 트리풀 시티에 대한 선호가 압도적 
# 단독 거래가가 가장 높은건 스마트 시티 5단지 12층
# 202003 도룡동   스마트시티5단지 12   134.97  120000 유성구
ggplot(data=apt5_1_0[3167:3217,], aes(x=아파트명, y=거래금액.일부보기,fill=아파트명)) +
  coord_flip() + geom_bar(stat="identity")




# 가장 선호하고 거래 횟수도 많은 층수도 알아보자.
ggplot(data=apt5_1_0, aes(x=층, y=거래금액.일부보기,fill=층)) +
  coord_flip() + geom_bar(stat="identity")


# 같은걸 빌라에도 해보자.

# 분석에서 제외할 데이터 일부 삭제
vilra1920 <- vilra19[,-c(2,4)]


# 결측값 제거
vilra1 <- subset(vilra1920, !is.na(법정동))
vilra2 <- subset(vilra1, !is.na(연립주택명))
vilra3 <- subset(vilra2, !is.na(층))
vilra4 <- subset(vilra3, !is.na(거래금액.일부보기))
vilra5 <- subset(vilra4, !is.na(시군구))
vilra6 <- subset(vilra5, !is.na(건축년도))
vilra6$층 <- as.numeric(vilra6$층)

str(vilra6)


sapply(vilra6, class)


# 거래 빈도가 50 이하인 빌라는 제외한다 최대 거래량은 한계를 두지 않는다.
# 단어의 위치와 색은 빈도순이다.
# 
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(vilra6$연립주택명, vilra6$거래금액.일부보기, scale=c(4,0.5),
          min.freq=50,max.words=Inf, rot.per=0.25,random.order=F, 
          random.color=F, colors=palete)

palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(vilra6$법정동, vilra6$거래금액.일부보기, scale=c(4,0.5),
          min.freq=50,max.words=Inf, rot.per=0.25,random.order=F, 
          random.color=F, colors=palete)

home

wordcloud(home$동명, home$뉴딜유형, scale=c(4,0.5),
          min.freq=50,max.words=Inf, rot.per=0.25,random.order=F, 
          random.color=F, colors=palete)


# 거래 금액을 내림차순으로 정리
vilra5_1_0 <- vilra6[c(order(vilra6$거래금액.일부보기,decreasing = T)),]
rownames(vilra5_1_0) <- NULL
vilra5_1_0 %>% head()
vilra5_1_0 %>% tail()


# 거래금액 상위 50개에 대한 막대그래프
# 482   202005 용문동 성진아트빌라  3  1996  55.44   46390   서구
# 74-12는 한마음 식당 옆옆에있는 상업용 빌라. 
# 단기간 거래 횟수가 많은 것으로 추정
# 현재는 촌댁맥주

dim(vilra5_1_0)
ggplot(data=vilra5_1_0[432:482,], aes(x=연립주택명, y=거래금액.일부보기,fill=연립주택명)) +
  coord_flip() + geom_bar(stat="identity")




# 가장 선호하고 거래 횟수도 많은 층수도 알아보자.
ggplot(data=vilra5_1_0, aes(x=층, y=거래금액.일부보기,fill=층)) +
  coord_flip() + geom_bar(stat="identity")



# 가장 주택거래가 활발하게 이루어지는 법정동은 어디인가?
# 거래 빈도 최소 거래량 = 100,  최대 거래량은 한계를 두지 않는다.
# 단어의 위치와 색은 빈도순이다.
# 
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(apt5$법정동, apt5$거래금액.일부보기, scale=c(4,0.5), min.freq=100,max.words=Inf,
          rot.per=0.25,random.order=F, random.color=F, colors=palete)
warnings()



# 가장 연립주택 거래가 활발하게 이루어지는 법정동은 어디인가?
# 거래 빈도 최소 거래량 = 100,  최대 거래량은 한계를 두지 않는다.
# 단어의 위치와 색은 빈도순이다.
# 
palete <- brewer.pal(8, "Dark2") # 글자색 지정
wordcloud(vilra6$법정동, vilra6$거래금액.일부보기, scale=c(4,0.5), min.freq=100,max.words=Inf,
          rot.per=0.25,random.order=F, random.color=F, colors=palete)
warnings()

table(vilra6$시군구, vilra6$법정동)



### 빈도를 눈에 보이게 구해보자
table(apt5$아파트명 , apt5$층)

prop.table(table(apt5$아파트명))

##### 알고 싶은 것 아파트의 지리적 위치와 집 값의 관계 
# 연관 규칙으로 알아 보자

# transaction matrix 만들기
Place.list = split(apt5$법정동, apt5$거래금액.일부보기) 
Place.transaction = as(Place.list, "transactions") 
Place.matrix = as(Place.transaction,'matrix') 
Sums = sort(colSums(Place.matrix),decreasing = TRUE) 
Place.matrix = Place.matrix[,names(Sums)]

# Place.transaction 
rules_0.15 = apriori(Place.transaction, parameter = list(support = 0.15, confidence = 0.2, minlen = 1))

rule.list_0.15 = as.data.frame(inspect(rules_0.15))















