###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 1
## 박종희
## 2020/06/13
###################################
source("index.R")

## ----echo=TRUE-----------------------------------------------------------------
## xlsx 파일 자료를 읽을 수 있는 패키지 로딩
library(readxl)          

download.file("http://gapm.io/dl_pop", destfile = "pop1800_2100.xlsx")

## 다운로드된 xlsx 파일에서 두 번째 시트를 선택해서 불러오기
world_pop = read_xlsx("pop1800_2100.xlsx", sheet = 2)


## ----echo=TRUE-----------------------------------------------------------------
## 객체의 클래스를 확인
class(world_pop)

## 데이터 프레임의 첫 6줄만 보여주기
head(world_pop)


## ---- echo=TRUE----------------------------------------------------------------
## 비함수형 프로그래밍
1 + 2

## 함수형 프로그래밍
`+` (1, 2)


## ----worldpop, fig.cap="세계인구증가: 1800 - 2019", echo=TRUE,  fig.align="center", fig.asp = 0.7, fig.pos = 'ht', fig.fullwidth=TRUE----

## 0. 2020년 이전 자료만 따로 저장
world_pop %>% filter(year < 2020) %>%

  ## 1. 자료와 x축과 y축 변수를 정의
  ggplot(aes(x = year, y=Population)) + 

  ## 2. 라인 그리기
  geom_line(size=2, alpha=0.6, color="forestgreen") + 

  ## 3. 레이블 입력
  labs(caption = "원자료: Gapminder") + 
  
  ## 4. 그래프의 형식 지정
  theme_jhp(base_size = 10, base_family = "sans") +
  
  ## 5. 축이름 지정
  xlab("연도") + ylab('인구')


## ----worldpop2, fig.cap="로그 변환된 세계인구증가: 1800 - 2019", echo=TRUE,  fig.align="center", fig.asp = 0.7, fig.pos = 'ht', warning=FALSE, fig.fullwidth=TRUE----
## 0. 인구자료 로그변환하여 
## 데이터 프레임에 새 변수로 지정
sub <- subset(world_pop, year < 2020) 
sub$log.pop <- log(sub$Population)

ggplot(sub, aes(x = year, y=log.pop)) + 
  geom_line(size=2, alpha=0.8, color="forestgreen") + 
  geom_smooth(method="lm", color = "firebrick2") +
  labs(caption = "원자료: Gapminder") + 
  theme_jhp() + xlab("연도") + ylab('인구 (log)')


## ----worldpopbreak, fig.cap="세계인구증가: 1950년 분절점을 이용하여 두 개의 선형모형으로 근사", echo=TRUE,  fig.align="center", fig.asp = 0.7, fig.pos = 'ht', fig.fullwidth=TRUE----
## 1950년을 기점으로 구분하는 더미변수 생성
sub$period <- ifelse(sub$year < 1951, "Before-1950", "Post-1950")

## x축과 y축 자료 지정, 그룹 정보와 색 정보도 지정
ggplot(sub, aes(x = year, y=log.pop, group=period, color=period)) + 
  geom_line(size=2, alpha=0.8, color="forestgreen") + 
  geom_smooth(method="lm", color = "firebrick2") +
  labs(caption = "원자료: Gapminder") + 
  theme_jhp() + xlab("연도") + ylab('인구 (log)')


## ----echo=TRUE-----------------------------------------------------------------
## 자료 github에서 불러오기
library (readr)
urlfile = "https://raw.githubusercontent.com/jongheepark/BayesianSocialScience/master/long-term-cereal-yields-in-the-united-kingdom.csv?token=AATGJDFKO2PW6JMDTV7NCJS7NQVSO"
uk_crop <-read_csv(url(urlfile))
## If you download the data in your workind directory, you can import by 
## uk_crop = read.csv("long-term-cereal-yields-in-the-united-kingdom.csv")

## 1800년 이후 자료만 따로 저장
sub_crop <- subset(uk_crop, Year > 1799)

## 새로 저장된 자료 살펴보기
head(sub_crop)


## ----ukcrop, fig.cap="영국 밀 생산: 헥타르당 생산량", echo=TRUE, fig.align="center", fig.pos = 'ht', fig.asp = 0.7, fig.fullwidth=TRUE----
## 1. x축과 y축 자료 지정
ggplot(sub_crop, aes(x = Year, y=Wheat..tonnes.per.hectare.)) +
  
  ## 2. 선 그리기
  geom_line(size=0.5, color="forestgreen") + 
  
  ## 3. 점 그리기
  geom_point(size=1.5, alpha=0.2, color="forestgreen") + 
  
  ## 4. 레이블 달기
  labs(caption = "자료출처: Our World In Data") +
  
  ## 5. 그래프 형식 지정
  theme_jhp() + 
  
  ## 6. 축 이름 달기
  xlab('연도') + ylab('헥타르당 톤')


## ----ukcrop2, fig.cap="영국 밀 생산: 1950년 분절점을 이용하여 두 개의 선형모형으로 근사", fig.pos = 'ht', echo=TRUE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
## 1950년 이전과 이후를 구분하는 더비변수 생성
sub_crop$period <- ifelse(sub_crop$Year < 1951, "Before-1950", "Post-1950")

## x축과 y축 자료 지정, 그룹 정보와 색 정보도 지정 
ggplot(sub_crop, aes(x = Year, y=Wheat..tonnes.per.hectare., 
                     group=period, color=period)) + 
  geom_line(size=0.5, color="forestgreen") + 
  geom_point(size=1.5, alpha=0.2, color="forestgreen") + 
  geom_smooth(method="lm", color="firebrick2") +
  labs(caption = "자료출처: Our World In Data") +
  theme_jhp() + xlab('연도') + ylab('헥타르당 톤')


## ----malthus, fig.cap="인구증가와 밀 생산의 역사적 변화. 인구증가는 초록색 선으로 표시되었고 밀 생산은 붉은 색 선으로 표시되었음. 인구증가가 식량생산을 추월할 것이라는 맬더스의 주장이 역사적 사료와 맞지 않음을 보여줌.", fig.pos = 'ht', echo=TRUE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
## 1. 인구자료와 밀생산 자료를 연도와 분절점 기준으로 결합
malthus <- sub %>% left_join(sub_crop, by = c("year" = "Year", "period" = "period"))

## 2. 변수를 표준화하는 함수를 작성
center <- function(x){
  out <- (x - mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE)
  return(out)
}

## 3. 인구와 밀생산 자료를 표준화
malthus$center.pop <- center(malthus$Population) 
malthus$center.wheat <- center(malthus$Wheat..tonnes.per.hectare.) 

## 4. ggplot을 이용한 시각화 
ggplot(malthus, aes(x = year, y=center.pop)) + 
  geom_line(size=2, alpha=0.6, color="firebrick2") + 
  geom_line(aes(x = year, y=center.wheat), size=0.5, color="forestgreen") + 
  geom_point(size=1, alpha=0.6, color="firebrick2") + 
  geom_point(aes(x = year, y=center.wheat), size=1.5, alpha=0.2, color="forestgreen") + 
  labs(caption = "자료출처: Gapminder와 Our World In Data") +
  theme_jhp() + xlab('연도') + ylab('표준화 지수')

