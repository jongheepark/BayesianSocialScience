###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 10
## 박종희
## 2020/06/13
###################################
source("index.R")
## ------------------------------------------------------------------------------
library(readxl)        
world_pop = read_xlsx("data/pop1800_2100.xlsx", sheet = 2)
sub <- subset(world_pop, year < 2020)
sub$log.pop <- log(sub$Population)
sub$log.pop <- ts(sub$log.pop, start= sub$year[1])


## ----autopop, fig.cap="로그변환된 세계 인구의 변화, 1800 - 2019", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
library(forecast)
library(ggfortify)
require(ggthemes)
autoplot(sub$log.pop, size=2) +
  xlab("연도") +  ylab("세계인구 (로그)") +
  labs(caption = "원자료 출처: Gapminder") + 
  theme_jhp()


## ------------------------------------------------------------------------------
d.arima <- auto.arima(sub$log.pop)
summary(d.arima)


## ----autopop2, fig.cap="세계 인구 변화에 대한 예측적 분석. ARIMA(3,2,2)를 이용", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
d.forecast <- forecast(d.arima, level = c(95), h = 50)
autoplot(d.forecast, size = 2) +   
  xlab("연도") + ylab("세계인구 (로그)") + 
  labs(subtitle = "검은 실선은 로그변환된 인구이며 붉은 선은 선형회귀선, 청색 실선과 회색 영역은 예측결과",
     caption = "원자료 출처: Gapminder") + 
  theme_jhp()


## ----autopop4, fig.cap="1950-2019 자료와 ARIMA(1,2,2)를 이용한 예측적 분석", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
y2 <- subset(sub$log.pop, start = 151)
d.arima2 <- auto.arima(y2)
summary(d.arima2)
d.forecast2 <- forecast(d.arima2, level = c(95), h = 50)
autoplot(d.forecast2, size = 2) +   
  xlab("연도") + ylab("세계인구 (로그)") + 
  labs(subtitle = "검은 실선은 로그변환된 인구이며 붉은 선은 선형회귀선, 청색 실선과 회색 영역은 예측결과",
     caption = "원자료 출처: Gapminder") + 
  theme_jhp()


## ----diffpop, fig.cap="세계 인구(로그)의 2차 차분값", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
autoplot(diff(sub$log.pop, 2), ts.colour = 'brown', size=2, 
         cpt.colour = 'firebrick3', cpt.linetype = 'dashed') + 
  xlab("연도") + ylab("인구") + 
  labs(caption = "원자료 출처: Gapminder") + 
  theme_jhp()


## ----struc, fig.cap="세계 인구 2차 차분값의 전환점: strucchange 패키지를 이용한 빈도주의 분석", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
library(strucchange)
y <- diff(sub$log.pop, 2)
lagy <- stats::lag(y, k = -1)
df.pop <- cbind(y, lagy)
df.pop <- window(df.pop, start = 1803, end = 2019)
colnames(df.pop) <- c("y", "lagy")
pop.model <- y ~ lagy
fs <- Fstats(pop.model, data=df.pop)
plot(fs)


## ----baipop, fig.cap="세계 인구 2차 차분값의 전환점", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
set.seed(1) 
out <- breakpoints(diff(sub$log.pop, 2) ~ 1)
summary(out)
break.dates <- paste(summary(out)$breakdates[nrow(summary(out)$breakdates),],
                     collapse=" ")
autoplot(out, ts.colour = 'brown', size=2, 
         cpt.colour = 'firebrick3', cpt.linetype = 'dashed') + 
  xlab("연도") + ylab("인구") + 
  labs(subtitle=paste0("세계인구 (로그) 예측: 전환점 = ", break.dates), 
     caption = "원자료 출처: Gapminder") + 
  theme_jhp()


## ------------------------------------------------------------------------------
set.seed(1)
N = 200
K = 2
mu.state = c(1, -1)
P = cbind(c(0.9,0.1),c(0.1,0.9))
sd = 1

## 은닉 마르코프 상태를 생성
Z = c(1, rep(NA, N-1))
for(t in 2:N){
  Z[t] = sample(K, size=1, prob=P[Z[t-1],])
}
table(Z)

## 관측변수 생성
X = rnorm(N, mean=mu.state[Z], sd=sd)


## ----hmm, fig.cap="은닉마르코프 모형에 의해 생성된 자료: 2개의 은닉상태", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
df.hmm <- data.frame(x = 1:length(Z), 
                     state = Z, y = X, mu = mu.state[Z])
df.hmm$state <- as.factor(df.hmm$state)
ggplot(df.hmm, aes(x = x, y = y, color=state)) + 
  geom_point(size=5, alpha=0.5) +
  labs(color="은닉 상태변수") + 
  theme_jhp()


## ------------------------------------------------------------------------------
#Assumed prior distribution on s1
pi = c(0.5, 0.5) 

# Pr(y_t | s_t=k) 
emit = function(k,x){ 
  dnorm(x, mean=mu.state[k], sd=sd)
}
alpha = matrix(nrow = N, ncol=K)
# Initialize alpha[1,]
for(k in 1:K){ 
  alpha[1,k] = pi[k] * emit(k,X[1])
}
## Forward algorithm
for(t in 1:(N-1)){
  m = alpha[t,] %*% P
  for(k in 1:K){
    alpha[t+1,k] = m[k]*emit(k,X[t+1]) 
  }
}
head(alpha)


## ------------------------------------------------------------------------------
## Backwards algorithm
beta = matrix(nrow = N, ncol=K)
# Initialize beta
for(k in 1:K){
  beta[N,k] = 1
}
for(t in (N-1):1){
  for(k in 1:K){
    beta[t,k] = sum(beta[t+1,]*P[k,]*emit(1:K,X[t+1]))
  }
}
head(beta)


## ----hmmres, fig.cap="은닉마르코프 모형에 의해 생성된 자료: 2개의 은닉상태", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.6, fig.fullwidth=TRUE----
ab = alpha*beta
prob = ab/rowSums(ab)
head(prob)
df.hmm$prop1 <- prob[,1]
df.hmm$prop2 <- prob[,2]
df.hmm$post.state <- as.factor(ifelse(df.hmm$prop1>0.5, 1, 2))
table(df.hmm$post.state)
head(df.hmm)


## ----hmmpost, fig.cap="은닉마르코프 모형의 추정결과. 연한 점은 추정치이며 진한 점은 참값", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
ggplot(df.hmm, aes(x = x, y = y, color=post.state)) + 
  geom_point(size=5, alpha=0.5) +
  geom_point(aes(x = x, y = y, color=state), size=2, alpha=1) + 
  labs(color="은닉 상태변수") +  
  theme_jhp()


## ------------------------------------------------------------------------------
mean(df.hmm$state != df.hmm$post.state)



