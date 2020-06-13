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


## ---- echo=TRUE, message=FALSE-------------------------------------------------
library(MCMCpack)
set.seed(1119)
n <- 200
x1 <- runif(n)
true.beta1 <- c(0, -2)
true.beta2 <- c(0,  2)
true.Sigma <- c(1, 1)
true.s <- rep(1:2, each=n/2)

mu1 <- cbind(1, x1[true.s==1])%*%true.beta1
mu2 <- cbind(1, x1[true.s==2])%*%true.beta2

y <- as.ts(c(rnorm(n/2, mu1, sd=sqrt(true.Sigma[1])), 
             rnorm(n/2, mu2, sd=sqrt(true.Sigma[2]))))
formula=y ~ x1


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
## b0 <- 0
## B0 <- 0.1 ## B0 is a precision (i.e. the inverse of variance)
## sigma.mu=var(y)
## sigma.var=sigma.mu^2
## c0 <- 4 + 2 * (sigma.mu^2/sigma.var)
## c0
## d0 <- 2 * sigma.mu * (c0/2 - 1)
## d0


## ----changebeta, fig.cap="베타분포(1, 1)", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.6----
sigma.mu=sd(y)
sigma.var=var(y)
curve(dbeta(x, 1, 1), lwd = 5, xlim=c(0, 1), ylim=c(0,3), 
      ylab="f(y)", xlab="y", col='firebrick4')


## ---- echo=TRUE, message=FALSE-------------------------------------------------
library(tictoc)
tic("MCMCregressChange model check")
sim0 <-  MCMCregressChange(formula, m=0, b0=b0, B0=B0, mcmc=1000, burnin=1000, 
           sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood="Chib95")
sim1 <-  MCMCregressChange(formula, m=1, b0=b0, B0=B0, mcmc=1000, burnin=1000,
           sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood="Chib95")
sim2 <-  MCMCregressChange(formula, m=2, b0=b0, B0=B0, mcmc=1000, burnin=1000,
           sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood="Chib95")
toc()


## ------------------------------------------------------------------------------
BayesFactor(sim0, sim1, sim2)[3]


## ----lm.change.state, fig.cap="선형회귀모형 은닉 상태변수 사후확률분포: 왼쪽 패널은 전환점이 1개인 모형, 오른쪽 패널은 전환점이 2개인 모형", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05), cex.main=0.5)
plotState(sim1, main="전환점 1개")
plotState(sim2, main="전환점 2개")


## ----lm.change.prob, fig.cap="선형회귀모형 전환점분석의 전환확률", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02, cex.main=0.5)
plotChangepoint(sim1, verbose=TRUE)


## ---- echo=TRUE----------------------------------------------------------------
print(summary(sim1), digits=2)


## ------------------------------------------------------------------------------
complete.pool.mcmc <- MCMCregress(formula, b0=b0, B0=B0, 
                                  mcmc=1000, burnin=1000,
                                  sigma.mu=sigma.mu,
                                  sigma.var=sigma.var)
data = data.frame(y, x1)
no.pool.mcmc1 <- MCMCregress(formula, 
                             data=data[rep(c(T, F), each = n/2), ], 
                             b0=b0, B0=B0, mcmc=1000, burnin=1000,
                             sigma.mu=sigma.mu, sigma.var=sigma.var)
no.pool.mcmc2 <- MCMCregress(formula, 
                             data=data[rep(c(F, T), each = n/2), ], 
                             b0=b0, B0=B0, mcmc=1000, burnin=1000,
                             sigma.mu=sigma.mu, sigma.var=sigma.var)


## ------------------------------------------------------------------------------
denstiy.compare <- function(no.pool, complete.pool, partial.pool, 
                            true.beta = 0,
                            title="", subtitle="", 
                            caption=""){
  df.dens <- data.frame(no.pool, complete.pool, partial.pool)
  colnames(df.dens) <- c("no pooling", "complete pooling", "partial pooling")
  df.dens.long <- tidyr::gather(df.dens, type, value) 
  g.dens <- ggplot(df.dens.long, aes(value, fill = type, colour = type)) +
    geom_density(alpha = 0.1)  + 
    geom_vline(xintercept = true.beta, col="red", linetype = "longdash") +
    theme_jhp() + xlab("value") + ylab("density") +
    labs(title = title, subtitle = subtitle, 
         caption = caption, colour=NULL, fill=NULL) 
  return(g.dens)
}


## ----changedens, fig.cap="깁스 추출 알고리듬의 궤적: 선형회귀분석 전환점모형", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
## beta 1 compare
beta1pre.complete <- complete.pool.mcmc[, 1]
beta1post.complete <- complete.pool.mcmc[, 1]
beta1pre.no.pool <- no.pool.mcmc1[, 1]
beta1post.no.pool <- no.pool.mcmc2[, 1]
beta1pre.partial <- sim1[, 1]
beta1post.partial <- sim1[, 3]

## beta 2 compare
beta2pre.complete <- complete.pool.mcmc[, 2]
beta2post.complete <- complete.pool.mcmc[, 2]
beta2pre.no.pool <- no.pool.mcmc1[, 2]
beta2post.no.pool <- no.pool.mcmc2[, 2]
beta2pre.partial <- sim1[, 2]
beta2post.partial <- sim1[, 4]

## draw plots
complete.pool = beta2pre.complete
no.pool = beta2pre.no.pool
partial.pool = beta2pre.partial
g1 <- denstiy.compare(no.pool, complete.pool, partial.pool, 
                true.beta = true.beta1[2], 
                subtitle="레짐 1의 기울기 추정치 비교: 세로선이 참값")

complete.pool = beta2post.complete
no.pool = beta2post.no.pool
partial.pool = beta2post.partial
g2 <- denstiy.compare(no.pool, complete.pool, partial.pool, 
                true.beta = true.beta2[2], 
                subtitle="레짐 2의 기울기 추정치 비교: 세로선이 참값")

ggsave("figures/partialpoolingregime1.png", 
       plot=g1, width=12, height=6, family="sans")   
ggsave("figures/partialpoolingregime2.png", 
       plot=g2, width=12, height=6, family="sans")   



## ------------------------------------------------------------------------------
RMSE <- function(sample, true){sqrt(mean((sample-true)^2))}
no.pool = beta2pre.no.pool
partial.pool = beta2pre.partial
cat("Regime 1 no pooling RMSE: ", RMSE(no.pool, true=true.beta1[2]), "\n")
cat("Regime 1 partial pooling RMSE: ", RMSE(partial.pool, true=true.beta1[2]), "\n")

no.pool = beta2post.no.pool
partial.pool = beta2post.partial
cat("Regime 2 no pooling RMSE: ", RMSE(no.pool, true=true.beta2[2]), "\n")
cat("Regime 2 partial pooling RMSE: ", RMSE(partial.pool, true=true.beta2[2]), "\n")


## ----regposttrace1, fig.cap="1개의 전환점을 갖는 선형회귀분석 모형의 추적 그래프", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05), cex.main=0.8)
plot(sim1, density=FALSE)

