###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 11
## 박종희
## 2020/06/13
###################################
source("index.R")
## ---- echo=TRUE, message=FALSE-------------------------------------------------
library(MCMCpack)
set.seed(1119)
n <- 200
x1 <- runif(n)

## 설명변수와 종속변수의 관계에서 전환점
true.beta1 <- c(1, -2)
true.beta2 <- c(-1,  2)
true.s <- rep(1:2, each=n/2)
mu1 <- cbind(1, x1[true.s==1])%*%true.beta1
mu2 <- cbind(1, x1[true.s==2])%*%true.beta2
y <- as.ts(c(rnorm(n/2, mu1, sd=1), rnorm(n/2, mu2, sd=1)))

cat("Regime 1 mean = ", mean(y[true.s==1]), "\n")
cat("Regime 2 mean = ", mean(y[true.s==2]), "\n")
cat("Global mean   = ", mean(y), "\n")


## ----change.model, fig.cap="종속변수만 보는 전환점 분석의 문제", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05))
plot(y, main="Time series of y with one break", cex.main = 0.5)
lines(ts(c(rep(mean(y[true.s==1]), n/2), rep(mean(y[true.s==2]), n/2))), 
      lwd=2, col="brown")
library(strucchange)
lagy <- stats::lag(y, k = -1)
df.y <- cbind(y, lagy)
df.y <- window(df.y, start = 1, end = length(lagy))
colnames(df.y) <- c("y", "lagy")
y.model <- y ~ lagy
fs <- Fstats(y.model, data=df.y)
plot(fs, main="Break test using F-stat and AR(1) model", cex.main = 0.5)


## ------------------------------------------------------------------------------
b0 <- 0
B0 <- 0.1 ## 분산의 역수인 정확도
sigma.mu=var(y)
sigma.var=sigma.mu^2
formula=y ~ x1
sim0 <-  MCMCregressChange(formula, m=0, b0=b0, B0=B0, mcmc=1000, burnin=1000, 
           sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood="Chib95")
sim1 <-  MCMCregressChange(formula, m=1, b0=b0, B0=B0, mcmc=1000, burnin=1000,
           sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood="Chib95")
sim2 <-  MCMCregressChange(formula, m=2, b0=b0, B0=B0, mcmc=1000, burnin=1000,
           sigma.mu=sigma.mu, sigma.var=sigma.var, marginal.likelihood="Chib95")
BayesFactor(sim0, sim1, sim2)[3]


## ----change.model.state, fig.cap="종속변수만 보는 전환점 분석의 문제", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05))
plotState(sim1, main="전환점 1개")
plotState(sim2, main="전환점 2개")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
require(MCMCpack)
set.seed(1973)
x1 <- rnorm(300, 0, 1)
true.beta <- c(-.5, .2, 1)
true.alpha <- c(.1, -1., .2)
X <- cbind(1, x1)

## 두 개의 전환점 생성: 100 and 200
true.phi1 <- pnorm(true.alpha[1] + x1[1:100]*true.beta[1])
true.phi2 <- pnorm(true.alpha[2] + x1[101:200]*true.beta[2])
true.phi3 <-  pnorm(true.alpha[3] + x1[201:300]*true.beta[3])

## 종속변수 생성
y1 <- rbinom(100, 1, true.phi1)
y2 <- rbinom(100, 1, true.phi2)
y3 <- rbinom(100, 1, true.phi3)
Y <- as.ts(c(y1, y2, y3))


## ---- echo=TRUE, message=FALSE-------------------------------------------------
## 서로 다른 전환점 수를 가진 여러 개의 모형을 추정
out0 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=0,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))
out1 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=1,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))
out2 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=2,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))
out3 <- MCMCprobitChange(formula=Y~X-1, data=parent.frame(), m=3,
                         mcmc=1000, burnin=1000, b0 = 0, B0 = 0.1,   
                         marginal.likelihood = c("Chib95"))

## 모형 설명력 비교
BayesFactor(out0, out1, out2, out3)[3]


## ----probitchangestate, fig.cap="프로빗 전환점 분석의 은닉 상태변수 확률분포", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.33, fig.fullwidth=TRUE----
par(mfrow=c(1,3), mai=c(0.4, 0.6, 0.3, 0.05))
plotState(out1, main = "전환점 1개")
plotState(out2, main = "전환점 2개")
plotState(out3, main = "전환점 3개")


## ----probit.change.prop, fig.cap="프로빗 전환점 분석의 전환확률", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
plotChangepoint(out2, verbose = TRUE, ylab="Density")


## ---- warning=FALSE, echo=TRUE, message=FALSE----------------------------------
set.seed(1909)
N <- 200
x1 <- rnorm(N, 1, .5);

## 전환점 1개를 100에 설정하고 
## 레짐별 모수(1, 1), (1, -0.2)로 프로빗 은닉 변수 z 생성
z1 <- 1 + x1[1:100] + rnorm(100);
z2 <- 1 -0.2*x1[101:200] + rnorm(100);
z <- c(z1,  z2);
y <- z

## 이분종속변수 생성
y[z < 1] <- 1;
y[z >= 1 & z < 2] <- 2;
y[z >= 2] <- 3;


## ---- warning=FALSE, echo=TRUE, message=FALSE----------------------------------
formula <- y ~ x1
out1 <- MCMCoprobitChange(formula, m=1,
      	mcmc=100, burnin=100, thin=1, tune=c(.5, .5), 
     	b0=0, B0=0.1, marginal.likelihood = "Chib95")
out2 <- MCMCoprobitChange(formula, m=2,
      	mcmc=100, burnin=100, thin=1, tune=c(.5, .5, .5), 
     	b0=0, B0=0.1, marginal.likelihood = "Chib95")


## ---- warning=FALSE, echo=TRUE, message=FALSE----------------------------------
BayesFactor(out1, out2)[3]


## ----oprobit.change.state, fig.cap="서수형 프로빗 선형회귀분석 전환점 분석", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05))
plotState(out1, main = "전환점 1개", legend.control = c(1, 0.6))
plotState(out2, main = "전환점 2개", legend.control = c(1, 0.6))


## ----oprobit.change.prob, fig.cap="서수형 프로빗 선형회귀분석 전환확률", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05))
plotChangepoint(out1, verbose = TRUE, ylab="확률밀도")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
set.seed(1129)
n <- 150
x1 <- runif(n, 0, 0.5)

## 레짐별 모수 설정
true.beta1 <- c(1,  1)
true.beta2 <- c(1,  -2)
true.beta3 <- c(1,  2)

## 전환점 2개를 (50, 100)에 설정
true.s <- rep(1:3, each=n/3)
mu1 <- exp(1 + x1[true.s==1]*1)
mu2 <- exp(1 + x1[true.s==2]*-2)
mu3 <- exp(1 + x1[true.s==3]*2)

y <- as.ts(c(rpois(n/3, mu1), rpois(n/3, mu2), rpois(n/3, mu3)))
formula = y ~ x1

## 3개의 전환점 모형을 추정
model0 <-  MCMCpoissonChange(formula, m=0,
    mcmc = 1000, burnin = 1000, 
    b0 = rep(0, 2), B0 = 1/5*diag(2), marginal.likelihood = "Chib95")
model1 <-  MCMCpoissonChange(formula, m=1,
    mcmc = 1000, burnin = 1000, 
    b0 = rep(0, 2), B0 = 1/5*diag(2), marginal.likelihood = "Chib95")
model2 <-  MCMCpoissonChange(formula, m=2,
    mcmc = 1000, burnin = 1000, 
    b0 = rep(0, 2), B0 = 1/5*diag(2), marginal.likelihood = "Chib95")

## 베이지안 모형비교
BayesFactor(model0, model1, model2)[3]


## ----poisson.change.state, fig.cap="포와송 선형회귀분석 전환점 분석", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05))
plotState(model1, main = "전환점 1개")
plotState(model2, main = "전환점 2개")


## ----poisson.change.prob, fig.cap="포와송 선형회귀분석 전환 확률분포", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
plotChangepoint(model2, verbose = TRUE, ylab="확률밀도")


## ----panelFE1, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE-------
library(MCMCpack)
set.seed(1974)
n.group <- 30
n.time <- 80
NT <- n.group*n.time

## 자료생성을 위한 모수 및 변수 설정
true.beta <- c(1, 1)
true.sigma <- 3
x1 <- rnorm(NT)
x2 <- runif(NT, 2, 4)
X <- as.matrix(cbind(x1, x2), NT, 2)
y <- rep(NA, NT)
id <- rep(1:n.group, each=NT/n.group)
K <-ncol(X)
true.beta <- as.matrix(true.beta, K, 1)

## 그룹별 전환점 설정: 각 1개, 모형 중간지점
break.point = rep(n.time/2, n.group)
break.sigma=c(rep(1, n.group))
break.list <- rep(1, n.group)

## 전환확률 계산
ruler <- c(1:n.time)
W.mat <- matrix(NA, n.time, n.group)
for (i in 1:n.group){
  W.mat[, i] <- pnorm((ruler-break.point[i])/break.sigma[i])
}
Weight <- as.vector(W.mat)

## time-varying individual effects를 추출해서 종속변수를 생성
j = 1
true.sigma.alpha <- 30
true.alpha1 <- true.alpha2 <- rep(NA, n.group)

for (i in 1:n.group){
  Xi <- X[j:(j+n.time-1), ]
  true.mean <- Xi%*% true.beta
  weight <- Weight[j:(j+n.time-1)]
  true.alpha1[i] <- rnorm(1, 0, true.sigma.alpha)
  true.alpha2[i] <- -1*true.alpha1[i]
  y[j:(j+n.time-1)] <- ((1-weight)*true.mean + (1-weight)*rnorm(n.time, 0, true.sigma) +
		(1-weight)*true.alpha1[i]) +
		(weight*true.mean + weight*rnorm(n.time, 0, true.sigma) + weight*true.alpha2[i])
  j <- j + n.time
}


## ----panelFE2, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE-------
FEols <- lm(y ~ X + as.factor(id) -1 )
summary(FEols)$coef[1:2,]


## ---- eval=FALSE, echo=TRUE----------------------------------------------------
## b0 <- 0
## B0 <- 0.1 ## 분산의 역수인 정확도
## sigma.mu=var(y)
## sigma.var=sigma.mu^2
## c0 <- 4 + 2 * (sigma.mu^2/sigma.var)
## c0
## d0 <- 2 * sigma.mu * (c0/2 - 1)
## d0


## ----panelFE22, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE------
## OLS 고정효과 모형의 표준화 잔차 추출
resid.all <- rstandard(FEols)
time.id <- rep(1:80, n.group)

## 그룹별 individual effects에서의 전환점 추정
G <- 100
BF <- testpanelSubjectBreak(subject.id=id, time.id=time.id,
                            resid= resid.all, max.break=3, minimum = 10,
                            mcmc=G, burnin = G, thin=1, verbose=0,
                            b0=b0, B0=B0, c0=c0, d0=d0, Time = time.id)

## 전환점 추정 결과에서 전환점 수 추출
estimated.breaks <- make.breaklist(BF, threshold=3)

## 전체 모형추정
out <- HMMpanelFE(subject.id = id, y=y, X=X, m =estimated.breaks,
                  mcmc=G, burnin=G, thin=1, verbose=0,
                  b0=0, B0=1/10, c0=2, d0=2, delta0=0, Delta0=1/1000)


## ----panelFE3, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE-------
cat("\n고정효과 패널 전환점 모형\n")
print(summary(out)[1], digits=2)

cat("\n고정효과 모형\n")
print(summary(FEols)$coef[1:2,], digits=2)


## ----panelRE1, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE-------
set.seed(1977)
Q <- 3
true.beta1 <-c(1, 1, 1) ; true.beta2 <-c(-1, -1, -1)
true.sigma2 <-c(2, 5); 
true.D1 <- diag(.5, Q); true.D2 <- diag(2.5, Q)
n.group = 30; 
n.time = 100;
NT <- n.group*n.time
x1 <- runif(NT, 1, 2)
x2 <- runif(NT, 1, 2)
X <- cbind(1, x1, x2)
W <- X
y <- rep(NA, NT)

## 자료 중간지점에 1개의 전환점 생성
break.point = rep(n.time/2, n.group)
break.sigma=c(rep(1, n.group))
break.list <- rep(1, n.group)
id <- rep(1:n.group, each=NT/n.group)
K <- ncol(X);
ruler <- c(1:n.time)

## 가중치 변수를 만들어 전환확률 계산
W.mat <- matrix(NA, n.time, n.group)
for (i in 1:n.group){
  W.mat[, i] <- pnorm((ruler-break.point[i])/break.sigma[i])
}
Weight <- as.vector(W.mat)

## 은닉상태별 평균과 분산을 가중치와 곱해서 패널 자료 생성
j = 1
for (i in 1:n.group){
  Xi <- X[j:(j + n.time -1), ]
  Wi <- W[j:(j + n.time -1), ]
  true.V1 <- true.sigma2[1]*diag(n.time) + Wi%*%true.D1%*%t(Wi)
  true.V2 <- true.sigma2[2]*diag(n.time) + Wi%*%true.D2%*%t(Wi)
  true.mean1 <- Xi%*%true.beta1
  true.mean2 <- Xi%*%true.beta2
  weight <- Weight[j:(j+n.time-1)]
  y[j:(j+n.time-1)] <- (1-weight)*true.mean1 + 
    (1-weight)*chol(true.V1)%*%rnorm(n.time) + 
    weight*true.mean2 + weight*chol(true.V2)%*%rnorm(n.time)
  j <- j + n.time
}


## ---- eval=TRUE, echo=TRUE-----------------------------------------------------
b0 <- 0
B0 <- 0.1 ## 분산의 역수인 정확도
sigma.mu=var(y)
sigma.var=sigma.mu^2
c0 <- 4 + 2 * (sigma.mu^2/sigma.var)
c0
d0 <- 2 * sigma.mu * (c0/2 - 1)
d0
## 역위셔트분포의 자유도
r0 <- 5 
## 역위셔트분포의 스케일 행렬
R0 <- diag(c(1, 0.1, 0.1))


## ----panelRE2, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE-------
## 패널 그룹 아이디
subject.id <- c(rep(1:n.group, each=n.time))
## 패널 시계열 아이디
time.id <- c(rep(1:n.time, n.group))

## 예제이므로 100번만 추출
G <- 100
subject.id <- c(rep(1:n.group, each=n.time))
time.id <- c(rep(1:n.time, n.group))
out1 <- HMMpanelRE(subject.id, time.id, y, X, W, m=1,
                   mcmc=G, burnin=G, thin=1, verbose=0,
                   b0=b0, B0=B0, c0=c0, d0=d0, r0=r0, R0=R0)


## ----panelRE3, fig.cap="임의효과 패널 전환점 분석 결과", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.47, fig.fullwidth=TRUE----
par(mfrow=c(1,2), mai=c(0.4, 0.6, 0.3, 0.05), tck=.02)
plotState(out1, main="은닉 상태변수의 확률분포")
plotChangepoint(out1, verbose=TRUE, overlay=TRUE, 
                main="전환점의 확률분포")


## ----panelRE4, echo=TRUE, message=FALSE,warning=FALSE, fig.showtext=TRUE-------
print(summary(out1)[[1]][1:8, ], digits=2)


## ----panel.re.trace, fig.cap="임의효과 패널 전환점 모형의 사후 확률분포", echo=TRUE, message=FALSE, warning=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
library("bayesplot")
library("rstan")
posterior <- rstan:::as.data.frame.stanfit(data.frame(out1[, grep("beta", colnames(out1))]))
color_scheme_set("red")
mcmc_areas(posterior, prob = 0.95, point_est = "mean")

