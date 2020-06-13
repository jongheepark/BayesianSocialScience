###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 7
## 박종희
## 2020/06/13
###################################
source("index.R")
## ------------------------------------------------------------------------------
set.seed(1999)
N = 100
x = y = seq(-3, 3, length=N)  
mu = c(0,0)

f = function(x, y, mu=c(0,0), sd.x=1, sd.y=1, rho=0) {
  mu.x = mu[1]
  mu.y = mu[2]
  A = (x-mu.x)^2/sd.x^2 + (y-mu.y)^2/sd.y^2
  B = 2*rho/(sd.x*sd.y)*(x-mu.x)*(y-mu.y)
  return((A-B)/(1-rho^2))
}

persp.plot = function(x, y, z, main="Bivariate Normal Density",
  theta=30, phi=25, r=50, d=.1, expand=0.5, ltheta=90, lphi=180,
  shade=0.5, ticktype="simple", nticks=5, col="lightgreen", zlab="", ...) {
  
  persp(x, y, z, main=main,
        theta=theta, phi=phi, r=r, d=d, expand=expand, ltheta=ltheta,
        lphi=lphi, shade=shade, ticktype=ticktype, nticks=nticks,
        col=col, zlab=zlab, ...)
}
fxy = function(x, y, mu, Sig, sd1, sd2, rho) {
  
  if(missing(mu)) mu=c(0,0)
  
  if(!missing(Sig)) {
    sd1 = sqrt(Sig[1,1])
    sd2 = sqrt(Sig[2,2])
    if(Sig[1,2] != Sig[2,1]) {
      print("Covariance matrix is not symmetric... Returning .")
      return(NULL)
    }
    rho = Sig[1,2]/(sd1*sd2)
  }
  else if(missing(rho) || missing(sd1) || missing(sd2)) {
    sd1 = sd2 = 1
    rho = 0
  }

  Q = (x-mu[1])^2/sd1^2 + (y-mu[2])^2/sd2^2 -
    2*rho*(x-mu[1])*(y-mu[2])/(sd1*sd2)
  
  1/(2*pi*sd1*sd2*sqrt(1-rho^2))*exp(-Q/(2*(1-rho^2)))
}

covariance = function(sd.x, sd.y, rho) {
  sig.xy = rho*sd.x*sd.y
  matrix(c(sd.x^2, sig.xy, sig.xy, sd.y^2), nrow=2)
}
z =  outer(x, y, fxy, mu, covariance(1, 1, 0.5))


## ----like, fig.cap="3d 그래프로 본 이변량 정규분포의 로그우도: 두 변수의 상관계수 0.5이며 이변량 정규분포를 따름", fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
persp.plot(x, y, z, main="", col="grey80", xlab=expression(x1), 
           ylab=expression(x2), zlab="log lieklihood",
           border=NA, cex.lab=1)
mtext(expression(list(~mu==0, ~sigma==1, ~rho==0.5)))


## ----like2, fig.cap="등고선 히트맵으로 본 이변량 정규분포의 로그우도: 두 변수의 상관계수 0.5이며 이변량 정규분포를 따름", fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
image(x, y, z)


## ---- echo=TRUE, message=FALSE, results='asis'---------------------------------
set.seed(1999)
X <- cbind(1, runif(100))
theta.true <- c(1,-1, 1)
y <- X%*%theta.true[1:2] + rnorm(100)
mle <- glm(y ~ X-1, family = gaussian)
jhp_report(mle, title="glm을 이용한 최대우도추정", 
           label="tab:mley",
           dep.var.labels = "y (가상자료)")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
poisson.loglike <- function(lambda, y){ 
  n <- length(y)   
  loglik <- sum(y)*log(lambda)-n*lambda 
  return(loglik)
}


## ----mle1, fig.cap="프와송분포의 최대우도추정", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
set.seed(1999)

## 자료생성 및 우도 계산
y <- rpois(10000, 3)
lambda.list <- seq(1, 6, length=100)
loglike.holder <- sapply(lambda.list, function(k){
  poisson.loglike(lambda = k, y=y)
  })

## 우도 시각화
plot(lambda.list, loglike.holder, pch=19, 
     col=addTrans("forestgreen", 80), 
     xlab=expression(~lambda), ylab="log likelihood") 
grid(col="lightgray")

## 최대우도추정
mle.est <- lambda.list[which.max(loglike.holder)]; print(mle.est)
abline(v=mle.est, col="firebrick2", lty=3)
abline(v=3, col="firebrick4", lty=1)
legend("topright", legend=c("true", "mle estimate"), 
       lty=c(1, 3), col=c("firebrick4", "firebrick2"), bty="n")


## ---- echo = TRUE--------------------------------------------------------------
normal.like <- function(theta, y){ 
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(y)  
  logl <- -.5*n*log(2*pi)-.5*n*log(sigma2)-(1/(2*sigma2))*sum((y-mu)^2) 
  return(logl)
}


## ----mle2, fig.cap="정규분포의 최대우도추정", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
## 자료생성 및 우도 계산
set.seed(1999)
y <- rnorm(10000)
mu.list <- seq(-3, 3, length=1000)
loglike.holder <- sapply(mu.list, function(k){
  normal.like(theta = c(k, 1), y=y)})

## 우도 시각화
plot(mu.list, loglike.holder, pch=19, 
     col=addTrans("forestgreen", 50), cex=0.5,
     xlab=expression(~mu), ylab="log likelihood") 
grid(col="lightgray")

## 최대우도추정
mle.est <- mu.list[which.max(loglike.holder)]; print(mle.est)
abline(v=mle.est, col="firebrick4", lty=3)
abline(v=0, col="firebrick2", lty=1)
legend("topright", legend=c("true", "mle estimate"), 
       lty=c(1, 3), col=c("firebrick2", "firebrick4"), bty="n")


## ---- echo = TRUE, message=FALSE-----------------------------------------------
lm.like <- function(theta, y, X){ 
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k] 
  sigma <- exp(theta[k + 1])
  loglike <- sum(log(dnorm(y, X %*% beta, sigma)))
  return(loglike)
}


## ---- echo = TRUE, message=FALSE-----------------------------------------------
set.seed(1999)
X <- cbind(1, runif(100))
theta.true <- c(1,-1, 1)
y <- X%*%theta.true[1:2] + rnorm(100)


## ---- echo = TRUE, message=FALSE-----------------------------------------------
mle <- optim(c(1,1,1), lm.like, method="BFGS", control = list(fnscale = -1), 
           hessian=TRUE, y=y, X=X)
mle


## ---- echo = TRUE, message=FALSE-----------------------------------------------
K <- ncol(X)
psi.mu <- mle$par[K + 1]
psi.sd <- sqrt(solve(-mle$hessian)[K + 1, K + 1])
sigma.mle <- exp(psi.mu + 0.5*psi.sd) 
sigma.mle


## ---- echo = TRUE, message=FALSE-----------------------------------------------
ols <- lm(y ~ X-1)
summary(ols)$sigma


## ---- echo = TRUE, message=FALSE-----------------------------------------------
probit.like <- function (beta) {
  ## 선형함수
  eta <- X %*% beta
  ## 확률
  p <- pnorm(eta)
  ## 로그우도
  return(sum((1 - y) * log(1 - p) + y * log(p)))
}


## ---- echo = TRUE, message=FALSE-----------------------------------------------
probit.gr <- function (beta) {
  ## 선형함수
  mu <- X %*% beta
  
  ## 확률
  p <- pnorm(mu)
  
  ## 체인규칙
  u <- dnorm(mu) * (y - p) / (p * (1 - p)) 
  
  return(crossprod(X, u))
}


## ---- echo = TRUE--------------------------------------------------------------
suppressMessages(library(MCMCpack))
data(birthwt)
formula <- low~age+as.factor(race)+smoke
ols <- lm(formula, data=birthwt, y=TRUE, x=TRUE)
y <- ols$y
X <- ols$x


## ----probit, fig.cap="프로빗모형의 최대우도추정", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
fit <- optim(ols$coef, probit.like, gr = probit.gr, control = list(fnscale = -1), 
             method = "BFGS", hessian =  TRUE)
fit.glm <- glm(formula, data=birthwt, 
               family = binomial(link = "probit"))
plot(fit$par, coef(fit.glm), xlab="optim 추정치", ylab="glm 추정치")
abline(a=0, b=1, col="red", lty=3)


## ---- echo = TRUE--------------------------------------------------------------
## 자료생성
set.seed(1999)
X <- cbind(1, runif(100))
theta.true <- c(1,-1, 1)
y <- X%*%theta.true[1:2] + rnorm(100)
K <- ncol(X)

## 최대우도추정
mle <- optim(c(1,1,1), lm.like, method="BFGS", 
             control = list(fnscale = -1), 
             hessian=TRUE, y=y, X=X)

## 추정치 추출
beta.mle <- mle$par[1:K]
beta.mle.var <- solve(-mle$hessian)[1:K, 1:K]
psi.mu <- mle$par[K+1]
psi.var <- sqrt(solve(-mle$hessian)[K + 1, K + 1])

sigma.mle <- exp(psi.mu + 0.5*sqrt(psi.var)) 
sigma.mle.var <- sigma.mle^2*exp(sqrt(psi.var) - 1)
coef.se <- c(sqrt(diag(beta.mle.var)), sqrt(sigma.mle.var))
coef.mle <- c(beta.mle, sigma.mle)   

## 왈드 검정값 계산
t.stat <- coef.mle/coef.se
pval <- 2*(1-pt(abs(t.stat), nrow(X)-ncol(X)))
results <- cbind(coef.mle, coef.se, t.stat, pval)
colnames(results) <- c("coef","se","t-stat","p-value")
rownames(results) <- c("Constant","slope","sigma2")
print(results,digits=3)

