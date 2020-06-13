###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 9
## 박종희
## 2020/06/13
###################################
source("index.R")
## ----mcchain, echo = TRUE, message=FALSE, warning=FALSE, eval=TRUE-------------
MarkovChainSampler <- function(mcmc=5000, burnin=1000){ 
    iter <- mcmc + burnin
    storage <- rep(NA, iter)
    # The initial state is set as State 1.
    storage[	1] <- 1
    for (g in 2:iter){
      u <- runif(1)
      if(storage[g-1] == 1){
      ## state t-1 = 1
        storage[g] <- ifelse(u<0.65, 1, 2)
      }else{
      ## state t-1 = 2
        storage[g] <- ifelse(u<0.25, 1, 2)
      }
      if(g>burnin & g%%1000 == 0){
        cat("iteration at ", g, 
            table(storage[(burnin+1):g])/(g - (burnin+1)), "\n")
      }
    }
    return(storage[(burnin+1):iter])
}


## ----mcchain2, echo = TRUE, message=FALSE, warning=FALSE, eval=TRUE------------
set.seed(1973)
out <- MarkovChainSampler()
table(out)/5000


## ----betaMH, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE----------------
BetaMH <- function(f.target,  # 목표분포의 밀도
                   f.prop,  # 제안분포의 밀도
                   r.prop, # 제안분포의 표본
                   x0, # 체인 시작값
                   mcmc = 1000, # MCMC 횟수
                   burnin=1000) { # 처음 1000번은 버리기
      
    iter <- mcmc + burnin
    mcmc.store <- rep(NA, mcmc)
    accepts <- 0
    x <- c(x0, rep(NA, iter-1))  
    
    for (g in 2:iter){
        candidate <- r.prop(x[g-1]) 
        numerator <- f.target(candidate)*f.prop(x[g-1],candidate)
        denominator <- f.target(x[g-1])*f.prop(candidate,x[g-1])
        alpha <- min(1, numerator/denominator)
        
        ## 수용확률 계산
        if(runif(1) < alpha){
            x[g] <- candidate
            accepts <- accepts + 1
        }else{
            x[g] <- x[g-1]
        }  
        if (g > burnin){ 
            mcmc.store[g-burnin] <- x[g]
        }
        if(g %% 500 == 0)
            cat("acceptance rate = ", accepts/g, "\n")   
    }
    return(mcmc.store)
}


## ---- echo=TRUE----------------------------------------------------------------
set.seed(1973)
a<-3; b<-4; 
f.target  <- function(x)   dbeta(x,a,b)
r.prop <- function(x)   runif(1,0,1)
f.prop  <- function(x,y) 1 
x0=runif(1,0,1)
beta.mh.post <- BetaMH(f.target, f.prop, r.prop, x0=runif(1,0,1))


## ----mh, fig.cap="균등분포를 이용한 베타분포에 대한 MH 샘플링", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
mcmc = 1000
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
par(mfrow=c(1,2), mar=c(2,2,1,1))

hist(beta.mh.post, breaks=50, col="blue", cex.main=0.5, 
     main="균등분포를 이용한 MH 추출", freq=FALSE)
curve(dbeta(x,a,b), col="sienna", lwd=2, add=TRUE)

hist(rbeta(mcmc,a,b), breaks=50, col="grey", cex.main=0.5, 
     main="IID 추출",freq=FALSE)
curve(dbeta(x,a,b), col="sienna", lwd=2, add=TRUE)


## ----mhtrace, fig.cap="MH 샘플링 결과에 대한 추적 그래프", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.3, fig.fullwidth=TRUE----
library("bayesplot")
library("rstan")
posterior <- rstan:::as.data.frame.stanfit(data.frame("beta" = beta.mh.post))
color_scheme_set("red")
mcmc_trace(posterior)


## ----mhtrace2, fig.cap="동일독립분포의 추적 그래프", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.3, fig.fullwidth=TRUE----
iid <- rstan:::as.data.frame.stanfit(data.frame("beta" = rbeta(mcmc,a,b)))
mcmc_trace(iid)


## ----mhdens, fig.cap="MH 샘플링 결과에 대한 최고사후밀도", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.3, fig.fullwidth=TRUE----
mcmc_areas(posterior, prob = 0.95, point_est = "mean")


## ----mhhpd, fig.cap="MH 샘플링 결과에 대한 확률구간", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.3, fig.fullwidth=TRUE----
mcmc_intervals_data(posterior)
mcmc_intervals(posterior)


## ----betaIndMH, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE-------------
 BetaIndMH <- function(mcmc=1000, burnin=1000, a=3, b=4){ 
    iter <- mcmc + burnin
    mcmc.store <- rep(NA, mcmc)
    accepts <- 0
    x <- runif(1)
    
    for (g in 1:iter){
      u2 <- runif(1)
      f2 <- dbeta(u2, a, b)
      f1 <- dbeta(x, a, b)
      alpha <- min(f2/f1, 1)
    
      ## 수용확률 계산
      if (runif(1) < alpha){
    	  x <- u2
        if (g > burnin){ 
          accepts <- accepts + 1
        }
      }  
      if (g > burnin){ 
        mcmc.store[g-burnin] <- x
      }
    }
    cat("acceptance rate = ", accepts/mcmc, "\n")
    return(mcmc.store)
 }


## ----mh2, fig.cap="독립커널을 이용한 메트로폴리스 해이스팅스 방법", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.5, fig.fullwidth=TRUE----
set.seed(1973)
a<-3; b<-4; 
mcmc = 1000
burnin=1000
beta.mh.ind <- BetaIndMH(mcmc, burnin)

par(mfrow=c(1,2),mar=c(2,2,1,1))
hist(beta.mh.ind, breaks=50, col="blue", cex.main=0.5, 
     main="독립커널 MH 추출",freq=FALSE)
curve(dbeta(x,a,b),col="sienna",lwd=2,add=TRUE)

hist(rbeta(mcmc,a,b),breaks=50,col="grey",cex.main=0.5, 
     main="IID 추출",freq=FALSE)
curve(dbeta(x,a,b),col="sienna",lwd=2,add=TRUE)


## ----mhindtrace, fig.cap="MH 샘플링 결과에 대한 추적 그래프", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.3, fig.fullwidth=TRUE----
posterior <- rstan:::as.data.frame.stanfit(data.frame("beta" = beta.mh.ind))
color_scheme_set("red")
mcmc_trace(posterior)
mcmc_areas(posterior, prob = 0.95, point_est = "mean")
mcmc_intervals(posterior)


## ----poissonlogpost, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE--------
poisson.log.post<-function(beta,...){
    n <- nrow(X)
    k <- ncol(X)
    eta <- X%*%matrix(beta, k,1)
    mu  <- exp(eta)
    log.like <- sum(dpois(y, mu, log=TRUE))            
    log.prior <- dmvnorm(beta, b0, B0, log=TRUE)    
    return(log.like + log.prior)
}


## ----poissonMH, echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE-------------
poissonMH <- function(y, X,
                      mcmc=1000, burnin=1000, verbose=0,
                      beta.hat, V.hat, tune=1){
  n  <- length(y)
  k  <- ncol(X)  
  mcmc.store <- matrix(NA, mcmc, k)
  tot.iter <- mcmc + burnin
  accepts <- 0
  beta <- beta.hat
  
  ## Metropolis-Hastings 샘플링
  for (g in 1:tot.iter){ 
     
    ## candidate 추출
    beta.can <- beta + rmvnorm(1, matrix(0, k, 1), tune*V.hat)
    
    ## 수용확률 계산
    log.ratio <- poisson.log.post(beta.can) - poisson.log.post(beta)   
    alpha <- min(exp(log.ratio), 1)
    
    ## 수용여부 결정
    if (runif(1) < alpha){
      beta   <- beta.can
      if (g > burnin) accepts <- accepts + 1
    }
    
    ## 저장 
    if (g > burnin){
      mcmc.store[g-burnin,] <- beta
    } 
    ## echo some results   
    if (verbose!=0&g%%verbose == 0){
      cat("iteration ", g, "beta ", beta, "\n") 
    }
  }
   cat("acceptance rate = ", accepts/mcmc, "\n")
  return(mcmc.store)
}


## ---- echo=TRUE, message=FALSE, warning=FALSE, eval=TRUE-----------------------
require(mvtnorm)
set.seed(1973)
## 가상의 자료 생성
X <- cbind(1, rnorm(100), runif(100))
true.beta   <- c(1, -1, 2)
y <- rpois(100, exp(X%*%true.beta))
mle <-  glm(y~X-1, family=poisson())

## MCMC 입력 준비물
V.hat <- vcov(mle)
beta.hat <- coef(mle)
b0 <- rep(0, 3)
B0 <- diag(1000, 3)

## MH로 모형 추정
poisson.rw.post <- poissonMH(y, X, 
                      mcmc=5000, burnin=1000, verbose=1000,
                      beta.hat=beta.hat, V.hat=V.hat, tune=1.5)
  


## ----mhpoisson, fig.cap="임의보행 MH 샘플러 성능", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
## display output
require(coda);
out <- as.mcmc(poisson.rw.post)
plot(out);


## ---- echo = TRUE--------------------------------------------------------------
summary(out)


## ----mhpoissongg, fig.cap="MCMC의 사후 확률분포 평균과 MLE 점추정치", echo=TRUE, message=FALSE, fig.align="center", fig.width = 4, fig.height = 4----
summary(mle)
df = data.frame(x = summary(mle)$coef[, "Estimate"], 
                y = summary(out)$stat[,"Mean"])
ggplot(df, aes(x, y)) + 
  geom_point(size=3, col="brown", alpha=0.3) + 
  geom_abline(intercept = 0, slope = 1, col="red",linetype="dashed") +
  xlab("MLE") + ylab("MCMC")


## ---- echo = TRUE--------------------------------------------------------------
binorm.gibbs <- function(mcmc=1000, rho){
  out <- matrix(NA, mcmc, 2)
  x2 <- 1
  for(i in 1:mcmc){
    x1 <- rnorm(1, rho*x2, 1-rho^2)
    x2 <- rnorm(1, rho*x1, 1-rho^2)
    out[i,] <- c(x1,x2)
  }
  return(out)
}
## 자료 생성
set.seed(1973)
mcmc <- 1000
rho0.2 <- binorm.gibbs(mcmc, 0.2)
rho0.5 <- binorm.gibbs(mcmc, 0.5)
rho0.8 <- binorm.gibbs(mcmc, 0.8)
rho0.95 <- binorm.gibbs(mcmc, 0.95)


## ----gibbsnorm1, fig.cap="깁스 추출 알고리듬의 궤적: 두 변수의 상관성은  0.2, 0.5, 0.8, 0.95.", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1.15, fig.fullwidth=TRUE----
library(RColorBrewer)
library(MASS)
col.brown = NetworkChange:::addTrans("brown", 80)
gibbs.density.plot <- function(input, xlab="x1", ylab="x2"){
    k <- 11
    my.cols <- rev(brewer.pal(k, "RdYlBu"))
    z <- kde2d(input[,1], input[,2], n=50)
    plot(input, xlab=xlab, ylab=ylab, type="o", 
         pch=19, col=col.brown, lwd=1)
    contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
    legend("topleft", paste("correlation=", round(cor(input)[1,2],2)), bty="n")
}
par(mfrow=c(2,2))
gibbs.density.plot(rho0.2)
gibbs.density.plot(rho0.5)
gibbs.density.plot(rho0.8)
gibbs.density.plot(rho0.95)


## ----gibbsnorm2, fig.cap="깁스 추출 알고리듬의 추적그래프", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
par(mfrow=c(2,2))
col.brown = NetworkChange:::addTrans("brown", 150)
plot(rho0.2[,1], type="l", col=col.brown)
plot(rho0.5[,1], type="l", col=col.brown)
plot(rho0.8[,1], type="l", col=col.brown)
plot(rho0.95[,1], type="l", col=col.brown)


## ----gibbsnorm3, fig.cap="깁스 추출 샘플의 자기상관성", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
par(mfrow=c(2,2))
acf(rho0.2[,1], lwd=5, col=col.brown)
acf(rho0.5[,1], lwd=5, col=col.brown)
acf(rho0.8[,1], lwd=5, col=col.brown)
acf(rho0.95[,1], lwd=5, col=col.brown)


## ---- echo = TRUE--------------------------------------------------------------
lm.gibbs <-function(y, X, mcmc=1000, burnin=1000, 
                   b0, B0, c0, d0){
  n  <- length(y)
  k  <- ncol(X)  
  mcmc.store <- matrix(NA, mcmc, k+1)
  tot.iter <- mcmc+burnin
  B0inv <- solve(B0)
  
  ## starting value
  sigma2 <-  1/runif(1)

  ## Sampler starts!
  for (g in 1:tot.iter){  
##########################
    ## Step 1: Sample beta  
##########################
    ## posterior beta variance
    post.beta.var <- 
      chol2inv(chol(B0inv + (t(X)%*%X)/sigma2))
    ## posterior beta mean
    post.beta.mean <- 
      post.beta.var%*%(B0inv%*%b0 + (t(X)%*%y)/sigma2) 
    ## draw new beta 
    beta <- post.beta.mean + chol(post.beta.var)%*%rnorm(k)  
##########################
    ## Step 2: Sample sigma2  
##########################
    ## new shape parameter
    c1 <- c0 + n/2
    ## error vector
    e <- y - X%*%beta
    ## new scale parameter
    d1 <- d0 + sum(e^2)/2
    ## draw new tau
    sigma2 <- 1/rgamma(1, c1, d1)   
    ## store Gibbs output after burnin
    if (g > burnin){
      mcmc.store[g-burnin,] <- c(beta, sigma2)
    }
  }
  return(mcmc.store)
}


## ---- echo = TRUE--------------------------------------------------------------
set.seed(1973)
X <- cbind(1, rnorm(100), rnorm(100))
true.beta   <- c(1, -1, 2); true.sigma2    <- 1
y <- X%*%true.beta + rnorm(100, 0, true.sigma2)


## ---- echo = TRUE--------------------------------------------------------------
## prior setting
b0  <- rep(0, 3) ; B0  <- diag(10, 3)
sigma.mu = var(y)[1]; sigma.var =sigma.mu^2
c0 <- 4 + 2 *(sigma.mu^2/sigma.var)
d0 <- 2*sigma.mu *(c0/2 - 1)


## ----echo=TRUE-----------------------------------------------------------------
library(tictoc)
tic("mycode")
gibbs.lm.post <- lm.gibbs(y=y, X=X, b0=b0, B0=B0, 
                          c0=c0, d0=d0, mcmc=10000)
toc()


## ------------------------------------------------------------------------------
require(MCMCpack)
library(tictoc)
tic("MCMCpack")
mp.gibbs <- MCMCregress(y~X-1, b0=b0, B0=diag(1/10, 3), c0=c0, d0=d0)
toc()


## ----gibbslm, fig.cap="선형회귀분석 모형에 대한 깁스 추출", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
require(coda); 
out <- as.mcmc(gibbs.lm.post)
summary(out)
plot(out)


## ------------------------------------------------------------------------------
mle <- glm(y~X-1, family=gaussian)
summary(mle)


## ----lmgibbsdens, fig.cap="깁스 추출 알고리듬의 궤적: 선형회귀분석 모형", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1.15, fig.fullwidth=TRUE----
par(mfrow=c(2,2))
col.brown = NetworkChange:::addTrans("brown", 80)
gibbs.density.plot(gibbs.lm.post[, 1:2], xlab=bquote(beta[1]), ylab=bquote(beta[2]))
gibbs.density.plot(gibbs.lm.post[, 2:3], xlab=bquote(beta[2]), ylab=bquote(beta[3]))
gibbs.density.plot(gibbs.lm.post[, 3:4], xlab=bquote(beta[3]), ylab=bquote(sigma^2))
gibbs.density.plot(gibbs.lm.post[, c(2,4)], xlab=bquote(beta[2]), ylab=bquote(sigma^2))


## ---- echo = TRUE, message=FALSE-----------------------------------------------
"ProbitGibbs"<-function(y, X, b0, B0, mcmc=5000, burnin=1000, verbose=0){
  
  N  <- length(y)
  k  <- ncol(X)  
  tot.iter <- mcmc+burnin
  B0inv <- solve(B0)
  XX <- t(X)%*%X
  
  ## 추출된 샘플 저장할 객체
  beta.store   <- matrix(NA, mcmc, k)
  Z <- matrix(rnorm(N), N, 1)
  
  ## MCMC 샘플링
  for (iter in 1:tot.iter){
    ##################################
    ## Step 1: beta|Z ~ N(b.hat, B.hat)
    ##################################
      XZ <- t(X)%*%Z
      post.beta.var  <-  solve(B0inv + XX)
      post.beta.mean <-  post.beta.var%*%(B0inv%*%b0 + XZ)
      beta  <- post.beta.mean + chol(post.beta.var)%*%rnorm(k)  
      
    ###################################
    ## Step 2: Z|beta ~ TN(X%*%beta, 1)      
    ################################### 
    mu      <-  X%*%beta 
    prob    <-  pnorm(-mu)
    for(j in 1:N){
        uj  <-  runif(1)
        z  <- ifelse(y[j]==0, mu[j] + qnorm(uj*prob[j]),
                     mu[j] + qnorm(prob[j] + uj*(1-prob[j])))
      
      ## infinity가 샘플되면 극단적 수를 대입
      if (z==-Inf){
        Z[j, 1] <- -300
      }else if (z==Inf){
        Z[j, 1] <- 300
      }else {
        Z[j, 1] <- z
      }
    }
    ## 저장
    if (iter > burnin){
      beta.store[iter-burnin,]   <-  beta
    }
    ## 리포트
    if (verbose>0&iter%%verbose == 0){
      cat("---------------------------------------",'\n')    
      cat("iteration = ", iter, '\n')
      cat("beta = ", beta, '\n')
    }  
  }
  return(beta.store)
}


## ---- echo = TRUE, message=FALSE-----------------------------------------------
## 자료 생성
set.seed(1973)
N   <-  100
x1  <-  rnorm(N)
X   <-  cbind(1, x1)
k   <-  ncol(X)
true.beta   <-  c(0, .5)

## 종속변수 생성 
Z <- X%*%true.beta + rnorm(N)
y <- ifelse(Z>0, 1, 0)

## 사전 확률분포
b0  <-  rep(0, k)
B0  <-  diag(100, k)

## MCMC 
probit.out <- ProbitGibbs(y, X, b0, B0)
require(coda)
out <- mcmc(probit.out)
summary(out)


## ------------------------------------------------------------------------------
mle <- glm(y~X-1, family=binomial(link="probit"))
summary(mle)


## ----probitDA, fig.cap="프로빗회귀분석 모형에 대한 자료증강 깁스 추출", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
plot(out)


## ----probitgibbs, fig.cap="프로빗 깁스 추출 알고리듬의 궤적: 선형회귀분석 모형", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
gibbs.density.plot(probit.out[, 1:2], xlab=bquote(beta[1]), 
                   ylab=bquote(beta[2]))

