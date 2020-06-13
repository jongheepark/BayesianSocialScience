###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 3
## 박종희
## 2020/06/13
###################################
source("index.R")
## ---- echo=TRUE, message=FALSE-------------------------------------------------
set.seed(1999)
n = 10 
p = 0.5

## 확률 p=0.5와 n=10를 가진 이항확률변수를 200번 추출
df.binom <- data.frame(x = rbinom(200, n, p))
table(df.binom)


## ----bivar, echo=TRUE, message=FALSE, fig.cap="10회 동전던지기를 200번 반복한 결과", fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----

ggplot(df.binom, aes(x)) + 
  geom_histogram(binwidth=0.3) + 
  theme_jhp() + xlab("동전의 앞면이 나온 횟수") + ylab("빈도") + 
  ## x축의 눈금을 1에서 9까지 한 칸씩 표시
  scale_x_continuous(breaks=seq(1,9,1))


## ---- echo=TRUE, message=FALSE-------------------------------------------------
n = 79
p = 0.52
# Pr(Y > 39|n, p) = 1 - Pr(Y <= 39|n, p)
1 - pbinom(39, n, p)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
dmultinom(x=c(28,28,23), prob=c(1/3, 1/3, 1/3))


## ----echo=TRUE-----------------------------------------------------------------
p <- .5
x <- 0:5
dgeom(x, p)


## ----geodist, fig.cap="기하분포,  p=0.5, n=5", echo=TRUE, fig.align="center", fig.pos = 'ht', fig.asp = 0.7, fig.fullwidth=TRUE----
df.geom <- data.frame(x = x, y = dgeom(x, p))
ggplot(df.geom, aes(x=x, y=y)) +
  geom_line() + geom_point(size=3, alpha=0.3) + 
  theme_jhp() + xlab("Y") + ylab("밀도")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
## 유한모집단 보정계수 함수 (N = 모집단 크기, n = 샘플크기)
fpcf <- function(N, n){
  sqrt((N-n)/(N-1))   
}
n = 100
s = 0.05
## 보정계수
fpcf(N=1000, n=100)

tvalue <- qt(0.975, df=(n-1))
## 보정 전의 신뢰구간
ci.raw <- c(0.52 + (tvalue*s/sqrt(n)), 
            0.52 - (tvalue*s/sqrt(n)))
ci.raw
## 보정 후의 신뢰구간
ci.correct <- c(0.52 + 
                  (tvalue*s/sqrt(n))*fpcf(N=1000, n=100), 
                0.52 - 
                  (tvalue*s/sqrt(n))*fpcf(N=1000, n=100))
ci.correct


## ----fpcf, fig.cap="유한모집단 보정 전후의 신뢰구간 비교", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
par(mfrow=c(2,2))
par (mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)
## plot 1
plot(x = ci.raw, y = c(2, 2), ylim=c(0, 3), type="l", 
     ylab="", xlab="", lwd=3, axes=FALSE)

## plot 2
plot(x = ci.raw, y = c(2, 2), ylim=c(0, 3), type="l", 
     ylab="", xlab="", lwd=3, axes=FALSE)
axis(1); grid()

## plot 3
plot(x = ci.raw, y = c(2, 2), ylim=c(0, 3), type="l", 
     ylab="", xlab="", lwd=3, axes=FALSE)
axis(1); grid()
lines(x = ci.correct, y = c(1, 1), lwd=3, col="brown")

## plot 4
plot(x = ci.raw, y = c(2, 2), ylim=c(0, 3), type="l", 
     ylab="", xlab="", lwd=3, axes=FALSE)
axis(1); grid()
lines(x = ci.correct, y = c(1, 1), lwd=3, col="brown")
text(mean(ci.raw), 1.75, "보정전 신뢰구간", adj=0)
text(mean(ci.raw), 0.75, "보정후 신뢰구간", adj=0)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
dhyper(2, 20, 30, 4)


## ----echo=TRUE-----------------------------------------------------------------
dhyper(3, 4, 4, 4) + dhyper(4, 4, 4, 4)


## ----echo=TRUE-----------------------------------------------------------------
dhyper(4, 4, 4, 4)


## ----dpois, fig.cap="프와송분포, 평균 = 18", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
set.seed(1990)
lambda <- 18
n <- 1000
x <- rpois(n, lambda)

df.pois <- data.frame(x = x, y = dpois(x, lambda))
ggplot(df.pois, aes(x=x, y=y)) +
  geom_line() + geom_point(size=3, alpha=.1) + 
  theme_jhp() + xlab("Y") + ylab("밀도")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
rnbinom(10, 5, 0.5) 


## ----dnbinom, fig.cap="음이항분포, r=5, p=1/2", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
x <- 1:50
y <- dnbinom(x, 5, 0.5)
df.nbinom <- data.frame(x = x, y = y)
ggplot(df.nbinom, aes(x=x, y=y)) +
  geom_line() + geom_point(size=3, alpha=.3) + 
  theme_jhp() + xlab("Y") + ylab("밀도")


## ----unifdist, message=FALSE, warning=FALSE, fig.cap="균등분포", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
set.seed(2000)
## unif(0,1)
curve(dunif(x, 0, 1), lwd = 2, xlim=c(0, 4), ylim=c(0,1 ), 
      ylab="밀도", xlab="Y", col= 1)
grid()
## unif(0,2)
curve(dunif(x, 0, 2), lwd = 2, add=T, col=2)
## unif(0,3)
curve(dunif(x, 0, 3), lwd = 2, add=T, col=3)
## unif(0,4)
curve(dunif(x, 0, 4), lwd = 2, add=T, col=4)
## 그래프 레전드
legend("topright", lwd=2, bty="n", 
       legend = c('Unif(0, 1)','Unif(0, 2)','Unif(0, 3)','Unif(0, 4)'), 
       col=1:4)


## ----betadist, message=FALSE, warning=FALSE, fig.cap="베타분포", echo=TRUE, fig.align="center", fig.asp = 0.9, fig.fullwidth=TRUE----
set.seed(2000)
curve(dbeta(x, 2, 2), lwd = 2, xlim=c(0, 1), ylim=c(0, 3), 
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dbeta(x, 3, 1), lwd = 2, add=T, col=2)
curve(dbeta(x, 1, 3), lwd = 2, add=T, col=3)
curve(dbeta(x, 1, 1), lwd = 2, add=T, col=4)
legend("top", lwd=2, bty="n", 
       legend = c('Beta(2, 2)', 'Beta(3, 1)', 'Beta(1, 3)', 'Beta(1, 1)'),
       col=1:4)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
pexp(300, rate=1/3000)


## ----expdist, message=FALSE, warning=FALSE, fig.cap="지수분포", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
set.seed(2000)
curve(dexp(x, 7), lwd = 1, xlim=c(0, 4), ylim=c(0,5), 
      ylab="밀도", xlab="Y", col=addTrans('firebrick4', 50))
grid()
curve(dexp(x, 5), lwd = 2, add=T, col=addTrans('firebrick4', 100))
curve(dexp(x, 3), lwd = 3, add=T, col=addTrans('firebrick4', 150))
curve(dexp(x, 1), lwd = 4, add=T, col=addTrans('firebrick4', 200))
legend("topright", lwd=1:5, bty="n", 
       legend = c('Exp(7)', 'Exp(5)', 'Exp(3)', 'Exp(1)'),
       col=c(addTrans('firebrick4', 50), addTrans('firebrick4', 100), 
             addTrans('firebrick4', 150), addTrans('firebrick4', 200)))


## ----gauss,  out.width = "100%", fig.cap="독일지폐에 인쇄된 정규분포와 가우스", echo=FALSE----
knitr::include_graphics("figures/gauss.jpg")


## ----pima, message=FALSE, warning=FALSE, fig.cap="피마 인디언 체질량지수. 세로 점선은 평균, 음영처리된 커브는 관측자료의 경험적 분포, 붉은 실선은 관측자료의 평균과  분산을 이용해 그린 정규분포 확률밀도함수", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
require(mlbench)
data(PimaIndiansDiabetes)
ggplot(PimaIndiansDiabetes, aes(x=mass)) + 
  geom_density(fill="brown", alpha=0.3) + 
  geom_vline(xintercept = mean(PimaIndiansDiabetes$mass, na.rm=TRUE),
             linetype="dashed", color = "blue", size=1)+
  stat_function(fun=dnorm, color="red",
                args=list(mean=mean(PimaIndiansDiabetes$mass),
                          sd=sd(PimaIndiansDiabetes$mass))) + 
  labs(caption = "자료출처: PimaIndiansDiabetes") +  
  theme_jhp() + xlab("Y") + ylab("밀도")


## ----normdist, message=FALSE, warning=FALSE, fig.cap="정규분포", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
curve(dnorm(x, 0, 0.5), lwd =2, xlim=c(-6,6), ylim=c(0, .8), 
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dnorm(x, 0, 1), lwd = 2, add=T, col=2)
curve(dnorm(x, 0, 2), lwd = 2, add=T, col=3)
curve(dnorm(x, 0, 3), lwd = 2, add=T, col=4)
legend("topleft", legend = c('N(0, 0.25)', 'N(0, 1)', 'N(0, 4)', 'N(0, 9)'),
       lwd=2, bty="n", col=1:4)


## ----diridist, message=FALSE, warning=FALSE, fig.cap="디리클레분포, Dirichlet(2,2,2)", fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
## 

require(plot3D); 
require(ggplot2); 
require(MCMCpack); 
require(akima); 
require(rgl)
set.seed(1999)
# Dirichlet parameters (Customize these!)
alpha_params = c(2,2,2)
# Get a grid of points and normalize them to be on the simplex
granularity = 20
draws <- matrix(ncol=3,nrow=(granularity*granularity*granularity)-1)
# lots of points on the edge
i=0
for (x in 1:granularity){
  for (y in 1:granularity){
    for (z in 1:granularity){
      draws[i,] <- c(x,y,z) # point on grid
      draws[i,] = draws[i,] / sum(draws[i,]) # normalize
      i = i+1
    }
  }
}
x <- draws[,1]
y <- draws[,2]
z <- draws[,3]
density <- ddirichlet(draws, alpha_params)
# transform the simplex to euclidean space (eliminating one dimension)
x <- .5 * (2*x+y)
y <- .5 * y * sqrt(3)
# interpolate a fine (100x100) grid from our points
grid <- interp.new(x,y,density,duplicate="strip",linear=FALSE,
                   xo=seq(min(x),max(x),length=100),
                   yo=seq(min(y),max(y),length=100))

# PLOT #1: a heatmap 
image2D(x=grid$x, y=grid$y, z=grid$z)


## ----tdist, message=FALSE, warning=FALSE, fig.cap="스튜던트  t 분포", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
addTrans <- NetworkChange:::addTrans
curve(dt(x, df = 1), lwd =1, xlim=c(-6,6), ylim=c(0, .45), 
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dt(x, df = 5), lwd = 1, add=T, col=2)
curve(dt(x, df = 10), lwd = 1, add=T, col=3)
curve(dt(x, df = 30), lwd = 1, add=T, col=4)
curve(dnorm(x, 0, 1), lwd = 5, add=T, col=addTrans(6, 50))
legend("topleft", 
       legend = c('t(0,1,1)', 't(0,1,5)', 't(0,1,10)', 't(0,1,30)', 'N(0, 1)'),
       lwd=c(1,1,1,1,5), bty="n", col=c(1:4, addTrans(6, 50)))


## ----cauchydist, message=FALSE, warning=FALSE, fig.cap="코시분포", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
curve(dcauchy(x, scale = 1), lwd =1, xlim=c(-8,8), ylim=c(0, .35), 
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dcauchy(x, scale = 2), lwd = 1, add=T, col=2)
curve(dcauchy(x, scale = 3), lwd = 1, add=T, col=3)
curve(dcauchy(x, scale = 4), lwd = 1, add=T, col=4)
curve(dt(x, df = 1), lwd = 5, add=T, col=addTrans(6, 50))
legend("topleft", 
       legend = c('Cauchy(0, 1)', 'Cauchy(0, 2)', 'Cauchy(0, 3)', 'Cauchy(0, 4)', 't(0,1,1)'), 
       lwd=c(1,1,1,1,5), bty="n", col=c(1:4, addTrans(6, 50)))


## ----laplacedist, message=FALSE, warning=FALSE, fig.cap="라플라스분포", echo=TRUE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
require(rmutil)
curve(dlaplace(x, s=1), lwd =1, xlim=c(-8,8), ylim=c(0, .55), 
      ylab="밀도", xlab="Y", col=1)
grid()
curve(dlaplace(x, s=2), lwd = 1, add=T, col=2)
curve(dlaplace(x, s=3), lwd = 1, add=T, col=3)
curve(dlaplace(x, s=4), lwd = 1, add=T, col=4)
legend("topleft", legend = c('Laplace(0, 1)', 'Laplace(0, 2)', 
                             'Laplace(0, 3)', 'Laplace(0, 4)'),
       lwd=1, bty="n", col=1:4)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
my.samples <- function(dist, r, n, param1, param2=NULL){
  set.seed(123) # set the seed for reproducibility
  switch(dist, 
         "Exponential" = matrix(rexp(r*n,param1),r),
         "Normal" = matrix(rnorm(r*n,param1,param2),r),
         "Uniform" = matrix(runif(r*n,param1,param2),r),
         "Poisson" = matrix(rpois(r*n,param1),r),
         "Binomial" = matrix(rbinom(r*n,param1,param2),r),
         "Beta" = matrix(rbeta(r*n,param1,param2),r),
         "Gamma" = matrix(rgamma(r*n,param1,param2),r),
         "Chi-squared" = matrix(rchisq(r*n,param1),r), 
         "Cauchy" = matrix(rcauchy(r*n,param1, param2),r) 
         )
}


## ---- echo=TRUE, message=FALSE-------------------------------------------------
mu <- function(dist, param1, param2=NULL){
  switch(dist, 
         "Exponential" = param1^-1,
  	     "Normal" = param1,
         "Uniform" = (param1+param2)/2,
         "Poisson" = param1,
         "Binomial" = param1*param2,
         "Beta" = param1/(param1+param2),
         "Gamma" = param1/param2,
         "Chi-squared" = param1, 
         "Cauchy" = param1)
}
sigma <- function(dist, param1, param2=NULL){
  switch(dist, 
         "Exponential" = param1^-1,
         "Normal" = param2,
         "Uniform" = sqrt((param2-param1)^2/12),
         "Poisson" = sqrt(param1),
         "Binomial" = sqrt(param1*param2*(1-param2)),
         "Beta" = sqrt(param1*param2/((param1+param2)^2*(param1+param2+1))),
         "Gamma" = sqrt(param1/(param2)^2),
         "Chi-squared" = sqrt(2*param1), 
         "Cauchy" = sqrt(param2))
}


## ---- echo=TRUE, message=FALSE-------------------------------------------------
CLT <- function(dist, param1, param2=NULL, r = 10000) {
  ## dist = 확률밀도함수
  ## r = 반복추출횟수
  par(mfrow = c(3,3), mgp = c(1.75,.75,0), 
      oma = c(2,2,2,2), mar = c(3,3,2,0), xpd = NA)
  for (n in c(1:6,10,50,100)) {
    samples <- my.samples(dist, r, n, param1, param2)
    ## 표본평균 계산
    sample.means <- apply(samples, 1, mean) 
    ## 표본평균 히스토그램
    hist(sample.means, col=ifelse(n<=10,gray(.1*(11-n)), rgb(0,0,n,max=110)), 
         freq=FALSE, xlab="Sample Mean", main=paste("n=",n)) 
    
    ## CLT 정규분포 그리기 N(mean=mu, sd=sigma/sqrt(n))
    x <- seq(min(sample.means),max(sample.means),length=100)
    curve(dnorm(x, mean=mu(dist, param1, param2), 
                sd=(sigma(dist, param1, param2))/sqrt(n)),
                col="red", lwd=2,add=TRUE) 
  }
  ## 확률분포 이름 레이블
  mtext(paste(dist," Distribution (",
              param1,ifelse(is.null(param2),"",","),
              param2,")",sep=""), outer=TRUE, cex=1)
}


## ----clt1, message=FALSE, warning=FALSE, fig.cap="지수분포를 이용한 중심극한정리 시뮬레이션, 9가지 서로 다른 표본크기로부터 10000번의 표본 추출 결과", echo=TRUE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
CLT("Exponential",1)


## ----clt2, message=FALSE, warning=FALSE, fig.cap="균등분포를 이용한 중심극한정리 시뮬레이션, 9가지 서로 다른 표본크기로부터 10000번의 표본 추출 결과", echo=TRUE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
CLT("Uniform",1, 5)


## ----clt3, message=FALSE, warning=FALSE, fig.cap="코시분포를 이용한 중심극한정리 시뮬레이션, 9가지 서로 다른 표본크기로부터 10000번의 표본 추출 결과", echo=TRUE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
CLT("Cauchy",1, 1)


## ----binormdens, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="상관성이 다른 이변량정규분포들", fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
knitr::include_graphics("figures/bivariateNormalR.pdf")

