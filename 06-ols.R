###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 6
## 박종희
## 2020/06/13
###################################
source("index.R")
## 자료 불러오기
crime <- read.delim("data/us_statewide_crime.txt", head=TRUE)
## DC를 제외하기
crime.nodc <- subset(crime, State!="District of Columbia")
reg1 <- with(crime.nodc, lm(murder.rate ~ single.parent))
reg2 <- with(crime.nodc, lm(murder.rate ~ single.parent * poverty))
## 빈곤 수준을 평균기준으로 상하로 구분
crime.nodc$poverty.level <- cut(crime.nodc$poverty, c(0, 10, 20))
reg3 <- with(crime.nodc, lm(murder.rate ~ single.parent * poverty.level))

## scatter plot 그리기
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
## 그래프그리기
basic_plot <- ggplot(crime.nodc,
       aes(x = single.parent, y = murder.rate, 
           group = poverty.level, 
           color = poverty.level)) +
  theme_bw() + xlab("Single Parent") + ylab("Violent Crime") + 
  labs(color = "poverty.level")

## plot of three regression lines for each poverty level
basic_plot + geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = TRUE,
              mapping=aes(y=predict(reg3, crime.nodc))) + 
  guides(color=guide_legend("Poverty level"))+
  labs(title = "Interaction of Single Parent and Poverty Level")



## ----olsperpen, echo=TRUE, message=FALSE, fig.cap="최소제곱선의 거리와 직교거리", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE, fig.subcap= c("최소제곱선", "직교선")----
set.seed(1999)
## 자료생성
x = seq(0, 1, length=5)
y = x + rnorm(5, 0, .5)
df.reg <- data.frame(x = x, y = y)

## 회귀분석
fit <- lm(y ~ x, data=df.reg)

## 산포도와 회귀분석선 그리기
plot(x,y, pch=19, cex=1.5, ylim=c(0, 1.5), xlim=c(0, 1.5), 
     col=addTrans("blue", 100), asp=1)
abline(fit, col="brown", lwd=1)
segments(x, fit$fitted, x, y, col="red",lty=2)

## 직교선 거리 구하기
perp.segment <- function(x0, y0, lm.fit){
  a <- coef(lm.fit)[1]  
  b <- coef(lm.fit)[2]  
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}
ss <- perp.segment(x, y, fit)
segments(x0=ss$x0, x1=ss$x1, y0=ss$y0, y1=ss$y1, col="blue",lty=3, pty="s")
legend("bottomright", legend=c("직교거리","최소제곱선에서의 거리"), lty=c(3,2), 
       bty="n", col=c("blue", "red"))


## ----rss, fig.cap="잔차(숫자)와 잔차제곱(정사각형)", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.9, fig.fullwidth=TRUE----
asp=0.5
plot(x,y, pch=19, cex=1.5, ylim=c(0, 1.5), xlim=c(0, 1.2), 
     col=addTrans("blue", 100), asp=asp); 
abline(fit, col="brown", lwd=1)
segments(x, fit$fitted, x, y, col="red",lty=2)
text(x, y - fit$res/2, round(fit$res, 2), cex=0.8)
rect(xleft = x - abs(fit$res)/2*asp, ybottom = fit$fitted, 
     xright = x + abs(fit$res)/2*asp, ytop = fit$fitted + fit$res, 
     col = rgb(1,0,0,0.3), border=NA)


## ----echo=TRUE, message=FALSE--------------------------------------------------
set.seed(1999)
x = seq(0, 1, length=10)
y = x + rnorm(10, 0, 0.3)
df.reg <- data.frame(x = x, y = y)
df.reg


## ----resid0, fig.cap="임의 생성된 두 변수의 산포도", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
plot(x,y, pch=19, cex=1.5, col=addTrans("blue", 100), asp = 0.5, main="")


## ----resid, fig.cap="회귀분석 추정값과 잔차", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
fit <- lm(y ~ x, data=df.reg)
plot(x,y, pch=19, cex=1.5, col=addTrans("blue", 100), asp = asp)
abline(fit, col="brown", lwd=1)
segments(x, fit$fitted, x, y, col="red",lty=2)
text(x, y - fit$res/2, round(fit$res, 2), cex=0.8)
rect(xleft = x - abs(fit$res)/2*asp, ybottom = fit$fitted, 
     xright = x + abs(fit$res)/2*asp, ytop = fit$fitted + fit$res, 
     col = rgb(1,0,0,0.3), border=NA)


## ----lowess, fig.cap="선형회귀선과 최적화 접근을 통한 방법", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
plot(x,y, pch=19, cex=1.5, col=addTrans("brown", 100), asp = 0.7)
# ols line
abline(fit, lwd=1, col = 1)
# a loess line
loess_fit <- loess(y ~ x, data=df.reg)
lines(df.reg$x, predict(loess_fit), lwd=1, col = 2)
# a polynomial regression
pol_fit <- lm(y ~ poly(x,3), data=df.reg)
lines(df.reg$x, predict(pol_fit), lwd=1, col = 3)
# a spline function
spline_fit <- smooth.spline(x=df.reg$x, y = df.reg$y)
lines(spline_fit, lwd=1, col = 4)

legend("topleft", c("OLS", "lowess", "polynomial", "spline"), lwd=1, 
       col=1:4, lty=1, bty="n")



## ---- echo=TRUE, message=FALSE, results='asis'---------------------------------
set.seed(1973)
N <- 100
X <- rnorm(N)
Y <- 1 + 3*X + rnorm(N)
reg1 <- lm(Y~X)
reg2 <- lm(X~Y)
jhp_report(reg1, reg2, title="설명변수와 종속변수의 교체", 
           label="tab:xy", dep.var.labels = c("Y", "X"))


## ----regxy, fig.cap="단순회귀분석: 두 변수 X와 Y에 대해 왼쪽 패널은 Y를 종속변수로 사용한 회귀분석결과이며 오른쪽 패널은 X를 종속변수로 사용한 회귀분서결과", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.55, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
par(mfrow=c(1, 2)); 
plot(X, Y, ylim=c(-6, 8), xlim=c(-6, 8), pch = 19, 
     col = addTrans("navy", 30), cex=1, main="종속변수:Y, 설명변수:X")
abline(reg1, col=addTrans("navy", 200))
plot(Y, X, ylim=c(-6, 8), xlim=c(-6, 8), pch = 19, 
     col = addTrans("navy", 30), cex=1, main="종속변수:X, 설명변수:Y")
abline(reg2, col=addTrans("navy", 200))


## ---- echo=TRUE, message=FALSE, results='asis', warning=FALSE------------------
## 자료생성
set.seed(1973)
N <- 100
x <- runif(N, 6, 20)
D <- rbinom(N, 1, .5)
y <-  1 + 0.5*x - .4*D + rnorm(N)
df.lm <- data.frame(y = y, x = x, D = D)
df.lm$D <- factor(df.lm$D, labels=c('남성','여성'))

## 회귀분석
reg.parallel <- lm(y ~ x + D, data=df.lm)
jhp_report(reg.parallel, title="더미변수 회귀모형 추정결과", label="tab:D",
           dep.var.labels = "y (가상자료)")


## ----dummy, fig.cap="더미변수를 이용한 병렬분석: 교육수준이 소득에 미치는 영향을 성별 고정값으로 살펴보는 방법", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
plot(x, y, xlab = "교육", ylab="소득", pch=19, col=addTrans("gray20", 40))
abline(a = reg.parallel$coef[1], b = reg.parallel$coef[2],  
       lwd=4, col=addTrans("brown", 200))
abline(a = reg.parallel$coef[1] + reg.parallel$coef[3], 
       b = reg.parallel$coef[2], lwd=4, col=addTrans("brown", 100))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4,4), lty=c(1,1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))


## ---- echo=TRUE, message=FALSE, results='asis', warning=FALSE------------------
reg.inter <- lm(y ~ x * D) 
jhp_report(reg.inter, title="상호작용 회귀모형 추정결과", label="tab:xD",
           dep.var.labels = "y (가상자료)")


## ----dummy2, fig.cap="상호작용분석: 교육수준이 소득에 미치는 영향을 성별 변수와의 상호작용으로 살펴보는 방법. 교육수준이 높아짐에 따라 성별 소득격차가 점차 줄어드는 경향이 있음.", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
plot(x, y, xlab = "교육", ylab="소득", pch=19, col=addTrans("gray20", 40))
abline(a = reg.inter$coef[1],  b = reg.inter$coef[2],  
       lwd=4, col=addTrans("brown", 200))
abline(a = reg.inter$coef[1] + reg.inter$coef[3], 
       b = reg.inter$coef[2] + reg.inter$coef[4],  
       lwd=4, col=addTrans("brown", 100))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4,4), lty=c(1,1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))


## ---- echo=TRUE, message=FALSE, results='asis', warning=FALSE------------------
set.seed(1973)
N <- 100
x <- runif(N, 6, 20)
D <- rbinom(N, 1, .5)
y <-  1 + 0.5*x - .4*D + 0.4*D*x + rnorm(N)
df.lm2 <- data.frame(y = y, x = x, D = D)
df.lm2$D <- factor(df.lm2$D, labels=c('여성','남성'))

reg.parallel2 <- lm(y ~ x + D, data=df.lm2)
reg.inter2 <- lm(y ~ x * D, data=df.lm2)
jhp_report(reg.parallel2, reg.inter2, 
           title="상호작용 회귀모형과 합산형 모형의 비교", 
           label="tab:xDD", dep.var.labels = "y (가상자료)")


## ----dummy3, fig.cap="더미변수를 이용한 병렬분석과 상호작용분석)", echo=TRUE, message=FALSE, fig.align="center", fig.width = 12, fig.height = 6, fig.fullwidth=TRUE----
par(mfrow=c(1, 2), mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02, cex.main=0.5)
## 병렬분석
plot(df.lm2$x, df.lm2$y, main="병렬분석", pch=19, col=addTrans("gray20", 40), 
     xlab = "교육", ylab="소득")
abline(a = reg.parallel2$coef[1], b = reg.parallel2$coef[2],  
       lwd=4, col=addTrans("brown", 100))
abline(a = reg.parallel2$coef[1] + reg.parallel2$coef[3], 
       b = reg.parallel2$coef[2],  lwd=4, col=addTrans("brown", 200))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4,4), lty=c(1,1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))

## 상호작용분석
plot(df.lm2$x, df.lm2$y, main="상호작용분석", pch=19, col=addTrans("gray20", 40), 
     xlab = "교육", ylab="소득")
abline(a = reg.inter2$coef[1], b = reg.inter2$coef[2],  
       lwd=4, col=addTrans("brown", 100))
abline(a = reg.inter2$coef[1] + reg.inter2$coef[3], 
       b = reg.inter2$coef[2] + reg.inter2$coef[4],  
       lwd=4, col=addTrans("brown", 200))
legend("topleft", legend=c("남성", "여성"), bty="n", lwd=c(4,4), lty=c(1,1),
       col=c(addTrans("brown", 200), addTrans("brown", 100)))


## ------------------------------------------------------------------------------
methods("predict")


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------------
## 회귀분석 예측치
df.lm2$pred.parallel2 = predict(reg.parallel2)
df.lm2$pred.inter2 = predict(reg.inter2)

## 신뢰구간
conf.parallel2 = predict(reg.parallel2, interval = "prediction")
conf.inter2 = predict(reg.inter2, interval = "prediction")

head(conf.parallel2)
head(conf.inter2)


## ----dummy4, fig.cap="병렬분석(왼쪽)과 상호작용분석(오른쪽)의 신뢰구간", echo=TRUE, message=FALSE, fig.align="center", fig.width = 12, fig.height = 6, fig.fullwidth=TRUE----

## 병렬분석
df.parallel = cbind(df.lm2, conf.parallel2)
g.parallel <- ggplot(df.parallel, aes(x = x, y = y, color = D) ) +
  geom_point() +
  geom_ribbon( aes(ymin = lwr, ymax = upr, fill = D, color = NULL), alpha = .2) +
  geom_line(aes(y = fit), size = 1) + 
  labs(subtitle = "병렬분석", fill = NULL, colour = NULL) +  
  theme_jhp() + xlab("교육") + ylab("소득")

## 상호작용분석
df.inter = cbind(df.lm2, conf.inter2)
g.inter <- ggplot(df.inter, aes(x = x, y = y, color = D) ) +
  geom_point() +
  geom_ribbon( aes(ymin = lwr, ymax = upr, fill = D, color = NULL), alpha = .2) +
  geom_line(aes(y = fit), size = 1) + 
  labs(subtitle = "상호작용분석", fill = NULL, colour = NULL) +  
  theme_jhp() + xlab("교육") + ylab("소득")

NetworkChange::multiplot(g.parallel, g.inter, cols=2)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
require(car)
data(Prestige)
Prestige <- Prestige %>% tidyr::drop_na()
attach(Prestige)
names(Prestige)


## ----prestige1, fig.cap="명성자료의 분포", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.9, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
Prestige %>%
  dplyr::select(education, income, women, prestige) %>%
  tidyr::gather()%>%
  ggplot(aes(value)) + 
    geom_histogram() + 
    facet_wrap(~key, scales = 'free_x')


## ----prestige2, fig.cap="명성자료의 변수간 상관성", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.9, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
library(GGally)
Prestige %>%
  dplyr::select(education, income, women, prestige, type) %>%
  ggpairs(aes(alpha = 0.4))


## ----prestige3, fig.cap="명성자료의 종속변수와 설명변수", echo=TRUE, message=FALSE, fig.align="center", fig.width = 10, fig.height = 8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
ggplot(Prestige, aes(x = education, y = prestige, col=type)) + 
  geom_point(size = 5, alpha=0.5) + 
  labs(colour=NULL) + 
  theme_jhp()


## ---- echo=TRUE, warning=FALSE, message=FALSE, results='asis'------------------
reg1 <- lm(prestige ~ education, data=Prestige)
jhp_report(reg1, title="직업별 명성과 교육수준의 관계", 
           label="tab:prestige1",
           dep.var.labels = "prestige")


## ----prestige4, fig.cap="명성자료의 회귀분석", echo=TRUE, message=FALSE, fig.align="center", fig.width = 10, fig.height = 8, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
ggplot(Prestige, aes(x = education, y = prestige)) +
  geom_smooth(method='lm') +
  geom_point(size = 5, alpha=0.3, aes(col=type)) +
  labs(colour=NULL) + 
  theme_jhp()


## ----corcomp, echo=TRUE, message=FALSE, results='asis'-------------------------
identical(with(Prestige, cor.test(prestige, education))$est, 
          reg1$coef[2] * with(Prestige, sd(education)/sd(prestige)))
print(with(Prestige, cor.test(prestige, education))$est- 
          reg1$coef[2] * with(Prestige, sd(education)/sd(prestige)))


## ----prestige5, fig.cap="명성자료 회귀분석의 진단", echo=TRUE, message=FALSE, fig.align="center", fig.width = 12, fig.height = 12, fig.fullwidth=TRUE----
library(ggfortify)
autoplot(reg1)


## ----prestige6, fig.cap="원소득자료의 부분상관계수", echo=TRUE, message=FALSE, fig.align="center", fig.width = 12, fig.height = 6, fig.fullwidth=TRUE----
avPlots(lm(prestige ~ education + income, data=Prestige))


## ----prestige6-1, fig.cap="로그변환된 소득자료의 부분상관계수", echo=TRUE, message=FALSE, fig.align="center", fig.width = 12, fig.height = 6, fig.fullwidth=TRUE----
avPlots(lm(prestige ~ education + log(income), data=Prestige))


## ---- echo=TRUE, message=FALSE, results='asis', warning=FALSE------------------
Prestige$blue <- as.factor(ifelse(Prestige$type == "bc", 1, 0))
reg2 <- lm(prestige ~ education * blue + log(income) + women, data=Prestige)
reg2.1 <- lm(prestige ~ education + blue, data=Prestige)
reg2.2 <- lm(prestige ~ education * blue, data=Prestige)
reg2.3 <- lm(prestige ~ education + blue + log(income) + women, data=Prestige)
jhp_report(reg2, reg2.1, reg2.2, reg2.3, title="직업군 명성의 결정요인", 
           label="tab:pres4", dep.var.labels = "prestige")


## ----prestige7, fig.cap="명성에 대한 소득의 효과: 블루컬러 노동자와 기타 직업군의 차이", echo=TRUE, message=FALSE, warning=FALSE, fig.align="center", fig.asp = 0.8, fig.fullwidth=TRUE----
basic_plot <- 
  ggplot(Prestige, aes(x = education, y = prestige, color = blue)) +
  labs(x = "education", y = "prestige", color = "blue collar") +
  theme_jhp()

basic_plot + geom_point(alpha = .3, size = 2) +
  geom_smooth(method = "lm", mapping=aes(y=predict(reg2, Prestige))) 


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(caret)
Prestige$logincome <- log(Prestige$income)
PrestigeCV <- 
  Prestige %>% dplyr::select(prestige, income, logincome, education, women, blue) %>% na.omit()


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------------
## 5겹 교차검증을 5회 실시
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

## 모형 훈련
model1 <- train(prestige ~ income + education + women, 
               data = PrestigeCV, method = "lm", trControl = train.control)
model2 <- train(prestige ~ logincome + education + women, 
               data = PrestigeCV, method = "lm", trControl = train.control)
model3 <- train(prestige ~ (logincome + education + women) * blue, 
                data = PrestigeCV, method = "lm", trControl = train.control)


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------------
results <- resamples(list(model1, model2, model3))
summary(results)


## ----bwplot, message=FALSE, warning=FALSE, fig.cap="교차타당성 검사결과에 대한 상자수염플롯", echo=TRUE, fig.align="center", fig.width = 12, fig.height = 5, fig.fullwidth=TRUE----
bwplot(results)


## ----dotplot, message=FALSE, warning=FALSE, fig.cap="교차타당성 검사결과에 대한 닷플롯", echo=TRUE, fig.align="center", fig.width = 12, fig.height = 5, fig.fullwidth=TRUE----
dotplot(results)


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------------
difValues <- diff(results)
summary(difValues)


## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------------
train.control <- trainControl(method = "LOOCV")
model2 <- train(prestige ~ logincome + education + women, 
               data = PrestigeCV, method = "lm", trControl = train.control)
model3 <- train(prestige ~ (logincome + education + women) * blue, 
                data = PrestigeCV, method = "lm", trControl = train.control)
print(list(model2, model3))

