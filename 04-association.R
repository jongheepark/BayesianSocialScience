###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 4
## 박종희
## 2020/06/13
###################################
source("index.R")
## Party Id and Gender: General Social Survey 1991
library(knitr)
library(kableExtra)
dt <- data.frame(female = c(279, 73, 225), 
                 male = c(165, 47, 191))
rownames(dt) <- c("D", "I", "R")
dt %>%
  kable("latex", booktabs = TRUE,
  caption = '성별 정당일체감, 자료출처: 1991 General Social Survey')  %>% 
  kable_styling(latex_options = "striped", position = "center")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
t(apply(dt, 1, prop.table))


## ---- echo=TRUE, message=FALSE-------------------------------------------------
apply(dt, 2, prop.table)


## ------------------------------------------------------------------------------
prop.table(dt)


## ----gss3d, message=FALSE, warning=FALSE, fig.cap="성별 정당일체감, 미국 일반사회조사 (1991)", echo=TRUE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
library(scatterplot3d)

## x, y, z 변수를 지정
x <- c("D", "I", "R")
y <- c("Female", "Male")
z <- prop.table(dt)

## 데이터 프레임으로 변환
mydat = data.frame("정당일체감" = as.vector(row(dt)),
                   "성별" = as.vector(col(dt)),
                   "확률" = as.vector(unlist(z)))

## 3d 그래프 그리기
scatterplot3d(mydat, type = "h", lwd = 2,
              x.ticklabs = c("민주당","","무당파","","공화당"),
              y.ticklabs = c("여성","","","","","남성"),
              col.axis="blue", col.grid="lightblue", angle= 60, pch=20, 
              y.margin.add = 0.5, cex.symbols=3, color = "brown", box = F)


## ---- echo=T-------------------------------------------------------------------
prop.table(apply(dt, 2, sum))


## ----chisq, fig.cap="카이제곱분포", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)
curve(dchisq(x, 1), from = 0, to = 100, lwd = 2, ylab="f(y)", xlab="y", 
      col=addTrans('firebrick4', 50))
curve(dchisq(x, 5), from = 0, to = 100, lwd = 2, add=T,
      col=addTrans('firebrick4', 100))
curve(dchisq(x, 10), from = 0, to = 100, lwd = 2, add=T,
      col=addTrans('firebrick4', 150))
curve(dchisq(x, 50), from = 0, to = 100, lwd = 2, add=T,
      col=addTrans('firebrick4', 200))

legend("topright", 
       legend = c('df = 1', 'df = 5', 'df = 10', 'df = 50'),
       lwd=2, bty="n", 
       col=c(addTrans('firebrick4', 50), addTrans('firebrick4', 100), 
             addTrans('firebrick4', 150), addTrans('firebrick4', 200)))



## ---- echo=TRUE, message=FALSE-------------------------------------------------
test0 <- chisq.test(dt)
test0$expected


## ---- echo=TRUE, message=FALSE-------------------------------------------------
test0


## ---- echo=TRUE, message=FALSE-------------------------------------------------
test0$stdres


## ---- echo=TRUE, message=FALSE-------------------------------------------------
fisher.test(dt)

## ---- echo=TRUE, message=FALSE-------------------------------------------------
data(Titanic)
tab.class <- apply(Titanic, c(1, 4), sum)
tab.class


## ---- echo=TRUE, message=FALSE-------------------------------------------------
test.class <- chisq.test(tab.class[1:3,])
test.class
test.class$stdres


## ---- echo=TRUE, message=FALSE-------------------------------------------------
Y = c(10, 9, 9.1, 7, 5)
X = c(3.4, 2.9, 3.3, 3, 3.9)
r <- sum((X - mean(X))*(Y - mean(Y)))/4/(sd(X)*sd(Y))
r


## ---- echo=TRUE, message=FALSE-------------------------------------------------
cor.test(x = X, y = Y)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
denom <- norm(as.matrix(X - mean(X)),"f")*
  norm(as.matrix(Y - mean(Y)),"f")
numer <- t(X - mean(X))%*%(Y - mean(Y))
numer/denom


## ---- echo=TRUE, message=FALSE-------------------------------------------------
cor.test(X, Y, method="kendall")
cor.test(X, Y, method="spearman")


## ---- echo=TRUE, message=FALSE-------------------------------------------------
require(stats); require(graphics)
anscombe
## mean
apply(anscombe, 2, mean)
## sd
apply(anscombe, 2, sd)


## ---- echo=TRUE, message=FALSE-------------------------------------------------
rhos <- setNames(as.list(1:4), paste0("rho", 1:4))
for(i in 1:4) {
  rhos[[i]] <- with(anscombe, 
                    eval(parse(text=paste0("cor(y",i, ", x", i, ")"))))
}
rhos


## ----anscombe, message=FALSE, warning=FALSE, fig.cap="앤스콤의 사중주", echo=TRUE, fig.align="center", fig.asp = 1, fig.fullwidth=TRUE----
pdf(file="anscomb.pdf", width=10, height=10, family="sans")
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), 
          oma =  c(0, 0, 2, 0))
ff <- y ~ x
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  fit <- lm(ff, data = anscombe)
  plot(ff, data = anscombe, col = "brown", 
       pch = 21, bg = "orange", cex = 2, ylab="", xlab=paste("(", i, ")"),
       xlim = c(3, 19), ylim = c(3, 13))
  abline(fit, lwd = 2, col = addTrans("navy", 50))
  abline(a = 0, b = rhos[[i]], lwd = 2, col = addTrans("navy", 200))
  legend("topleft", legend=c("correlation", "regression line"), 
         bty="n", lty=1, lwd=2, 
         col = c(addTrans("navy", 200), addTrans("navy", 50)))
}
par(op)
dev.off()


## ----table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'---------
tabl <- "  
|               | 박근혜          | 문재인  | 합계 |
|---------------|:-------------:  |------:|------:|
| 청장년        | 0.35            |  0.05 | 0.40 |
| 노년          | 0.25            |  0.35 | 0.60 |
| 합계          | 0.60            |  0.40 | 1    |
"
cat(tabl) # output the table in a format good for HTML/PDF/docx conversion

