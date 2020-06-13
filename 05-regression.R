###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 5
## 박종희
## 2020/06/13
###################################
source("index.R")
## ----galton, fig.cap="골튼의 신장유전 자료", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
library(UsingR)
data(galton)
ggplot(galton, aes(x = parent, y = child)) +
  geom_point(size = 4, alpha=0.1, col="brown") + 
  xlab("부모의 중간 신장") + ylab("자녀의 신장") + 
  labs(caption = "자료출처: UsingR package") + 
  theme_jhp()


## ----echo=TRUE-----------------------------------------------------------------
center <- function(x){
  out <- (x - mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE)
  return(out)
  }
galton.cen <- data.frame(apply(galton, 2, center))
rho.test <- cor.test(galton.cen[,1], galton.cen[,2])
rho.test


## ----echo=TRUE, message=FALSE, warning=FALSE, results='asis'-------------------
rho <- rho.test$estimate
galton.lm <- lm(child ~ parent, data = galton.cen)
stargazer(galton.lm, header=FALSE, type='latex',
          title = '골튼의 신장유전 자료에 대한 회귀분석: 자녀의 신장 ~ 부모의 중간 신장', 
          label='galton.reg')


## ----galton2, fig.cap="골튼의 신장유전에 대한 회귀분석. 자료는 표준화됨. 음영으로 둘러싸인 굵은 실선이 회귀분석선이며 신뢰구간이며 점선이 피어슨 상관계수 1", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
ggplot(galton.lm, aes(x = parent, y = child)) +
  geom_smooth(method = 'lm', aes(fill = 'confidence'), 
  show.legend = F, alpha = 0.2, col = "navy") +
  geom_point(size = 4, alpha=0.1, col="brown") + 
  geom_abline(intercept=0, slope=1, size = 0.5, col="navy", linetype="dashed") + 
  xlim(-3, 3) + ylim(-3, 3) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  xlab("부모의 평균신장(표준화)") + ylab("자녀의 신장(표준화)") + 
  labs(caption = "자료출처: UsingR package") + 
  theme_jhp()


## ----loglinear, echo=TRUE, message=FALSE, fig.cap="지수함수의 근사", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.9, fig.fullwidth=TRUE----
x <- seq(-3, 3, length=100)
plot(x, exp(x), col=addTrans("brown", 50), type="p", cex=0.5, pch=19)
grid()
abline(v = 0, col="gray40", lty=3)
lines(x, x + 1, col="brown", lwd=1)
text(x[90], exp(x[90]) - 0.5, "y = exp(x)")
text(x[80], x[80] - 0.5, "y = x + 1")


## ----sdem, fig.cap="SDEM (Signification, Direction, Effect size, Model-fit)", echo=TRUE, message=FALSE, fig.align="center", fig.asp = 0.7, fig.fullwidth=TRUE----
DiagrammeR::grViz("digraph {
  graph [layout = dot, rankdir = TB]
  
  node [shape = rectangle]        
  rec1 [label = 'Step 1. Significance']
  rec2 [label = 'Step 2. Direction']
  rec3 [label =  'Step 3. Effect Size']
  rec4 [label = 'Step 4. Model-fit']
  
  # edge definitions with the node IDs
  rec1 -> rec2 -> rec3 -> rec4
  }", 
  height = 500)

