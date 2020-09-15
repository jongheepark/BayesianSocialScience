###################################
## 사회과학자를 위한 데이터과학 방법론
## Ch. 2
## 박종희
## 2020/06/13
###################################
source("index.R")
## ---- echo=TRUE, message=FALSE, eval=FALSE-------------------------------------
## birthday_problem_test <- function(k){
##   ## k: 그룹안의 사람 수
##   n <- 365
##   numerator <- factorial(365)
##   denominator <- n^k*(factorial(365-k))
##   out <- 1 - numerator/denominator
##   return(out)
## }


## ---- echo=TRUE, message=FALSE, eval=TRUE--------------------------------------
birthday_problem <- function(k){
  n <- 365
  log.numerator <- lgamma(365+1)
  log.denominator <- log(n^k) + lgamma(365-k + 1)
  out <- 1 - exp(log.numerator - log.denominator)
  return(out)
}
start_time <- Sys.time()
birthday_problem(k=20)
Sys.time() - start_time


## ----echo=TRUE-----------------------------------------------------------------
## textbook 윤년 수정 1: leap year correction
birthday.simulator <- function(n, total.sim = 1000000){
probs <- c(rep(1/365.25,365),(97/400)/365.25)
  anyduplicated <- function(ignored) 
    any(duplicated(sample(1:366, n, prob=probs, replace=TRUE)))
  out <- sum(sapply(seq(total.sim), anyduplicated))/total.sim
  return(out)
}

birthday.simulator(20)

## revised 윤년 수정 2: leap year computing using Gregorius correction
## on "Tue Sep 15 09:28:54 2020"
birthday.simulator.gregorius<- function(n, total.sim = 1000000){
  one.year <- 365+97/400
  probs <- c(rep(1/one.year,365),(97/400)/one.year)
  anyduplicated <- function(ignored) 
    any(duplicated(sample(1:366, n, prob=probs, replace=TRUE)))
  out <- sum(sapply(seq(total.sim), anyduplicated))/total.sim
  return(out)
}
birthday.simulator.gregorius(20)

## numerical birthday prob calculator 
birthday.simulator2 <- function(n, total.sim = 1000000){
  probs <- c(rep(1/365, 365))
  anyduplicated <- function(ignored) 
    any(duplicated(sample(1:365, n, prob=probs, replace=TRUE)))
  out <- sum(sapply(seq(total.sim), anyduplicated))/total.sim
  return(out)
}
birthday.simulator2(20)

