## ----setup, include=FALSE------------------------------------------------------
## First specify the packages of interest
packages <- c("tidyverse", "knitr", "survey", "ggthemes", "extrafont", "dplyr", "readxl", 
              "UsingR", "srvyr", "tidyr", "readxl", "showtext", "ggplot2", "ggExtra", 
              "tibble", "broom", "kableExtra", "lmtest", "scales", "DiagrammeR",  
              "NetworkChange", "MCMCpack", "stargazer", "sysfonts")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# 한글 폰트 추가 (시스템에 설치된 폰트 사용)
font_add_google("Noto Sans KR", "noto")  # 구글 폰트 사용
showtext_auto()


## transparent dots in graph
addTrans <- NetworkChange:::addTrans

## default R base plot format
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=.02)

## theme_set(theme_classic())
knitr::opts_chunk$set(echo=TRUE, out.width="100%", fig.align = "center", message=F, warning=F, fig.height = 6, cache=T, dpi = 300, dev = "png")
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
theme_set(theme_gray(base_family='NanumGothic'))

## my style latex summary of regression
jhp_report <- function(...){
  output <- capture.output(stargazer(..., omit.stat=c("f", "ser")))
  # The first three lines are the ones we want to remove...
  output <- output[4:length(output)]
  # cat out the results - this is essentially just what stargazer does too
  cat(paste(output, collapse = "\n"), "\n")
}

## regression summary plot
## The first variable after ~ will be considered as the explanatory var.
ggReg <- function (fit, title="") {
  gtext <- paste("Call = ", summary(fit)$call[2], "\n",   
                 "Adj R2 = ",signif(summary(fit)$adj.r.squared, 5))
  ggplot(fit$model, aes_string(x = names(fit$model)[2], 
                               y = names(fit$model)[1])) +
    labs(title=title, subtitle = gtext) + 
      geom_point() +
        stat_smooth(method = "lm", col = "red") +
          theme(plot.title = element_text(size = rel(.8))) 
}

## my style ggplot2 theme
theme_jhp <- function (base_size = 10, base_family = "sans") 
{
    colors <- tibble::deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])
    (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(line = element_line(colour = "black"),
           rect = element_rect(fill = colors["Light Gray"], 
                               linetype = 0, colour = NA),
           text = element_text(colour = colors["Dark Gray"]), 
           ## axis.title = element_blank(), axis.text = element_text(), 
           axis.ticks = element_blank(), axis.line = element_blank(), 
           legend.background = element_rect(), legend.position = "bottom", 
           legend.direction = "horizontal", legend.box = "vertical", 
           panel.grid = element_line(colour = NULL), 
           panel.grid.major = element_line(colour = colors["Medium Gray"]), 
           panel.grid.minor = element_blank(),
           plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
           plot.margin = unit(c(1, 1, 1, 1), "lines"),
           strip.background = element_rect()))
}
