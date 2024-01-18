#qua faremo lo script e dopo nel markdown faremo vedere gli output dell'analisi
#ovviamente senza mostrare lo script `echo = FALSE`
library(tidyverse)
library(mclust)
library(Rmixmod)

X <-read.csv("credit_score.csv")
#leggendo il dataset sembra essere generato randomicamente
dataset <- tibble(X)
X$credit_levels <- cut(X$CREDIT_SCORE, 5)
levels(X$credit_levels) <- c("vlow", "low", "medium", "high", "vhigh")

X$credit_levels


sample_frac()

creditors_data <- sample_frac(X, 0.8) %>% 
  select(SAVINGS,DEBT,INCOME)


set.seed(321)
(pr<-mixmodLearn(creditors_data, X$credit_levels,    #all three variables in diabetes.data
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  



pr@bestResult
