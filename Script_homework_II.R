#qua faremo lo script e dopo nel markdown faremo vedere gli output dell'analisi
#ovviamente senza mostrare lo script `echo = FALSE`
#install.packages("Rmixmod")
library(tidyverse)
library(mclust)
library(Rmixmod)

# credit ------------------------------------------------------------------


X <-read.csv("credit_score.csv")
X
#leggendo il dataset sembra essere generato randomicamente
dataset <- tibble(X)
X$credit_levels <- cut(X$CREDIT_SCORE, 5)
levels(X$credit_levels) <- c("vlow", "low", "medium", "high", "vhigh")

X$credit_levels


#sample_frac()

creditors_data <- X %>% 
  sample_frac(0.8) %>%
  select(CUST_ID,SAVINGS,DEBT,INCOME,labels = credit_levels)
# samples random data

D_training <- X %>%
  filter(CUST_ID %in% creditors_data$CUST_ID) %>% 
  select(SAVINGS,DEBT,INCOME)  # 
# Strips of the labels

D_label <- X %>%
  filter(CUST_ID %in% creditors_data$CUST_ID) %>% 
  select(labels = credit_levels) %>%
  as_vector()
# Is the labels
  
Test <- X %>%
  filter(!(CUST_ID %in% creditors_data$CUST_ID)) %>% 
  select(SAVINGS,DEBT,INCOME,labels = credit_levels)
# Data to test on 

set.seed(321)
(pr<-mixmodLearn(D_training, D_label,
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  


pr@bestResult


#PREDICTION<- mixmodPredict() da implementare correttamente

# Fetal health ------------------------------------------------------------

fetal_health <- tibble(read.csv("fetal_health.csv"))

sum(is.na(fetal_health)) #no NAs
n <- nrow(fetal_health)
fetal_health <- fetal_health %>% 
  rowid_to_column("id")

train <- fetal_health %>% 
  sample_frac(.70)

data_train <- fetal_health %>% 
  select(-c(id, fetal_health, starts_with("histogram"), severe_decelerations))

label_train <- fetal_health %>%
  select(fetal_health) %>%
  mutate_at(vars(fetal_health),as.factor) # non è elegante da migliorare

test<- anti_join(fetal_health, train, by = 'id')%>%
  select(-c(id, starts_with("histogram"), severe_decelerations))

  

(pr<-mixmodLearn(data_train, label_train$fetal_health,
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  

#prediction con il nostro classifier sembra accurate al 86 percento
#bisogna capire meglio le variabili che iniziano per "histogram" che sembrano non entrare nel classifi
summary(pr)
str(pr)

PREDICTION<- mixmodPredict(data = select(test,-fetal_health), classificationRule=pr["bestResult"])
str(PREDICTION)

mean(PREDICTION@partition == test$fetal_health) # bisogna andare a vedere la specificity dei malati 3
PREDICTION

confusion_matrix <- table( test$fetal_health, PREDICTION@partition)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) # confirmed 86%


#analisi delle componenti principali
pca = princomp(data_train,cor=T)
pca$sdev 
cumsum(pca$sdev^2/10) < 0.60

names(data_train)[apply(pca$loadings[,1:3], 2, function(x) which(x**2==max(x**2)))]
# da fare meglio




