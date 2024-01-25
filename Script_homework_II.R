#qua faremo lo script e dopo nel markdown faremo vedere gli output dell'analisi
#ovviamente senza mostrare lo script `echo = FALSE`
#install.packages("Rmixmod")
library(tidyverse)
library(mclust)
library(Rmixmod)
library(GGally)
# Fetal health ------------------------------------------------------------

#capiamo meglio come mai nelle analisi dobbiamo escludere (o capire come incorporare) 
#le variabili con hist e severe decelleration

#bisogna strutturare meglio la analisi iniziale 
fetal_Health <- tibble(read.csv("fetal_health.csv")) %>%
  mutate_at(vars(fetal_health),as.factor) # non è elegante da migliorare

sum(fetal_Health$severe_decelerations != 0)# not normaly distributed most values are 0
fetal_Health%>% 
  select(c(starts_with("histogram"), severe_decelerations, fetal_health))%>%
  ggpairs(mapping = aes(color = fetal_health))
#se escludiamo tutte queste come vediamo dal grafico non va bene però ALCUNE D
#di queste sono più problematiche, bisogna vedere 1 ad 1 
#sembrano essere severe deceleration histogram number of zeros e histogram variance
# mentre le altre potrebbero essere reinserite

sum(is.na(fetal_Health)) #no NAs
n <- nrow(fetal_Health)

#analisi delle componenti principali
pca <- fetal_Health%>% 
  select(-c(fetal_health, starts_with("histogram"), severe_decelerations))%>%
  princomp(cor=T) 
#è necessario standardizzare siccome le variabili sono su ordini di grandezza differenti
#altrimenti l'80% della variabilità sarebbe gestita unicamente da una variabile (variabile o componente?????? RIVEDERE)
pca$sdev 
cumsum(pca$sdev^2/10) < 0.70 #4 componenti

names(fetal_Health)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))]
# da fare meglio

fetal_Health %>%
  select(abnormal_short_term_variability, percentage_of_time_with_abnormal_long_term_variability, fetal_health) %>%
  ggplot(mapping = aes(x=abnormal_short_term_variability , y =percentage_of_time_with_abnormal_long_term_variability, color = fetal_health)) +
  geom_point()

fetal_Health%>% 
  select(-c(starts_with("histogram"), severe_decelerations))%>%
  ggpairs(mapping = aes(color = fetal_health))



(fetal_Health <- fetal_Health %>% 
  rowid_to_column("id"))

set.seed(123)

train <- fetal_Health %>% 
  sample_frac(.70)

data_train <- fetal_Health %>% 
  select(-c(id, fetal_health, starts_with("histogram"), severe_decelerations))

label_train <- fetal_Health %>%
  select(fetal_health)
  

test<- anti_join(fetal_Health, train, by = 'id')%>%
  select(-c(id, starts_with("histogram"), severe_decelerations))

  
#dobbiamo provare altri modelli ovviamente 
(pr<-mixmodLearn(data_train, label_train$fetal_health,
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  

#prediction con il nostro classifier sembra accurate al 85 percento
#bisogna capire meglio le variabili che iniziano per "histogram" che sembrano non entrare nel classifi
summary(pr)
str(pr)

PREDICTION<- mixmodPredict(data = select(test,-fetal_health), classificationRule=pr["bestResult"])
str(PREDICTION)

mean(PREDICTION@partition == test$fetal_health) # bisogna andare a vedere la specificity dei malati 3
PREDICTION

#c'è un modo migliore di fare la confusion matrix? 
confusion_matrix <- table( test$fetal_health, PREDICTION@partition)
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)) # confirmed 85% 




#conviene provare a styimare la classificazine sia tramite 10 variabili
#sia tramite le 4 slezionate (magari per far vedere che con 10 variabili alcuni modelli
#non li stima)
