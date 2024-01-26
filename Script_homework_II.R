#qua faremo lo script e dopo nel markdown faremo vedere gli output dell'analisi
#ovviamente senza mostrare lo script `echo = FALSE`
#install.packages("Rmixmod")
library(tidyverse)
library(mclust)
library(Rmixmod)
library(GGally)
# Fetal health ------------------------------------------------------------

#metto che si vedono tutte le variabili:
options(tibble.width=Inf)

#capiamo meglio come mai nelle analisi dobbiamo escludere (o capire come incorporare) 
#le variabili con hist e severe decelleration

#bisogna strutturare meglio la analisi iniziale 
fetal_Health <- tibble(read.csv("fetal_health.csv")) %>%
  mutate_at(vars(fetal_health),as.factor) # non è elegante da migliorare

sum(fetal_Health$severe_decelerations != 0)# not normaly distributed most values are 0
hist(fetal_Health$histogram_number_of_zeroes) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_number_of_peaks) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_variance) # questo sembra distribuirsi come gamma
fetal_Health$histogram_tendency
fetal_Health$percentage_of_time_with_abnormal_long_term_variability # non è distribuito come normale

#PRIMA DI FARE ALTRO BISOGNA CAPIRE CHE VARIABILI TENERE E CHE VARIABILI SCARTARE

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

main_comp <- names(fetal_Health)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))]
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
  sample_frac(.70) # prendo il .70 percento e lo uso come training set 

data_train <- fetal_Health %>% 
  select(-c(id, fetal_health, starts_with("histogram"), severe_decelerations)) # rimuovo variabili problematiche e i labels

label_train <- fetal_Health %>%
  select(fetal_health)   #  prendo i labels
  

test<- anti_join(fetal_Health, train, by = 'id')%>%
  select(-c(id, starts_with("histogram"), severe_decelerations)) # estraggo le osservazioni non presenti nel data set train 
# e le uso come test set

  
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
#non li stima)>>>>>>idem provare a fare un EM basato su tutte le variabili o quanto meno non solo sulle 4 selezionatre tramite pca


#IMPLEMENTO UN EM DI NORMALI SENZA SPECIFICARE IL NUMERO DI GRUPPI
#install.packages("mclust")
library(mclust)
(fetal_Health_EM<-fetal_Health%>%
  select(all_of(main_comp))) #dataset solo con le variabili selezionate tramite pca

health_mclust_ICL<-mclustICL(fetal_Health_EM) #non ha fatto alcun salto (si può ipoptizzare che il numero di u.s. sia sufficiente
#a stimare anche il modello più complkesso VVV anche con 9 gruppi....diu default mclust stima da 1 a 9 gruppi per i 14 modelli possibili)
summary(health_mclust_ICL) #modello VEV con 3 componenti 
plot(health_mclust_ICL)  #guardate che bellino 
str(health_mclust_ICL)


set.seed(123)
health_mclust_BIC<-Mclust(fetal_Health_EM) # qua bisogna settare seed a volte viene 3 a volte 8
summary(health_mclust_BIC)

#SIA TRAMITE ICL SIA TRAMITE BIC IL MODEL BASED CLUSTERING FORNISCE UN NUMERO DI GRUPPI DIFFERENTE....PROVIAMO A SPECIFICARE IL NUMERO DI GRUPPI:
# 
# health_mclust_ICL_k3<-mclustICL(fetal_Health_EM,G=3)
# ?mclustICL
# summary(health_mclust_ICL_k3) #EEV (anche con 2000 di entropia di distanza da EEI....significa molto più accurato degli altri)
# 
# health_mclust_BIC_k3<-Mclust(fetal_Health_EM,G=3)
# summary(health_mclust_BIC_k3) #sempre EEV
# 
# #confronto dei vari modelli
# health_EM_3gruppi<-Mclust(fetal_Health_EM,model="VEV",G=3)
#(etichette<-fetal_Health$fetal_health)
#(etichette_stimate<-health_EM_3gruppi$classification)
# #confronto classificazione
#questo lo commentato tutto perchè non penso abbia più ragione di esistere 



(etichette<-fetal_Health$fetal_health)
(etichette_stimate<-health_mclust_BIC$classification)


precisione_EM<-classError(etichette_stimate, class=etichette)
1-precisione_EM$errorRate   #perchè si è abbasato ?
#l'EM sapendo del numero di gruppi risulta preciso al 81.28% (poco più di 4 su 5)
#Ma il vero problkema risulta nella scelta del numero di cluster che in assenza delle etichette
#risulta spesso maggiore di 3 (6 o 7 a seconda dell'indice utilizzato)
#probabilmente i cluster non sono ben definiti nemmeno dalle etichette note....stando al risultato
#dell'algoritmo è possibile che alcuni di essi siano ulteriormente scomponibili in altri
#sotto gruppi
#come capire quale delle etichette è balorda:
#valutare nelleclassificazione se alcuni gruppi vengono visti come misture....
#possibile soluzione alla presenza di sotto gruppi (inoltre una analisis accurata di sentitivity e
#specificity per valutare se una etichetta in particolare è soggetta ad errori)
#anche la differenza sostanziale tra le numerosità nei gruppi può essere una motivazione
#della scarsa precisione dell'algoritmo EM (risolvibile tramite under sampling o con le cavolate di Gervi????)




#poi in futuro voi geni se volete mettere le variabili in INGLESE è più bello
