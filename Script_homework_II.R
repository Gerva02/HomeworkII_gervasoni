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

sum(fetal_Health$severe_decelerations != 0)# not normaly distributed most values are 0 (cos'è sta cosa????)
hist(fetal_Health$histogram_number_of_zeroes) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_number_of_peaks) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_variance) # questo sembra distribuirsi come gamma
fetal_Health$histogram_tendency # questo è un factor
fetal_Health$percentage_of_time_with_abnormal_long_term_variability # non è distribuito come normale



#variabile sospetta fetal_Health$mean_value_of_long_term_variability, fetal_Health$mean_value_of_short_term_variability
#hanno un lower bound di 0 

fetal_Health <- fetal_Health %>%
  select(-c(severe_decelerations,
            histogram_number_of_zeroes,
            histogram_number_of_peaks,
            histogram_variance,
            histogram_tendency, 
            percentage_of_time_with_abnormal_long_term_variability))


fetal_Health%>% 
  ggpairs(mapping = aes(color = fetal_health))


#PRIMA DI FARE ALTRO BISOGNA CAPIRE CHE VARIABILI TENERE E CHE VARIABILI SCARTARE
#ho tolto queste variabili tutte se ne volete togliere altre fatelo UNA volta da qui


#se escludiamo tutte queste come vediamo dal grafico non va bene però ALCUNE D
#di queste sono più problematiche, bisogna vedere 1 ad 1 
#sembrano essere severe deceleration histogram number of zeros e histogram variance
# mentre le altre potrebbero essere reinserite

sum(is.na(fetal_Health)) #no NAs
n <- nrow(fetal_Health)

#analisi delle componenti principali
pca <- fetal_Health%>%
  select(-fetal_health)%>%
  princomp(cor=T) 


#è necessario standardizzare siccome le variabili sono su ordini di grandezza differenti
#altrimenti l'80% della variabilità sarebbe gestita unicamente da una variabile (variabile o componente?????? RIVEDERE)
k <- ncol(fetal_Health)
pca$sdev 
cumsum(pca$sdev^2/k) < 0.70 #4 componenti

#selezioniamo le prime d variabili che sono normali e unite coprono il 60 o 70 % della variabilità
(main_comp <- names(fetal_Health)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))])
# da fare meglio


#scatterplot con le prime 2 variabili più significative a seguito della pca
fetal_Health %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point()


(fetal_Health <- fetal_Health %>% 
  rowid_to_column("id"))

set.seed(123)

train <- fetal_Health %>% 
  sample_frac(.70) # prendo il .70 percento e lo uso come training set 

data_train <- fetal_Health %>% 
  select(-c(id, fetal_health)) # rimuovo variabili problematiche e i labels

label_train <- fetal_Health %>%
  select(fetal_health)   #  prendo i labels
  

test<- anti_join(fetal_Health, train, by = 'id')%>%
  select(-c(id)) # estraggo le osservazioni non presenti nel data set train 
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

set.seed(123) #col set.seed stima con k=3 (siccome facilmente sbaglia....probabilmente la scelta degli initial values è cruciale...
#da specificare nell'homework)
health_mclust_ICL<-mclustICL(fetal_Health_EM) #non ha fatto alcun salto (si può ipoptizzare che il numero di u.s. sia sufficiente
#a stimare anche il modello più complkesso VVV anche con 9 gruppi....diu default mclust stima da 1 a 9 gruppi per i 14 modelli possibili)
summary(health_mclust_ICL) #modello VEV con 3 componenti 
plot(health_mclust_ICL,ylim=c(-20000,20000))  #guardate che bellino (si vede che il modello EEV con più cluster sta prendendo una buona risalita)
str(health_mclust_ICL)

#per curiosità:
set.seed(123)
plot(mclustICL(fetal_Health_EM,G=2:21),ylim=c(0,50000)) #dopo k=18 sembra scendere quindi il modello VEV con 3 componenti è il migliore ma ci va bene solo con
#accurati starting values (probabilmente 3 gruppi è raro che li colga accuratamente...motivazione: DA COMPLETARE)
#qui si vede la difficoltà dell'EM probabilmente con gruppi molto vicini tra loro (DA ARGOMENTARE)
#nessun salto nel fitting dei modelli (le u.s. sono sufficienti anche per k molto grande (con 4 variabili i parametri non sono eccessivi))
#CURSE OF DIMENSION
#da verificare questa cosa che stima tutti i modelli....


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

(confusion_matrix <- table( etichette, etichette_stimate))

#sbagliamo quasi tutti i malati e i dubbiosi 
#i 3 gruppi che sembra indovinare sono sbagliati


#questo è diventato tutto falso?

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

#CLASSIFICAZIONE SECONDO MDA
#creo un dataset come bozza x fare la classificazione (DA RIVEDERE LE VARIABILI DA USARE)
(fetal_Health_bozza<-fetal_Health%>%
  select(-c(starts_with("histogram"), severe_decelerations, fetal_health))) #se ricordo bene l'mda coglie 
#le misture tra diverse variabili..non all'interno di una singola variabile che quindi viene richietsa normale
#DA VERIFICARE
etichette  


#suddivisione train test
set.seed(123)
(index<-sample(c(T,F),size=n,replace=T,prob=c(0.7,0.3)))
prop.table(table(index)) #ci sta

#stima modello migliore + calcolo MER sul test set (STO SCHIFPO NON FUNZIONA)
set.seed(123)
(fetal_Health_bozza_train<-fetal_Health_EM[index,])
(etichette_train<-fetal_Health[index,"fetal_health"])
(fetal_Health_bozza_test<-fetal_Health_EM[ifelse(index==F,T,F),])
(etichette_test<-fetal_Health[ifelse(index==F,T,F),"fetal_health"])
mod = MclustDA(data=data.frame(fetal_Health_bozza_train), as.factor(etichette_train)) # sul primo si è soffermato sul 20%
sum(predict(mod, fetal_Health_bozza_test)$class != etichette_test)
class(data.frame(fetal_Health_bozza_train))

#per il futuro un eventuale under/over sampling ha senso (probabilmente più under sampling dalla
#categoria sano che è maggiormente presente rispetto alle altre 2)
table(fetal_Health$fetal_health)
# 1655 295 176

#ha senso provare ad implementare un MDA basato sul MER con il quale:
#60% training
#20% selection (basata sul MER o sul BIC....col MER tocca implementarla)
#20% testing 
#con cui poi fare un cross-validation ripetendo 10 volte la selezione dei 3 sets e valutare la media dei MER 
#per selezionare il miglior modello

#provare una funzione che prende g1 g2 e g3 e mod1 mod2 e mod3 e restituisce un MER (facendo la media 
#con 10 cross-validation)
#si può implementare con un apply ma devo creare una matrice
# con 6 variabili: g1 (numero gruppi nella mistura della prima etichetta) g2 g3 mod1 (tipo modello prima etichetta)
#mod2 e mod3
#rischio: che per ogni gruppo il più accurato sia sempre VVV con 5 cluster non avcendo penalizzazione se uso il MER invece
#del BIC (o magari VVV con 5 gruppi va in overfitting e quindi su dati nuovi (del selection set) non worka)

#ci penso lunedì

#tocca  cambiare un po' i nomi se no palese che abbiamo copiato e incollato dallo script...