#qua faremo lo script e dopo nel markdown faremo vedere gli output dell'analisi
#ovviamente senza mostrare lo script `echo = FALSE`
#install.packages("Rmixmod")
library(tidyverse)
library(mclust)
library(Rmixmod)
library(GGally)
# Fetal health ------------------------------------------------------------

# data exploration  -------------------------------------------------------

#TO do :
# Usare grafici migliore per scartare variabili 
# fare un buon gg paris v
# PCA v

# #metto che si vedono tutte le variabili:
# options(tibble.width=Inf) no PLS
#capiamo meglio come mai nelle analisi dobbiamo escludere (o capire come incorporare) 
#le variabili con hist e severe decelleration

#bisogna strutturare meglio la analisi iniziale 
fetal_Health <- tibble(read.csv("fetal_health.csv")) %>%
  mutate_at(vars(fetal_health),as.factor) # non è elegante da migliorare

sum(fetal_Health$severe_decelerations != 0)# not normaly distributed most values are 0 
hist(fetal_Health$histogram_number_of_zeroes) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_number_of_peaks) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_variance) # questo sembra distribuirsi come gamma
fetal_Health$histogram_tendency # questo è un factor
fetal_Health$percentage_of_time_with_abnormal_long_term_variability # non è distribuito come normale

hist(fetal_Health$accelerations )
hist(fetal_Health$fetal_movement)
hist(fetal_Health$prolongued_decelerations)

#variabile sospetta fetal_Health$mean_value_of_long_term_variability, fetal_Health$mean_value_of_short_term_variability
#hanno un lower bound di 0 

fetal_Health <- fetal_Health %>%
  select(-c(severe_decelerations,
            histogram_number_of_zeroes,
            histogram_number_of_peaks,
            histogram_variance,
            histogram_tendency, 
            percentage_of_time_with_abnormal_long_term_variability, 
            accelerations, 
            fetal_movement,
            prolongued_decelerations))
etichette<-fetal_Health$fetal_health


fetal_Health %>%
ggplot(aes(x=fetal_health,
                y= ..count../sum(..count..))) + 
  geom_bar(aes(fill = fetal_health), color="black") +         
  labs(x="Condizione del feto", y="Frequenza Relativa", title="Salute del feto")+
  scale_x_discrete(labels = c('Normale','Sospetto','Patologico')) 


fetal_Health%>% 
  select(-fetal_health)%>%
  ggpairs(mapping = aes(color = fetal_Health$fetal_health))
# nome variabili distinte
#histogram_mean and median (molto correlate) e mode


# commento che si può fare è che evidente anche in casi unidensionali si può vedere che le variabili (anche se distinte per fetal health)
#SONO MULTIMODALI (perforza andrà usato un MDA)

#alcune variabili sono già evidenti tipo le ultime 3 in basso che una delle categorie siu distingue benone

#SEGNARE ULTIME 3 VARIABILI DEL ggpairs

#ha senso vedere se ci sono outliers o cose del genere nelle variabili selezionate???

#qualche outliers......
boxplot(fetal_Health[,-c(2,3,5,6,13)])
boxplot(fetal_Health[,6])
boxplot(fetal_Health[,c(2,3)])
boxplot(fetal_Health[,5])
boxplot(fetal_Health[,13])
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
pca$loadings

#è necessario standardizzare siccome le variabili sono su ordini di grandezza differenti
#altrimenti l'80% della variabilità sarebbe gestita unicamente da una variabile (variabile o componente?????? RIVEDERE)
k <- ncol(fetal_Health)
pca$sdev 
cumsum(pca$sdev^2/k) < 0.80 #4 componenti

#selezioniamo le prime d variabili che sono normali e unite coprono il 60 o 70 % della variabilità
(main_comp <- names(fetal_Health)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))])
# da fare meglio

fetal_Health%>% 
  select(all_of(main_comp))%>%
  ggpairs(mapping = aes(color = fetal_Health$fetal_health))


# classification ----------------------------------------------------------
# da fare  
# normal V
# con MDA X
# con oversampling X

#normal

#scatterplot con le prime 2 variabili più significative a seguito della pca
fetal_Health %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point()
  #strano che i dubbiosi si collochino a destra dei sani e non tra sani e malati


(fetal_Health <- fetal_Health %>% 
    rowid_to_column("id"))

set.seed(123)

train <- fetal_Health %>% 
  sample_frac(.70) # prendo il .70 percento e lo uso come training set 

data_train <- train %>% # tenere train NON fetal_Health
  #select(-c(id, fetal_health)) %>%# rimuovo variabili problematiche e i labels
  select(all_of(main_comp))
  
label_train <- train %>%
  select(fetal_health)   #  prendo i labels


test<- anti_join(fetal_Health, train, by = 'id')%>%
#  select(-c(id)) # estraggo le osservazioni non presenti nel data set train 
# e le uso come test set
  select(all_of(main_comp),fetal_health)


#dobbiamo provare altri modelli ovviamente 
(pr<-mixmodLearn(data_train, c(label_train$fetal_health),
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè

#prediction con il nostro classifier sembra accurate al 80 percento
#bisogna capire meglio le variabili che iniziano per "histogram" che sembrano non entrare nel classifi
summary(pr) #ma quindi error rate MAP=0% significa che se eseguiamo sul train set stesso la classificazione è perfetta???? è razionale
str(pr)

PREDICTION<- mixmodPredict(data = select(test,-fetal_health), classificationRule=pr["bestResult"])
str(PREDICTION)
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè


mean(PREDICTION@partition == test$fetal_health) # bisogna andare a vedere la specificity dei malati 3
PREDICTION@proba[1:30,] #se no ci mette anni a plottare tutto (PREDICTION@partition non ci interessa visualizzarlo)

#c'è un modo migliore di fare la confusion matrix? 
confusion_matrix <- table(test$fetal_health, PREDICTION@partition)  #non prendiamo bene gli ammalati molto male
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)) # confirmed 81% confirmed

# con MDA X
#CLASSIFICAZIONE SECONDO MDA

set.seed(869)
mod2 <- MclustDA(data_train, label_train$fetal_health) #, modelNames = "VVV", G = 4
#bisogna capire come mai non mi specifica i modelli se non metto niente in model names e
#se lascio g senza niente (perchè devo specificare che modello e quanti g non dovrebbe farlo da solo????)
# Ok era semplicemente perchè gli davamo da stimare troppi parametri 
summary(mod2)
str(mod2) 
predict(mod2, select(test,-fetal_health))$class #  questo sono le prediction del MDA
mean(c(predict(mod2, select(test,-fetal_health))$class) == pull(test,fetal_health)) #pull estrae un vettore da un db
# a quanto pare riesce a predirre un 83 % 
#quindi bisogna fare oversampling


# Ho messo nei commenti tutto il codice spaghetti se ti serve qualcosa lo lascio ma mi sembra tutto da canellare

# #creo un dataset come bozza x fare la classificazione (DA RIVEDERE LE VARIABILI DA USARE)
# (fetal_Health_bozza<-fetal_Health%>%
#     select(-c(starts_with("histogram"), severe_decelerations, fetal_health))) #se ricordo bene l'mda coglie 
# 
# 
# #le misture tra diverse variabili..non all'interno di una singola variabile che quindi viene richietsa normale
# #DA VERIFICARE
# etichette  
# 
# 
# #suddivisione train test
# set.seed(123)
# (index<-sample(c(T,F),size=n,replace=T,prob=c(0.7,0.3)))
# prop.table(table(index)) #ci sta
# 
# #stima modello migliore + calcolo MER sul test set (STO SCHIFPO NON FUNZIONA)
# 
# # Lo script per dividere in traning e test è già scritto!
# 
# 
# # set.seed(123)
# # (fetal_Health_bozza_train<-fetal_Health_EM[index,])
# # (etichette_train<-fetal_Health[index,"fetal_health"])
# # (fetal_Health_bozza_test<-fetal_Health_EM[ifelse(index==F,T,F),])
# # (etichette_test<-fetal_Health[ifelse(index==F,T,F),"fetal_health"])
# mod = MclustDA(data=data.frame(fetal_Health_bozza_train), as.factor(etichette_train)) # sul primo si è soffermato sul 20%
# sum(predict(mod, fetal_Health_bozza_test)$class != etichette_test)
# class(data.frame(fetal_Health_bozza_train))
# 
# #per il futuro un eventuale under/over sampling ha senso (probabilmente più under sampling dalla
# #categoria sano che è maggiormente presente rispetto alle altre 2)
# table(fetal_Health$fetal_health)
# # 1655 295 176
# 
# 
# # NON HA SENSO FARE CON UN 20 PERCENTO FISSO IL MER A QUESTO PUNTO FAI SOLO IL CV
# # CHE è PIU' UTILIZZATO E HA MOLTO PIù SENSO 
# # BISOGNEREBBE PERò VEDERE L'OVERSAMPLING COME FUNZIONA
# 
# # ha senso provare ad implementare un MDA basato sul MER con il quale:
# # 60% training
# # 20% selection (basata sul MER o sul BIC....col MER tocca implementarla)
# # 20% testing
# # con cui poi fare un cross-validation ripetendo 10 volte la selezione dei 3 sets e valutare la media dei MER
# # per selezionare il miglior modello
# 
# #provare una funzione che prende g1 g2 e g3 e mod1 mod2 e mod3 e restituisce un MER (facendo la media 
# #con 10 cross-validation)
# #si può implementare con un apply ma devo creare una matrice
# # con 6 variabili: g1 (numero gruppi nella mistura della prima etichetta) g2 g3 mod1 (tipo modello prima etichetta)
# #mod2 e mod3
# #rischio: che per ogni gruppo il più accurato sia sempre VVV con 5 cluster non avcendo penalizzazione se uso il MER invece
# #del BIC (o magari VVV con 5 gruppi va in overfitting e quindi su dati nuovi (del selection set) non worka)
# 
# #ci penso lunedì

# over sampling

#install.packages( "https://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
#install.packages("grid")
#install.packages("DMwR")

library(lattice)
library(grid)
library(DMwR)

train2<-train %>%
  select(all_of(main_comp), fetal_health) %>% 
  as.data.frame()
table(train2$fetal_health)

levels(train2$fetal_health) <- c(1,1,3) # ho messo tutti della classe 2 nella classe 1

table(train2$fetal_health)


new_train <- SMOTE(fetal_health ~ ., train2, perc.over= 200, perc.under = 500)
# + perc.over/100 % is the number of cases generated (in questo caso 1/3 sono reali)


#  if 200 new examples were generated for the minority class, a value of perc.under of 100 will randomly select exactly 
#  200 cases belonging to the majority classes from the original data set to belong to the final data set. Values above 100 will select more examples from the majority classes.
# in questo caso prendiamo (+ perc.over/100 %) * ncasi * 5 di casi sani 

table(new_train$fetal_health)

mod3 <- MclustDA(new_train[,-5], new_train$fetal_health)



summary(mod3)
str(mod3) 
predicted_labels <- predict(mod3, select(test,-fetal_health))$class #  questo sono le prediction del MDA
real_labels <- pull(test,fetal_health) #etichette vere
levels(real_labels) <- c(1,1,3)
real_labels
mean(predicted_labels == real_labels) #pull estrae un vettore da un db

table(predicted_labels,real_labels ) # vediamo che sbagliamo in maniera simile ma riusciamo 
#a non perderci i "falsi sani" 

# clustering --------------------------------------------------------------

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
plot(health_mclust_ICL,ylim=c(-35000,-25000))  #guardate che bellino
str(health_mclust_ICL)


#per curiosità:
set.seed(123)
plot(mclustICL(fetal_Health_EM,G=2:15,ylim=c(-34000,-26000))) #non risulta per nulla un k predominante
#gran parte dei modelli in base all'ICL risultano della stessa precisione qualunque sia
#il numero di gruppi

set.seed(123)
health_mclust_BIC<-Mclust(fetal_Health_EM) 
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

#necessario provare con k=3 (SENZA set.seed VIENE DIVERSISSIMO) >>>escono precisioni che variano dal 60% all'80%
#significa che è un EM molto variabile/instabile e poco robusto (tutte caratteristiche che rendono cruciale la scelta dei valori iniziali)
set.seed(123)
health_mclust_ICL_k3<-mclustICL(fetal_Health_EM,G=3)
summary(health_mclust_ICL_k3) #EVV
set.seed(123)
health_mclust_BIC_k3<-Mclust(fetal_Health_EM,G=3)
summary(health_mclust_BIC_k3) #EVV

#megliobasarsi sull'ICL ma in questo caso si popssono prendere le etichette dal BIC senza problemi perchè i 2 modelli coincidono


(etichette<-fetal_Health$fetal_health)
(etichette_stimate<-health_mclust_BIC_k3$classification)


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
# IPOTETICA CLASSIFICAZIONE SOLO SANI E MALATI E POI VEDIAMO COSA FA STO MODELLO
#SUI DUBBIOSI (DA VEDERE IN FUTURO) >>>>MODELLO MDA CON 2 CLUSTER
#BASATO SU BIC O SU MER E POI OUTPUT CON LA SUDDIVISIONE DEI DUBBIOSI IN MALATI E NON
#per selezionare il miglior modello con 2 gruppi:
#tra tutti i possibili facciamo cv ripetuta 10 volte e poi ristimiamo il modello
#selezioanto come migliore (avente mer più basso) su tutti i dati che abbiamo di 
#malati e sani>>>>stimato questo modello poi classifichiamo i dubbiosi e vediamo
#se effettivamente finiscono nei sani (DA GIUSTIFICARE CHE SONO SOLO ASSUNZIONI
#BASATE SUI DATI E NON SUPERANO QUELLE DI UN MEDICO: i dati sembrano molto più simili ai sani
#ma sono anche diversi/anomali e non ancora classsificati dal pto di vista scientifico)
#DA RIVEDERE



#MDA con MER e suddivisione train evaluation e test

accuracy<-function(g,mod,nCV=5,data,etichette){
  set.seed(123)
  mod_mda<-MclustDA(data,class=etichette,G=as.list(g),modelName=mod)
  return(1-cvMclustDA(mod_mda,nfold=nCV)$ce)
}


g1<-g2<-g3<-c(1,2,3,4)
g1<-as.data.frame(g1)
g2<-as.data.frame(g2)
g3<-as.data.frame(g3)

join<-cross_join(cross_join(g1,g2),g3)
join["mod"]<-"VII" 
#altrimenti con più modelli il codice impegherebbe troppo tempo
#usiamo come alternativa il modello VII  che sono delle ipersfere del quale varia solo il volume
dim(join)
join

#CI METTE QUALCHE MINUTINO
output<-apply(join,MARGIN=1,function(pos) accuracy(g=pos[1:3],mod=pos[4],nCV=4,data=fetal_Health,etichette=etichette))



(lis<-list(modello=join[which.max(output),],accuracy=output[which.max(output)]))



modello_MDA_k3<-function(data,etichette){
  g1<-g2<-g3<-c(1,2,3,4)
  g1<-as.data.frame(g1)
  g2<-as.data.frame(g2)
  g3<-as.data.frame(g3)
  join<-cross_join(cross_join(g1,g2),g3)
  join["mod"]<-"VII" 
  out<-apply(join,MARGIN=1,function(pos) accuracy(g=pos[1:3],mod=pos[4],nCV=4,data=data,etichette=etichette))
  lis<-list(modello=join[which.max(out)],accuracy=out[which.max(out)])
  return(lis)
}
modello_MDA_k3(fetal_Health,etichette)
#la valutazione del test set deve essere fatta attraverso un dataset che non ha partecipato
#alla costruzione del modello


