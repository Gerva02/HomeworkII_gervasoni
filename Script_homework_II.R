#qua faremo lo script e dopo nel markdown faremo vedere gli output dell'analisi
#ovviamente senza mostrare lo script `echo = FALSE`
#install.packages("Rmixmod")
#install.packages("caret")
rm(list=ls())
library(tidyverse)
library(mclust)
library(Rmixmod)
library(GGally)
library(caret)

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

levels(fetal_Health$fetal_health) <- c("Normale","Sospetto","Patologico")

#istogrammi da non includere nel markdown ma da riportare le osservazioni/motivazioni per cui non sono utilizzate
sum(fetal_Health$severe_decelerations != 0)# not normaly distributed most values are 0 
hist(fetal_Health$histogram_number_of_zeroes) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_number_of_peaks) # è un conteggio dunque probabilmente si distribuisce come poisson
hist(fetal_Health$histogram_variance) # questo sembra distribuirsi come gamma
fetal_Health$histogram_tendency # questo è un factor (-1/0/1)
fetal_Health$percentage_of_time_with_abnormal_long_term_variability # non è distribuito come normale

hist(fetal_Health$accelerations ) #fortemente asimmetrica
hist(fetal_Health$fetal_movement) #fortemente asimmetrica
hist(fetal_Health$prolongued_decelerations) #fortemente asimmetrica

#variabile sospetta fetal_Health$mean_value_of_long_term_variability, fetal_Health$mean_value_of_short_term_variability
#hanno un lower bound di 0 >>>>>le possiamo accettare

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

(etichette<-fetal_Health$fetal_health)



fetal_Health %>%
ggplot(aes(x=fetal_health,
                y= after_stat(count)/sum(after_stat(count)))) + 
  geom_bar(aes(fill = fetal_health), color="black") +         
  labs(x="Condizione del feto", y="Frequenza Relativa", title="Salute del feto") #dati sbilanciati nelle frequenze relative:
#grande maggioranza di persone sane


fetal_Health%>% 
  select(-fetal_health)%>%
  ggpairs(mapping = aes(color = fetal_Health$fetal_health))
# nome variabili distinte
#histogram_mean, histogram_median e histogram_mode sono (ovviamente) fortemente correlate


# commento che si può fare: è evidente (anche in casi unidensionali) che le variabili (anche se distinte per fetal health)
#SONO MULTIMODALI (perforza andrà usato un MDA)


#essendo dati presi da macchine è improbabile la presenza di outliers dovuto a "data entry"



sum(is.na(fetal_Health)) #no NAs
n <- nrow(fetal_Health)

#analisi delle componenti principali: 
pca <- fetal_Health%>%
  select(-fetal_health)%>%
  princomp(cor=T) 


#è necessario standardizzare usando "cor=T" siccome le variabili sono su ordini di grandezza differenti
#altrimenti l'80% della variabilità sarebbe coperta unicamente da una variabile 

k <- ncol(fetal_Health)
pca$sdev 
cumsum(pca$sdev^2/k) < 0.80 #4 componenti per coprire l'80% della variabilità 


#selezioniamo le prime 4 variabili in base ai loadings (quanto peso ha la singola variabile all'interno della componente principale)
(load_vars <- names(fetal_Health)[apply(pca$loadings[,1:4], 2, function(x) which(x**2==max(x**2)))])


fetal_Health%>% 
  select(all_of(load_vars))%>%
  ggpairs(mapping = aes(color = fetal_Health$fetal_health)) #si può notare che
#le ultime 2 variabili sezionate (mean_value_of_long_term_variability e uterine_contractions)
#non rispettano la normalità e la simmetria in tutti i gruppi

#inoltre dallo scatterplot delle prime 2 variabili si evince una netta distinzione tra i gruppi: 
fetal_Health %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point()

#i sospetti si collocano a destra dei sani e non tra sani e malati>>>>motivazione:
#gli individui sospetti sono semplicemente dei casi sul quale la ricerca in campo medico non ha svolto analisi approfondite; 
#infatti sono individui in cui i dati raccolti sono molto diversi sia dagli individui sani sia dagli individui malati

#ASH CONTROLLA SE QUESTA COSA CORRISPONDE ALLA REALTà!!!!!!!!!!!!!!


#dataset per model-based clustering:
(fetal_Health_EM<-fetal_Health%>%
  select(all_of(load_vars))) #dataset solo con le variabili selezionate tramite pca (non servono le etichette per il clustering)

#dataset per model-based classification
(fetal_Health_classification <-fetal_Health%>%
  select(all_of(load_vars),fetal_health))

fetal_Health_viz <- fetal_Health_classification %>%
  gather(key = "Variable", value = "Value", -fetal_health)

# Create a facetted box plot using ggplot2
ggplot(fetal_Health_viz, aes(x = fetal_health, y = Value, fill =fetal_health )) +
  geom_boxplot() +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title = "Box Plot",
       x = "Species",
       y = "Value",
       fill = "Variable") +
  theme_minimal()
#a livello univariato non si evince particolare differenza tra i vari gruppi ad eccezione della variabile "histogram_mean"
#non significa che non vi sia corrispondenza a livello bivariato (scatterplot precdenti) o multivariato (non visibile con strumenti grafici)  
# a sostegno delle osservazioni precdenti sul gruppo dei sospetti in generale sono individui più simili al gruppo dei sani ma questo
#non vale per ogni variabile (3 su 4)


# clustering --------------------------------------------------------------

#conviene provare a styimare la classificazine sia tramite 10 variabili
#sia tramite le 4 slezionate (magari per far vedere che con 10 variabili alcuni modelli
#non li stima)>>>>>>idem provare a fare un EM basato su tutte le variabili o quanto meno non solo sulle 4 selezionatre tramite pca


#IMPLEMENTO UN EM DI NORMALI SENZA SPECIFICARE IL NUMERO DI GRUPPI
#install.packages("mclust")
library(mclust)

set.seed(123) #col set.seed stima con k=3 (siccome facilmente sbaglia....probabilmente la scelta degli initial values è cruciale...
#da specificare nell'homework)>>>>>CON IL SEED PRECEDENTE NON RISULTAVA K=3
health_mclust_ICL<-mclustICL(fetal_Health_EM, G=2:6) #non ha fatto alcun salto (si può ipoptizzare che il numero di u.s. sia sufficiente
#a stimare anche il modello più complkesso VVV anche con 9 gruppi....di default mclust stima da 1 a 9 gruppi per i 14 modelli possibili)
summary(health_mclust_ICL) #modello EEV con 3 componenti 
plot(health_mclust_ICL,ylim=c(-35000,-25000))  #guardate che bellino
str(health_mclust_ICL)


#per curiosità:
set.seed(123)
plot(mclustICL(fetal_Health_EM,G=2:10),ylim=c(-34000,-26000)) #non risulta per nulla un k predominante
#gran parte dei modelli in base all'ICL risultano della stessa precisione qualunque sia
#il numero di gruppi

set.seed(123)
health_mclust_BIC<-Mclust(fetal_Health_EM) 
summary(health_mclust_BIC) #secondo il BIC (non accurato come ICL) risulta k=8 e modello VEV

#SIA TRAMITE ICL SIA TRAMITE BIC IL MODEL BASED CLUSTERING FORNISCE UN NUMERO DI GRUPPI DIFFERENTE....PROVIAMO A SPECIFICARE IL NUMERO DI GRUPPI:


#necessario provare con k=3 (SENZA set.seed VIENE DIVERSISSIMO) >>>escono precisioni che variano dal 60% all'80%
#significa che è un EM molto variabile/instabile e poco robusto (tutte caratteristiche che rendono cruciale la scelta dei valori iniziali)
set.seed(123) #MA è CORRETTO USARE TUTTE LE VOLTE SET.SEED (oltre per avere gli stessi risultati su qualunque pc.....)
health_mclust_ICL_k3<-mclustICL(fetal_Health_EM,G=3)
summary(health_mclust_ICL_k3) #EVV 


set.seed(123)
health_mclust_BIC_k3<-Mclust(fetal_Health_EM,G=3)
summary(health_mclust_BIC_k3) #EVV


#megliobasarsi sull'ICL ma in questo caso si ppssono prendere le etichette dal BIC senza problemi perchè i 2 modelli coincidono

(etichette_stimate<-health_mclust_BIC_k3$classification)

precisione_EM<-classError(etichette_stimate, class=etichette)
1-precisione_EM$errorRate   #76%

(confusion_matrix <- table( etichette, etichette_stimate)) #DA CORREGGERE


coordProj (as.data.frame(fetal_Health_EM), dimens=c(1,2), what="classification",
           classification=health_mclust_BIC_k3$classification,
           col=c("dodgerblue2","green3","red2"), symbols=c(0 ,16 ,17),
           sub="(b) Model-Based Clustering")
points(fetal_Health_EM[precisione_EM$misclassified,c(1,2)],pch=19)


coordProj (data=as.data.frame(fetal_Health_EM), dimens=c(1,2), what="uncertainty",
           parameters=health_mclust_BIC_k3$parameters , z=health_mclust_BIC_k3$z) #più questa che vogliamo implementare

adjustedRandIndex (etichette_stimate , etichette) #rand index molto basso



#sbagliamo quasi tutti i malati e i dubbiosi 
#i 3 gruppi che sembra indovinare sono sbagliati


#il vero problkema risulta nella scelta del numero di cluster che in assenza delle etichette
#risulta spesso maggiore di 3 (6 o 7 a seconda del seed utilizzato)
#probabilmente i cluster non sono ben definiti nemmeno dalle etichette note....stando al risultato
#dell'algoritmo è possibile che alcuni di essi siano ulteriormente scomponibili in altri
#sotto gruppi
#come capire quale delle etichette è balorda:
#valutare nelleclassificazione se alcuni gruppi vengono visti come misture....
#possibile soluzione alla presenza di sotto gruppi (inoltre una analisis accurata di sentitivity e
#specificity per valutare se una etichetta in particolare è soggetta ad errori)
#anche la differenza sostanziale tra le numerosità nei gruppi può essere una motivazione
#della scarsa precisione dell'algoritmo EM (risolvibile tramite under sampling o con le cavolate di Gervi????)








# classification ----------------------------------------------------------
# da fare  
# normal V
# con MDA X
# con oversampling X

#normal

train_test<-function(data,perc=0.7){
  #gruppi è il nome della variabile con le etichette
  set.seed(123)
  index<-sample(c("train","test"),size=nrow(data),replace=T,prob=c(perc,1-perc))
  train<-data[index=="train",]
  test<-data[index=="test",]
  lis<-list(data_train=train,
            data_test=test)
  return(lis)
}


#costruzione train e test set per classification:



out<-train_test(fetal_Health_classification,0.7)
data_train<-out$data_train
data_test<-out$data_test

# EDDA (CV) -------------------------------------------------------------------------------------

#dobbiamo provare altri modelli ovviamente 
set.seed(123)
(pr<-mixmodLearn(data_train[,1:4], c(data_train$fetal_health),
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè

#prediction con il nostro classifier sembra accurate al 80 percento
#bisogna capire meglio le variabili che iniziano per "histogram" che sembrano non entrare nel classifi
summary(pr) #ma quindi error rate MAP=0% significa che se eseguiamo sul train set stesso la classificazione è perfetta???? è razionale
str(pr)

PREDICTION<- mixmodPredict(data = select(data_test,-fetal_health), classificationRule=pr["bestResult"])   #non puoi fare direttamente predict??????????
str(PREDICTION)
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè

etichette_prediction_EDDA <- PREDICTION@partition
etichette_prediction_EDDA <-as.factor(etichette_prediction_EDDA)
levels(etichette_prediction_EDDA) <- c("Normale","Sospetto", "Patologico")
mean(etichette_prediction_EDDA == data_test$fetal_health) # bisogna andare a vedere la specificity dei malati 3
PREDICTION@proba[1:30,] #se no ci mette anni a plottare tutto (PREDICTION@partition non ci interessa visualizzarlo)

#c'è un modo migliore di fare la confusion matrix? 
# (confusion_matrix <- table(data_test$fetal_health, etichette_prediction_EDDA)) #non prendiamo bene gli ammalati molto male (DA COMMENTARE)
# (accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)) # confirmed 84% confirmed

confmatrix <-confusionMatrix(etichette_prediction_EDDA,  data_test$fetal_health) #qua c'è sia confusion matrix che tutto
confmatrix

  
prob.post_incertezza<- tibble(PREDICTION@proba) %>%
  rowwise() %>% # operiamo riga per riga
  mutate(incertezza = 1 - max(c_across(everything()))) 


data_test %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point(size=prob.post_incertezza$incertezza*10)+
  geom_point(data = filter(data_test,etichette_prediction_EDDA != data_test$fetal_health ), 
             color = "black", alpha = 0.3,size=prob.post_incertezza$incertezza[etichette_prediction_EDDA != data_test$fetal_health]*10)

# MDA (BIC) --------------------------------------------------------------------------------------------

set.seed(123)
mod2 <- MclustDA(data_train[,1:4], data_train$fetal_health) 

#COMMENTO DA CONTROLLARE
#, modelNames = "VVV", G = 4
#bisogna capire come mai non mi specifica i modelli se non metto niente in model names e
#se lascio g senza niente (perchè devo specificare che modello e quanti g non dovrebbe farlo da solo????)
# Ok era semplicemente perchè gli davamo da stimare troppi parametri 
summary(mod2)
str(mod2) 
etichette_prediction_MDA<-predict(mod2, select(data_test,-fetal_health))$class #  questo sono le prediction del MDA
mean(etichette_prediction_MDA == data_test$fetal_health) 
confusionMatrix(etichette_prediction_MDA, data_test$fetal_health) #(DA COMMENTARE)

prob.post_incertezza<- tibble(PREDICTION@proba) %>%
  rowwise() %>% # operiamo riga per riga
  mutate(incertezza = 1 - max(c_across(everything()))) 

data_test %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point(size=prob.post_incertezza$incertezza*10)+
  geom_point(data = filter(data_test,etichette_prediction_EDDA != data_test$fetal_health ), 
             color = "black", alpha = 0.3,size=prob.post_incertezza$incertezza[etichette_prediction_EDDA != data_test$fetal_health]*10)


# a quanto pare riesce a predirre un 85 % 
#quindi bisogna fare oversampling
#da fare confusion matrix specificity sensitivity ecc... (ha senso fare una funzione in cui gli diamo: etichette previste ed etichette reali (sul test set) e
#ci restituisce tutto....)

# MDA (CV) -------------------------------------------------------------------------------------------------------

accuracy<-function(g,mod,nCV=5,data,etichette){
  set.seed(123)
  mod_mda<-MclustDA(data,class=etichette,G=as.list(g),modelName=mod)
  return(1-cvMclustDA(mod_mda,nfold=nCV)$ce)
}

#funzione che valuta il miglior modello mda tramite cross validation con nfold=4 e restituisce il più accurato:
modello_MDA_k3<-function(data,etichette){
  g1<-g2<-g3<-c(1,2,3,4,5)
  g1<-as.data.frame(g1)
  g2<-as.data.frame(g2)
  g3<-as.data.frame(g3)
  join<-cross_join(cross_join(g1,g2),g3)
  join["mod"]<-"VII" #altrimenti con più modelli il codice impegherebbe troppo tempo
  #usiamo come alternativa il modello VII  che sono delle ipersfere del quale varia solo il volume
  out<-apply(join,MARGIN=1,function(pos) accuracy(g=pos[1:3],mod=pos[4],nCV=4,data=data,etichette=etichette))
  lis<-list(modello=join[which.max(out),],accuracy=out[which.max(out)]) #questa accuracy non è valida siccome è stimata sullo stesso dataset usato
  #per allenaere il modello (fuori dalla funzione viene valuatto su un test set a parte)
  return(lis)
}

#PROVARE CON EEE OPPURE FATTO GIà SOPRA?????? IN TEORIA COINCIDE CON EDDA...?????????

#CI METTE QUALCHE MINUTINO
(out<-modello_MDA_k3(data_train[,1:4],as.factor(data_train$fetal_health)))
#stimiasmo il modello migliore e sul test set forniamo la precisione tramite accuracy

mod_mda_k3<-MclustDA(data_train[,1:4],data_train$fetal_health,G=c(5,4,5),modelNames="VII")
etichette_prediction_MDA_cv<-predict(mod_mda_k3, select(data_test,-fetal_health))$class
mean(etichette_prediction_MDA_cv== data_test$fetal_health) #84%
confusionMatrix(etichette_prediction_MDA_cv, data_test$fetal_health) #confusion matrix
#fatica sulla etichetta "dubbiosi"
#da calcolare tutte le specificity ecc...


# MDA classificare dubbiosi come sani o malati -----------------------------------------------------------------


#funzione per un modello mda con soli 2 gruppi:
accuracy<-function(g,mod,nCV=5,data,etichette){
  set.seed(123)
  mod_mda<-MclustDA(data,class=etichette,G=as.list(g),modelName=mod)
  return(1-cvMclustDA(mod_mda,nfold=nCV)$ce)
}

modello_MDA_k2<-function(data,etichette){
  g1<-g2<-c(1,2,3,4,5)
  g1<-as.data.frame(g1)
  g2<-as.data.frame(g2)
  join<-cross_join(g1,g2)
  join["mod"]<-"VII" #altrimenti con più modelli il codice impegherebbe troppo tempo
  #usiamo come alternativa il modello VII  che sono delle ipersfere del quale varia solo il volume
  out<-apply(join,MARGIN=1,function(pos) accuracy(g=pos[1:2],mod=pos[3],nCV=4,data=data,etichette=etichette))
  lis<-list(modello=join[which.max(out),],accuracy=out[which.max(out)])
  return(lis)
}


(fetal_Health_no_sospetti<-fetal_Health_classification%>%
  filter(fetal_health!="Sospetto"))


(etichette_k2<-as.factor(fetal_Health_no_sospetti$fetal_health))
levels(etichette_k2)<-c("Normale","Normale","Patologico")
etichette_k2
modello_MDA_k2(fetal_Health_no_sospetti[,1:4],etichette_k2)
#set.seed() serve?????
mod_mda_k2<-MclustDA(fetal_Health_no_sospetti[,1:4],etichette_k2,G=4,modelNames="VII")
summary(mod_mda_k2)

(fetal_Healt_sospetti<-fetal_Health_classification%>%
  filter(fetal_health=="Sospetto"))

table(predict(mod_mda_k2,fetal_Healt_sospetti[,1:4])$class)/nrow(fetal_Healt_sospetti)

#7 dubbiosi su 8 classificati come sani





# MDA under/oversampling --------------------------------------------------------------------------------------

# Ho messo nei commenti tutto il codice spaghetti se ti serve qualcosa lo lascio ma mi sembra tutto da canellare

# 
# #per il futuro un eventuale under/over sampling ha senso (probabilmente più under sampling dalla
# #categoria sano che è maggiormente presente rispetto alle altre 2)

# over sampling

#install.packages( "https://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
#install.packages("grid")
#install.packages("DMwR")
#install.packages("ROCR")

library(lattice)
library(grid)
library(DMwR)

(train2<-data_train %>% 
  as.data.frame())
table(train2$fetal_health)

levels(train2$fetal_health) <- c("Normale","Normale","Patologico") # ho messo tutti della classe 2 nella classe 1 (DA RIVEDERE)

table(train2$fetal_health)


new_train <- SMOTE(fetal_health ~ ., train2, perc.over= 600, perc.under = 117)
dim(new_train)
# + perc.over/100 % is the number of cases generated (in questo caso 1/3 sono reali)


#  if 200 new examples were generated for the minority class, a value of perc.under of 100 will randomly select exactly 
#  200 cases belonging to the majority classes from the original data set to belong to the final data set. Values above 100 will select more examples from the majority classes.
# in questo caso prendiamo (+ perc.over/100 %) * ncasi * perc.under 

table(new_train$fetal_health) #PAREGGIARE LE U.S. NEI 2 GRUPPI NON è ECCESSIVO????

# mod3 <- MclustDA(new_train[,-5], new_train$fetal_health)
# summary(mod3)
# str(mod3) 
# predicted_labels <- predict(mod3, select(test,-fetal_health))$class #  questo sono le prediction del MDA
# real_labels <- pull(test,fetal_health) #etichette vere
# levels(real_labels) <- c(1,1,3)
# real_labels
# mean(predicted_labels == real_labels) #pull estrae un vettore da un db
# 
# table(predicted_labels,real_labels ) # vediamo che sbagliamo in maniera simile ma riusciamo 
# #a non perderci i "falsi sani" 


mm <-mixmodGaussianModel(family = "all",
                                        free.proportions = F)

?mixmodGaussianModel
modsmote <- mixmodLearn(new_train[,-5], new_train$fetal_health ,models=mm,
                        criterion = "CV")

modsmote@bestResult
new_train
#TUTTA STA PARTE DA RICONTROLLARE PERCHè CI SONO I NOMI DEI DATASET SBAGLIATI


#bisogna capire meglio le variabili che iniziano per "histogram" che sembrano non entrare nel classifi
summary(modsmote) #ma quindi error rate MAP=0% significa che se eseguiamo sul train set stesso la classificazione è perfetta???? è razionale
str(modsmote)

(test_no_sospetti<-data_test%>%filter(fetal_health!="Sospetto"))

PREDICTION<- mixmodPredict(data = select(test_no_sospetti,-fetal_health), classificationRule=modsmote["bestResult"])
str(PREDICTION)
PREDICTION@classificationRule
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè

(real_labels <- as.factor(test_no_sospetti$fetal_health)) #etichette vere
levels(real_labels) <- c("Normale","Normale","Patologico")

(etichette_prediction_oversampling<-as.factor(PREDICTION@partition))
levels(etichette_prediction_oversampling)<-c("Normale","Patologico")
mean(etichette_prediction_oversampling == real_labels) # bisogna andare a vedere la specificity dei malati 3
PREDICTION@proba[1:30,] #se no ci mette anni a plottare tutto (PREDICTION@partition non ci interessa visualizzarlo)

#c'è un modo migliore di fare la confusion matrix? 
confusionMatrix(etichette_prediction_oversampling,real_labels)  #non prendiamo bene gli ammalati molto male!
#questo è un 88% dato da un oversampling su un modello EDDA (credo) ?????



PREDICTION<- mixmodPredict(data = select(data_test,-fetal_health), classificationRule=modsmote["bestResult"])
str(PREDICTION)
PREDICTION@classificationRule
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè

(real_labels <- as.factor(data_test$fetal_health)) #etichette vere
levels(real_labels) <- c("Normale","Normale","Patologico")

(etichette_prediction_oversampling<-as.factor(PREDICTION@partition))
levels(etichette_prediction_oversampling)<-c("Normale","Patologico")
mean(etichette_prediction_oversampling == real_labels) # bisogna andare a vedere la specificity dei malati 3
PREDICTION@proba[1:30,] #se no ci mette anni a plottare tutto (PREDICTION@partition non ci interessa visualizzarlo)

#c'è un modo migliore di fare la confusion matrix? 
confusionMatrix(etichette_prediction_oversampling,real_labels) 


prob.post_incertezza<- tibble(PREDICTION@proba) %>%
  rowwise() %>% # operiamo riga per riga
  mutate(incertezza = 1 - max(c_across(everything()))) 


data_test %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = real_labels)) +
  geom_point(size=prob.post_incertezza$incertezza*5)+
  geom_point(data = filter(data_test,etichette_prediction_oversampling != real_labels), 
             color = "black", alpha = 0.3,size=prob.post_incertezza$incertezza[etichette_prediction_oversampling != real_labels]*5)

data_test%>%
  select(-fetal_health)%>%
  ggpairs(aes(colour = ifelse(etichette_prediction_oversampling != real_labels,real_labels, "black"), alpha = 1)) 
 
#ora lo provo su MDA

 



#poi in futuro voi geni se volete mettere le variabili in INGLESE è più bello

#classificazione su sani e malati (ultima parte dello script)
#malati e sani>>>>stimato questo modello poi classifichiamo i dubbiosi e vediamo
#se effettivamente finiscono nei sani (DA GIUSTIFICARE CHE SONO SOLO ASSUNZIONI
#BASATE SUI DATI E NON SUPERANO QUELLE DI UN MEDICO: i dati sembrano molto più simili ai sani
#ma sono anche diversi/anomali e non ancora classsificati dal pto di vista scientifico)
#DA RIVEDERE






#provare il daatset con oversampling in un MDA CV

new_train #per il training del modello MDA con oversampling (forse 50 e 50 è eccesivo????? un 2/3 e 1/3 non andava già bene??????)
test_no_sospetti #per valutare se i normali e malati li mette nella categoria giusta ma qui i "Sospetti" li esclude....
#....mentre nel training i sospetti li considera come sani (che facciamo ??????)

set.seed(123)
modello_MDA_k2(new_train[,1:4],as.factor(new_train$fetal_health))
modello_MDA_k2_UOsampling<-MclustDA(new_train[,1:4],as.factor(new_train$fetal_health),G=list(4,5),modelNames="VII")
(etichette_prediction_MDA_oversampling_k2<-predict(modello_MDA_k2_UOsampling,test_no_sospetti[,1:4])$class)
confusionMatrix(etichette_prediction_MDA_oversampling_k2,real_labels)
#solito 91%....
#..io proverei a non mettere le etichette 2 come etichette 1......

