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
fetal_Health$histogram_tendency # questo è un factor (-1/0/1)
fetal_Health$percentage_of_time_with_abnormal_long_term_variability # non è distribuito come normale

hist(fetal_Health$accelerations )
hist(fetal_Health$fetal_movement)
hist(fetal_Health$prolongued_decelerations)

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

#qualche outliers...... non ha senso ragionare sugli outliers ma ci sta fare dei boxplot suddivisi per etichetta e selezionare se c'è qualche variabile 
#molto rilevante per classification
boxplot(fetal_Health[,-c(2,3,5,6,13)])
boxplot(fetal_Health[,6])
boxplot(fetal_Health[,c(2,3)])
boxplot(fetal_Health[,5])
boxplot(fetal_Health[,13])
colnames(fetal_Health)

boxplot(fetal_Health$baseline.value~fetal_Health$fetal_health) #dubbiosi diversi dagli altri 2
boxplot(fetal_Health$uterine_contractions~fetal_Health$fetal_health) #NAH
boxplot(fetal_Health$light_decelerations~fetal_Health$fetal_health) # 3 abbastanza diverse
boxplot(fetal_Health$abnormal_short_term_variability~fetal_Health$fetal_health) #sani diversi
boxplot(fetal_Health$mean_value_of_short_term_variability~fetal_Health$fetal_health) # dubbiosi un po' diversi
boxplot(fetal_Health$mean_value_of_long_term_variability~fetal_Health$fetal_health) # malati leggermente diversi
boxplot(fetal_Health$histogram_width~fetal_Health$fetal_health) #sempre dubbiosi un po' diversi
boxplot(fetal_Health$histogram_min~fetal_Health$fetal_health) #sempre dubbiosi un po' diversi
boxplot(fetal_Health$histogram_max~fetal_Health$fetal_health) #identiche
boxplot(fetal_Health$histogram_mode~fetal_Health$fetal_health) #malato un po' diverso
boxplot(fetal_Health$histogram_mean~fetal_Health$fetal_health) #malato un po' diverso
boxplot(fetal_Health$histogram_median~fetal_Health$fetal_health) #malato un po' diverso

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

#scatterplot con le prime 2 variabili più significative a seguito della pca
fetal_Health %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point()
  #strano che i dubbiosi si collochino a destra dei sani e non tra sani e malati


(fetal_Health <- fetal_Health %>% 
    rowid_to_column("id"))





# clustering --------------------------------------------------------------

#conviene provare a styimare la classificazine sia tramite 10 variabili
#sia tramite le 4 slezionate (magari per far vedere che con 10 variabili alcuni modelli
#non li stima)>>>>>>idem provare a fare un EM basato su tutte le variabili o quanto meno non solo sulle 4 selezionatre tramite pca


#IMPLEMENTO UN EM DI NORMALI SENZA SPECIFICARE IL NUMERO DI GRUPPI
#install.packages("mclust")
library(mclust)
(fetal_Health_EM<-fetal_Health%>%
  select(all_of(main_comp))) #dataset solo con le variabili selezionate tramite pca (non servono le etichette per il clustering)

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

train_test<-function(data,gruppi,perc=0.7){
  #gruppi è il nome della variabile con le etichette
  set.seed(123)
  index<-sample(c("train","test"),size=nrow(data),replace=T,prob=c(perc,1-perc))
  train<-data[index=="train",]
  test<-data[index=="test",]
  lis<-list(data_train=train,
            data_test=test)
  return(lis)
}



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
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)) # confirmed 82% confirmed

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

train2<-train %>%
  select(all_of(main_comp), fetal_health) %>% 
  as.data.frame()
table(train2$fetal_health)

levels(train2$fetal_health) <- c(1,1,3) # ho messo tutti della classe 2 nella classe 1 (DA RIVEDERE)

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
                                        free.proportions = F, equal.proportions = TRUE)
                  
modsmote <- mixmodLearn(new_train[,-5], new_train$fetal_health ,models=mm,
                        criterion = "CV")


#bisogna capire meglio le variabili che iniziano per "histogram" che sembrano non entrare nel classifi
summary(modsmote) #ma quindi error rate MAP=0% significa che se eseguiamo sul train set stesso la classificazione è perfetta???? è razionale
str(modsmote)

PREDICTION<- mixmodPredict(data = select(test,-fetal_health), classificationRule=modsmote["bestResult"])
str(PREDICTION)
#fino alla colonna 7 il mio pc funziona dopo crusha 
#capire perchè
real_labels <- pull(test,fetal_health) #etichette vere
levels(real_labels) <- c(1,1,3)

mean(PREDICTION@partition == test$fetal_health) # bisogna andare a vedere la specificity dei malati 3
PREDICTION@proba[1:30,] #se no ci mette anni a plottare tutto (PREDICTION@partition non ci interessa visualizzarlo)

#c'è un modo migliore di fare la confusion matrix? 
confusion_matrix <- table(test$fetal_health, PREDICTION@partition)  #non prendiamo bene gli ammalati molto male
(accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)) # ESCE 68% OCCHIO!!!


 



#poi in futuro voi geni se volete mettere le variabili in INGLESE è più bello

#classificazione su sani e malati (ultima parte dello script)
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

#CI METTE QUALCHE MINUTINO
(out<-modello_MDA_k3(data_train,as.factor(label_train$fetal_health)))
#stimiasmo il modello migliore e sul test set forniamo la precisione tramite accuracy

mod_mda_k3<-MclustDA(data_train,label_train$fetal_health,G=c(5,2,2),modelNames="VII")
mean(c(predict(mod_mda_k3, select(test,-fetal_health))$class) == pull(test,fetal_health)) #83.4%
table((predict(mod_mda_k3, select(test,-fetal_health))$class),pull(test,fetal_health)) #confusion matrix
#fatica sulla etichetta "dubbiosi"
#da calcolare tutte le specificity ecc...


#funzione per un modello mda con soli 2 gruppi:
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

(fetal_Health_no_dubbiosi<-fetal_Health%>%
  select(all_of(main_comp),fetal_health)%>%
  filter(fetal_health!=2))


(etichette_k2<-as.factor(fetal_Health_no_dubbiosi$fetal_health))
levels(etichette_k2)<-c("1","1","3")
etichette_k2
modello_MDA_k2(fetal_Health_no_dubbiosi[,1:4],etichette_k2)
#set.seed() serve?????
mod_mda_k2<-MclustDA(fetal_Health_no_dubbiosi[,1:4],etichette_k2,G=4,modelNames="VII")
summary(mod_mda_k2)

(fetal_Healt_dubbiosi<-fetal_Health%>%
  filter(fetal_health==2)%>%
  select(all_of(main_comp)))

table(predict(mod_mda_k2,fetal_Healt_dubbiosi)$class)

#PROVARE CON EEE OPPURE FATTO GIà SOPRA?????? IN TEORIA COINCIDE CON EDDA...?????????







UOsampling<-train_test(new_train,gruppi="fetal_health",0.8)
str(UOsampling)

(train_set<-UOsampling$data_train)
(test_set<-UOsampling$data_test)

set.seed(123)
modello_MDA_k2(train_set[,1:4],as.factor(train_set$fetal_health))
modello_MDA_k2_UOsampling<-MclustDA(train_set[,1:4],as.factor(train_set$fetal_health),G=5,modelNames="VII")
mean(predict(modello_MDA_k2_UOsampling,test_set[,1:4])$class==as.factor(test_set$fetal_health)) 
#solito 83%....io proverei a non mettere le etichette 2 come etichette 1...
