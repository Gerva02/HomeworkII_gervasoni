
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




fetal_Health <- tibble(read.csv("fetal_health.csv")) %>%
  mutate_at(vars(fetal_health),as.factor)

levels(fetal_Health$fetal_health) <- c("Normale","Sospetto","Patologico")

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

colnames(fetal_Health)
fetal_Health%>%select(-fetal_health)%>%ggpairs() #ggpairs senza etichette: osservazioni/conclusioni per clustering
#non sono presewnti (soprattutto nelle 4 pca selezionate) molte distribuzioni con multimodalità ma
#è normale siccome siamo su datoi reali e siccome la clusterizzazione è improbabile che sia evidente a livello 
#univariato 


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


#dataset per model-based clustering:
(fetal_Health_EM<-fetal_Health%>%
  select(all_of(load_vars))) #dataset solo con le variabili selezionate tramite pca (non servono le etichette per il clustering)

#dataset per model-based classification:
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


#IMPLEMENTO UN EM DI NORMALI SENZA SPECIFICARE IL NUMERO DI GRUPPI
#install.packages("mclust")
library(mclust)

set.seed(123) 
health_mclust_ICL<-mclustICL(fetal_Health_EM, G=2:10) #l'algoritmo EM non riesce a stimare modelli complessi come VVV a causa della
#bassa disponibilità di u.s. (nemmeno VVV con k=3)
summary(health_mclust_ICL) #VEV,7 e EVV,3 (sono molto vicini tra loro)  
plot(health_mclust_ICL,ylim=c(-33000,-29000)) 

#non risulta un k predominante
#gran parte dei modelli in base all'ICL risultano della stessa precisione qualunque sia il numero di gruppi
#dovuto al fatto che essendo dati reali è verosimile che ciascun gruppo abbia diversi sotto gruppi (sensato un MDA in fase
#di model-based classification)


set.seed(123)
health_mclust_BIC<-Mclust(fetal_Health_EM,G=2:10) 
summary(health_mclust_BIC) #secondo il BIC (non accurato come ICL, in quanto non tiene conto dell'entropia) risulta k=8 e modello VEV

#è un EM molto variabile/instabile e poco robusto (tutte caratteristiche che rendono cruciale la scelta dei valori iniziali)


#SIA TRAMITE ICL SIA TRAMITE BIC IL MODEL BASED CLUSTERING FORNISCE UN NUMERO DI GRUPPI DIFFERENTE....PROVIAMO A SPECIFICARE IL NUMERO DI GRUPPI:



set.seed(123) 
health_mclust_ICL_k3<-mclustICL(fetal_Health_EM,G=3)
summary(health_mclust_ICL_k3) #EVV 

set.seed(123)
health_mclust_BIC_k3<-Mclust(fetal_Health_EM,G=3)
summary(health_mclust_BIC_k3) #EVV


#in questo caso ICL e BIC restituiscono lo stesso modello (se fossero diversi sarebbe opportuno dare piorità al modello fornito tramite ICL)


(etichette_stimate<-health_mclust_BIC_k3$classification)
table(etichette)
table(etichette_stimate) 

(precisione_EM<-classError(etichette_stimate, class=etichette))
(CER<-precisione_EM$errorRate) 
(accuracy<-1-CER)
#76% di accuracy

adjustedRandIndex (etichette_stimate , etichette) #rand index molto basso

(etichette_stimate<-as.factor(etichette_stimate))
levels(etichette_stimate)<-c("Patologico","Normale","Sospetto") #sembra la classificazione più coerente con le numerosità e con l'output di "classError" 
#(considera l'abbinamento più adeguato tra i possibili delle etichette originaliu con quelle stimate)
 
confusionMatrix(etichette_stimate,etichette) #la sensitivity è molto alta solo nella classe "Normale"
#al contrario la specificity è molto bassa (significa che tra patologico e normale non riesce ad ottenere una buonma divisione)
# in generale si può dire che i gruppi "Sospetto" e "Patologico" non sono rilevati correttamente dall'EM
#ES: 253 casi sospetti che vengono classificati come "Normali"


coordProj (as.data.frame(fetal_Health_EM), dimens=c(1,2), what="classification",
           classification=health_mclust_BIC_k3$classification,
           col=c("dodgerblue2","green3","red2"), symbols=c(0 ,16 ,17),
           sub="(b) Model-Based Clustering")
points(fetal_Health_EM[precisione_EM$misclassified,c(1,2)],pch=19) #rappresentazione in nero delle u.s. missclassified
#si osserva che quanto meno le u.s. facenti parte del gruppo dei malati vengono allocate correttamente


#questo grafico rappresenta l'incertezza delle u.s.:
coordProj (data=as.data.frame(fetal_Health_EM), dimens=c(1,2), what="uncertainty",
           parameters=health_mclust_BIC_k3$parameters , z=health_mclust_BIC_k3$z, xlim=c(85,170),
           ylim=c(125,216)) #più questa che vogliamo implementare
           

#Infatti vie è una sostanziale differenza tra i cluster iniziali con le etichette note e quelli conb le etichette stimate
#il gruppo iniziale dei sospetti si fonde a quello dei normali e il gruppo dei normnali viene scisso in 2 sotto gruppi (uno molto grande che include anche gran parte 
#dei sospetti ed uno più piccolo che si pone a metà tra i malati ed i normali) (stando a questo grafico tutte le assosciazioni con la confusion matrix le distanze l'incertezza ecc
#non hanno senso e sono da ricontrollare)>>>>>soprattutto le conflusioni sulla matrice delle distanze KLs
#il gruppo dei malati si distingue comunque abbastanza bene ma l'EM non riconosce (ovviamente) quel gruppo di individui malati che nel grafico iniziale erano molto distandi dal
#baricentro degli individui malati (in basso a destra....) 

(pj<-health_mclust_BIC_k3$parameters$pro)
zij<-health_mclust_BIC_k3$z

(entropia<-(-1)*sum(rowSums(zij*log(zij))))
(entropia_relativa<-entropia/n/log(3))  #non male

(mu1<-health_mclust_BIC_k3$parameters$mean[,1]) #questi sono i patologici e sono l'unica cosa che ha senso confrontsare a livello di sensitivity 
(mu2<-health_mclust_BIC_k3$parameters$mean[,2])
(mu3<-health_mclust_BIC_k3$parameters$mean[,3])
(mu<-mu1*pj[1]+mu2*pj[2]+mu3*pj[3])

sigma1<-matrix(health_mclust_BIC_k3$parameters$variance$sigma[1:16],nrow=4,ncol=4,byrow=T)
sigma2<-matrix(health_mclust_BIC_k3$parameters$variance$sigma[17:32],nrow=4,ncol=4,byrow=T)
sigma3<-matrix(health_mclust_BIC_k3$parameters$variance$sigma[33:48],nrow=4,ncol=4,byrow=T)

var_within<-pj[1]*sigma1+pj[2]*sigma2+pj[3]*sigma3
var_between<-pj[1]*(mu1-mu)%*%t(mu1-mu)+pj[2]*(mu2-mu)%*%t(mu2-mu)+pj[3]*(mu3-mu)%*%t(mu3-mu)
(var_mixture<-var_within+var_between)

(R2_tr<-1-sum(diag(var_within))/sum(diag(var_mixture))) #0.27 (male)
(R2_det<-1-det(var_within)/det(var_mixture)) #0.73 (non così male)

post_prob<-apply(zij,1,max)
incertezza<-1-post_prob
(unita_incerte<-tibble(index=1:n,incertezza=incertezza)%>%
  arrange(desc(incertezza))%>%
  print(n=10))

fetal_Health$fetal_health[unita_incerte$index[1:20]] #molto strano che siano tutte del gruppo normali 
#(ha senso stando alle stime dei gruppi che l'algoritmo
#EM ha fatto.....pessime.....da vedere nel grafico ma ha senso che siano u.s. molto dubbiose tra malati e sani o all'interno del gruppo dei sani che è stato "spezzato")



#funzione che risteituisce la distanza/divergenza di kullback-leibler simmetrizzata:           CORRETTO "DISTANZA"??????
KLs<-function(mu1,mu2,sigma1,sigma2) {
  return(0.5*t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)+0.5*sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))-length(mu1)) #gervi dimmi se è giusta...
  }

KLs_matrix<-matrix(0,nrow=3,ncol=3)
KLs_matrix[1,2]<-KLs_matrix[2,1]<-KLs(mu1,mu2,sigma1,sigma2)
KLs_matrix[1,3]<-KLs_matrix[3,1]<-KLs(mu1,mu3,sigma1,sigma3)
KLs_matrix[3,2]<-KLs_matrix[2,3]<-KLs(mu3,mu2,sigma3,sigma2)
KLs_matrix #non  ha nulla a che vedere coi i gruppi reali....
det(KLs_matrix) #non credo abbia senso....ma forse per sintetizzare le KLs.......DA CONTROLLARE LE KLs PER 3 O PIù CLUSTER.....


#pessima clusterizzazione 


#DA PROVARE UN EM CON IL MODELLO TOP SENZA K SPECIFICATO
summary(health_mclust_ICL)
health_mclust_best<-Mclust(fetal_Health_EM,G=7,"VEV")
summary(health_mclust_best)

(zij_best<-health_mclust_best$z)
dim(zij_best)
(entropia_best<-(-1)*sum(rowSums(zij_best*log(zij_best)),na.rm=T)) #entropia molto più alta
entropia_best/n/log(7) #idem entropia relativa


#R2 
mu<-matrix(NA,nrow=4,ncol=7)
for (i in 1:7){
  mu[,i]<-health_mclust_best$parameters$mean[,i]
}
p<-health_mclust_best$parameters$pro
sigma<-list()
for (i in 1:7){
  sigma[[i]]<-matrix(health_mclust_best$parameters$variance$sigma[(i*16-16+1):(i*16)],nrow=4,ncol=4,byrow=T)
}

(mu_best<-apply(mu, MARGIN=1, function(x) t(x)%*%p))

var_within_best<-0
for (i in 1:7){
  var_within_best<-var_within_best+sigma[[i]]*p[i]
}

var_between_best<-0
for(i in 1:7){
  var_between_best<-var_between_best+p[i]*((mu[,i]-mu_best)%*%t(mu[,i]-mu_best))
}

(R2_det_best<-1-det(var_within_best)/det(var_between_best+var_within_best)) #a livello di R2 risulta meglio il modello con 7 gruppi (opposto rispetto all'entropia...ha senso
#essendo un numero di gruppi decisamente maggiore all'interno dello stesso grafico...comunque l'entropia viene già considerata tramite l'ICL)
(R2_tr_best<-1-sum(diag(var_within_best))/sum(diag(var_between_best+var_within_best))) #migliora anche questo

(incertezza_best<-apply(zij_best,1,function(x) 1-max(x)))
sort(incertezza_best)




# classification ----------------------------------------------------------

#funzione per eseguire una suddivisione in train set e test set:
train_test<-function(data,perc=0.7){
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


set.seed(123)
(pr<-mixmodLearn(data_train[,1:4], c(data_train$fetal_health),
                 models=mixmodGaussianModel(family='all'),
                 criterion=c('CV','BIC')))  



summary(pr) #la parte di evaluation deve essere svolta sul test set


PREDICTION<- mixmodPredict(data = select(data_test,-fetal_health), classificationRule=pr["bestResult"])   


etichette_prediction_EDDA <- as.factor(PREDICTION@partition)
levels(etichette_prediction_EDDA) <- c("Normale","Sospetto", "Patologico")


(confmatrix <-confusionMatrix(etichette_prediction_EDDA,  data_test$fetal_health)) #l'accuracy è elevata solo a causa
#della differenza di numerosità tra i gruppi; infatti ben 61 dei casi sospetti vengono nuovamente classificati come normali
#anche (seppur in minor parte) nei casi patologici si verifica questa missclassification (osservabile tramite sia la
#confusion matrix sia tramite sensitivity e specificity)


prob.post_incertezza<- tibble(PREDICTION@proba) %>%
  rowwise() %>% # operiamo riga per riga
  mutate(incertezza = 1 - max(c_across(everything()))) 


data_test %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point(size=prob.post_incertezza$incertezza*10)+
  geom_point(data = filter(data_test,etichette_prediction_EDDA != data_test$fetal_health ), 
             color = "black", alpha = 0.3,size=prob.post_incertezza$incertezza[etichette_prediction_EDDA != data_test$fetal_health]*10)
#maggior incertezza nella sezione del grafico in cui sono presenti gli individui sospetti


# MDA (BIC) --------------------------------------------------------------------------------------------

#sia dal ggpairs sia dall'algoritmo EM si evince la necessità di implementare un modello di classificazione di tipo MDA
set.seed(123)
mod2 <- MclustDA(data_train[,1:4], data_train$fetal_health) 

summary(mod2)

etichette_prediction_MDA<-predict(mod2, select(data_test,-fetal_health))$class 
confusionMatrix(etichette_prediction_MDA, data_test$fetal_health) #rimane la classe dei sospetti la più problematica stando ai valori di 
#sensitivity (bassa in patologico) e specificity (alta in patologico)
# in ogni caso la classificazione tramite MDA risulta migliore di quella eseguita attraverso il modello EDDA

str(predict(mod2, select(data_test,-fetal_health)))
prob.post_incertezza<- tibble(predict(mod2, select(data_test,-fetal_health))$z) %>%
  rowwise() %>% # operiamo riga per riga
  mutate(incertezza = 1 - max(c_across(everything()))) 

data_test %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = fetal_health)) +
  geom_point(size=prob.post_incertezza$incertezza*10)+
  geom_point(data = filter(data_test,etichette_prediction_MDA != data_test$fetal_health ), 
             color = "black", alpha = 0.3,size=prob.post_incertezza$incertezza[etichette_prediction_MDA != data_test$fetal_health]*10) #in nero la u.s. missclassified
#maggior incertezza nella sezione del grafico in cui sono presenti gli individui sospetti

#riesce a predirre un 85 % 
#la sensitivity del caso patologico è inferiore rispetto ad un valore soddisfacente, si può ipotizzare un oversampling



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
  #per allenaere il modello (fuori dalla funzione viene valutato su un test set)
  return(lis)
}

#------------------------------------------------------------------------------------------------------------------#
# 2 minuti per eseguire il comando:                                                                                #
# (out<-modello_MDA_k3(data_train[,1:4],as.factor(data_train$fetal_health))) #G=(5,4,5)                            #
# stimiasmo il modello migliore e sul test set forniamo la precisione tramite accuracy e la confusion matrix       #
#------------------------------------------------------------------------------------------------------------------#

set.seed(123)
mod_mda_k3<-MclustDA(data_train[,1:4],data_train$fetal_health,G=c(5,4,5),modelNames="VII")
etichette_prediction_MDA_cv<-predict(mod_mda_k3, select(data_test,-fetal_health))$class
confusionMatrix(etichette_prediction_MDA_cv, data_test$fetal_health) 

#fatica in "Sospetto" e "Patologico"
#abbiamo aumentato la sensitivity del caso "sospetto" ma peggiorato quello del caso "patologico"

'''

# MDA classificare sospetti come sani o malati -----------------------------------------------------------------

#implementare un MDA con lo scopo di classificare gli individui "sospetti" in una delle altre 2 classi

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


(fetal_Health_no_sospetti<-fetal_Health_classification%>%
  filter(fetal_health!="Sospetto"))


(etichette_k2<-as.factor(fetal_Health_no_sospetti$fetal_health))
levels(etichette_k2)<-c("Normale","Normale","Patologico")
modello_MDA_k2(fetal_Health_no_sospetti[,1:4],etichette_k2)
set.seed(123)
mod_mda_k2<-MclustDA(fetal_Health_no_sospetti[,1:4],etichette_k2,G=4,modelNames="VII") #G=(4,4)
summary(mod_mda_k2)

(fetal_Healt_sospetti<-fetal_Health_classification%>%
  filter(fetal_health=="Sospetto"))

table(predict(mod_mda_k2,fetal_Healt_sospetti[,1:4])$class)/nrow(fetal_Healt_sospetti)

#stando ai dati si evince che nella gran parte dei casi "sospetti" apparterrebbero alla classe normale



'''


# smote  ------------------------------------------------------


#install.packages( "https://cran.r-project.org/src/contrib/Archive/DMwR/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
#install.packages("grid")
#install.packages("DMwR")
#install.packages("ROCR")
library(lattice)
library(grid)
library(DMwR)


library(scutr)
dt <- as.data.frame(data_train)
data_new_patologici  <-oversample_smote(dt, "Patologico" , cls_col = "fetal_health", m = 600)
data_new_sospetto    <-oversample_smote(dt, "Sospetto" , cls_col = "fetal_health", m = 300)

new_train<-data_train %>%
  filter(fetal_health == "Normale") %>%
  sample_n(size=300) %>%
  rbind(tibble(data_new_patologici),tibble(data_new_sospetto) ) %>%
  as.data.frame()

mm <-mixmodGaussianModel(family = "all",
                         free.proportions = F) 
set.seed(123)
mod = MclustDA(new_train[,-5],
               new_train$fetal_health ,G=1:5,
               models=mm )


etichette_prediction_oversampling <- predict(mod, select(data_test,-fetal_health))$class

# modsmote <- mixmodLearn(new_train[,-5], new_train$fetal_health ,models=mm,
#                         criterion = "CV")
# 
# PREDICTION<- mixmodPredict(data = select(data_test,-fetal_health), classificationRule=modsmote["bestResult"])
real_labels <- as.factor(data_test$fetal_health)

#etichette_prediction_oversampling<-as.factor(PREDICTION@partition)
#levels(etichette_prediction_oversampling)<-c("Normale","Sospetto","Patologico")

(SMOTE_Confusion<-confusionMatrix(etichette_prediction_oversampling,real_labels) )
SMOTE_Confusion$table

prob.post_incertezza<- tibble(predict(mod, select(data_test,-fetal_health))$z) %>%
  rowwise() %>% # operiamo riga per riga
  mutate(incertezza = 1 - max(c_across(everything()))) 

data_test %>%
  ggplot(mapping = aes(x=histogram_mean , y = histogram_max, color = real_labels)) +
  geom_point(size=prob.post_incertezza$incertezza*7)+
  geom_point(data = filter(data_test,etichette_prediction_oversampling != real_labels), 
             color = "black", alpha = 0.3,size=prob.post_incertezza$incertezza[etichette_prediction_oversampling != real_labels]*7)


tibble(indice=c("R^2 determinante","R^2 traccia","entropia","entropia relativa"),EVV_3=c(0.73,0.27,356,0.15),VEV_7=c(0.89,0.39,1019,0.25))
