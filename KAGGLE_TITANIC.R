library(discretization)
library(doBy)
library(plyr)
library(DMwR)
library(pastecs)
library(sm)
library(car)
library(lattice)
library(Hmisc)
library(gclus)
library(smbinning)
library(caTools)
library(tcltk)
library(e1071)
library(adabag)
library(rpart)

options(scipen=9) #Affichage ecriture scientifique

#Lire les CSV
chemin = "C:/Users/Benoit/Documents/Formation Data Science AXA/"
files <- list.files(path= chemin, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(chemin,file,sep=""),sep=",",header=TRUE,na.strings=c("","NA"))
  )
}

#Stats desc rapides
summary(train)
summary(test)
## TRAIN : Valeurs manquantes sur Cabin / Age / Embarked
## TEST : Valeurs manquantes sur Cabin / Age / Fare 

#Stats miss par colonne
miss.stat <- function(data=NULL,id=NULL){
  
  dataset <- data
  
  nb.na <- as.data.frame(sapply(dataset, function(x) sum(is.na(x))))
  nb.na$pct.miss <- nb.na[,1] / nrow(dataset) * 100
  nb.na <- cbind(row.names(nb.na),nb.na)
  nb.na <- arrange(nb.na,desc(nb.na$pct.miss))
  print(nb.na)
  
}
miss.stat(data = train, id = 'PassengerId')
miss.stat(data = test, id = 'PassengerId')
##Trop de valeurs manquantes pour Cabin

#Supprimer variables inutiles
train.clean <- subset(train, select = -c(Name,Ticket,Cabin))
test.clean <- subset(test, select = -c(Name,Ticket,Cabin))

#Label cible
group.f <- factor(train$Survived, levels= c(1,0), labels = c("Yes", "No"))

#Imputations valeurs manquantes KNN
  #Test (voisins dans base Train)
knnimptest <- knnImputation(subset(test.clean, select = -c(PassengerId)),k=10,meth='median',
                            distData=subset(train.clean, select = -c(PassengerId, Survived)))
inspect <- as.numeric(knnimptest[is.na(test.clean[,'Age']),'Age']) #Verification imputation sur var. Age
stat.desc(inspect)
stat.desc(test.clean$Age)
test.clean <- cbind(subset(test.clean, select = -c(Age,Fare)),knnimptest$Age,knnimptest$Fare)
test.clean <- rename(test.clean, c('knnimptest$Age' = 'Age','knnimptest$Fare'='Fare'))
  #Train
knnimptrain <- knnImputation(subset(train.clean, select = -c(PassengerId)),k=10,meth='median')
inspect <- as.numeric(knnimptrain[is.na(train.clean[,'Age']),'Age']) #Verification imputation sur var. Age
stat.desc(inspect)
stat.desc(train.clean$Age)
train.clean <- cbind(subset(train.clean, select = -c(Age,Embarked)),knnimptrain$Age,knnimptrain$Embarked)
train.clean <- rename(train.clean, c('knnimptrain$Age' = 'Age','knnimptrain$Embarked'='Embarked'))

#Stats descriptives sur var. continues

  #Scatterplot
dta <- train.clean[,c('Fare','Age')] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" ) 
##Fare et Age peu correlees

  #Distribution univariee (Histogramme, QQPlot, Nuage de points)
dist.plot <- function(data=NULL,var.name=NULL){
  
  dataset <- data
  var <- dataset[,paste(var.name)]
  
  par(mfrow=c(1,2))
  hist(var, prob=T, xlab='',main=paste('Histogram of',var.name,sep=' '))
  lines(density(var,na.rm=T))
  rug(jitter(var))
  qq.plot(var,main=paste('Normal QQ plot of',var.name,sep=' '))
  par(mfrow=c(1,1))
  
  plot(var,xlab='')
  abline(h=mean(var,na.rm=T),lty=1)
  abline(h=mean(var,na.rm=T)+sd(var,na.rm=T),lty=2)
  abline(h=median(var,na.rm=T),lty=3)
}

  #Distribution bivariee (Boxplot, Violinplot, Densit?s Kernel) 
dist.bivar.plot <- function(data=NULL,var.name=NULL,group.name='target'){
  
  dataset <- data
  var <- dataset[,paste(var.name)]
  group <- dataset[,paste(group.name)]
  
  # plot densities
  sm.density.compare(var, group, xlab=paste(var.name)) 
  title(main=paste("Kernel Distribution :", var.name, "by", group.name, sep =' '))
  
  # add legend 
  colfill<-c(2:(2+length(levels(group.f))))
  legend('topright', levels(group.f), fill=colfill) 
  
  print(bwplot(group.f ~ var, data=dataset, ylab=paste(group.name), xlab=paste(var.name)))
  print(bwplot(group.f ~ var, data=dataset, panel=panel.bpplot, 
               probs=seq(.01,.49,by=.01), datadensity=TRUE,
               ylab=paste(group.name),xlab=paste(var.name)))
}

dist.plot(data=train.clean[,c('Fare','Age')],var.name='Fare')
##Forte concentration sur 0-30, kurtosis, 3 obs à valeur elevee (=500) tirant fortement la moyenne (med=14,moy=30)
dist.bivar.plot (data = train.clean[,c('Fare','Age','Survived')], var.name = 'Fare', group.name = 'Survived')
##Peu discriminante, davantage de dispersion sur les passagers ayant survecu

dist.plot(data=train.clean[,c('Fare','Age')],var.name='Age')
##Repartition plus homogene, med = moy = 29 ans
dist.bivar.plot (data = train.clean[,c('Fare','Age','Survived')], var.name = 'Age', group.name = 'Survived')
##Discriminante, proba de survie meilleure sur les jeunes

#Stats descriptives sur var. nominales/ordinales
cross.bivar.plot <- function(data=NULL,var.name=NULL,group.name='target'){
  dataset <- data
  var <- dataset[,paste(var.name)]
  group <- dataset[,paste(group.name)]
  
  # Data transformation. Data type must be factor.
  dataset$factor <- as.factor(var)
  
  # Package application and results
  result=smbinning.factor(df=dataset,y=paste(group.name),x="factor")
  print(var.name)
  print(result$ivtable)
  
  # Plots
  par(mfrow=c(2,2))
  smbinning.plot(result,option="dist",sub=paste(var.name))
  smbinning.plot(result,option="badrate",sub=paste(var.name))
  #smbinning.plot(result,option="WoE",sub=paste(var.name))
  par(mfrow=c(1,1))
}

cross.bivar.plot (data = train.clean[,c('Pclass','Sex','SibSp','Parch','Embarked','Survived')], 
                  var.name = 'Survived', group.name = 'Survived')
##Cible : 62% morts, 38% survivants
cross.bivar.plot (data = train.clean[,c('Pclass','Sex','SibSp','Parch','Embarked','Survived')], 
                  var.name = 'Pclass', group.name = 'Survived')
##Tres discriminant : proba de survie forte sur classe 1, faible sur classe 3
cross.bivar.plot (data = train.clean[,c('Pclass','Sex','SibSp','Parch','Embarked','Survived')], 
                  var.name = 'Sex', group.name = 'Survived')
##Tres discriminant : proba de survie forte sur femmes, faible sur hommes
cross.bivar.plot (data = train.clean[,c('Pclass','Sex','SibSp','Parch','Embarked','Survived')], 
                  var.name = 'SibSp', group.name = 'Survived')
##Discriminant : 65% morts si aucune famille a bord, 46% si 1, autres modalites peu representees
cross.bivar.plot (data = train.clean[,c('Pclass','Sex','SibSp','Parch','Embarked','Survived')], 
                  var.name = 'Parch', group.name = 'Survived')
##Discriminant : 65% morts si aucun partent/enfant a bord, 45% si 1-2, autres modalites peu representees
cross.bivar.plot (data = train.clean[,c('Pclass','Sex','SibSp','Parch','Embarked','Survived')], 
                  var.name = 'Embarked', group.name = 'Survived')
##Meilleure proba de survie des passagers embarques a Cherbourg (19% de l'echantillon), 70% ont embarque a South Hampton

#Discretisation var. continues
disc.continue <- function(data=NULL,sl=0.1,pctl=10,var.todisc=NULL,target=NULL,id=NULL) {
  
  dataset <- data
  var.disc <- paste(var.todisc,'disc',sep='.') 
  var.disc.label <- paste(var.disc,'label',sep='.')
  
  ##Nettoyage
  if(paste(var.disc) %in% colnames(dataset))
  {
    dataset <- dataset[,!(names(dataset) %in% c(paste(var.disc)) )]
  }
  if(paste(var.disc.label) %in% colnames(dataset))
  {
    dataset <- dataset[,!(names(dataset) %in% c(paste(var.disc.label)) )]
  }
  
  ##Decilage
  evenbins <- function(x, bin.count=10, order=T) {
    bin.size <- rep(length(x) %/% bin.count, bin.count)
    bin.size <- bin.size + ifelse(1:bin.count <= length(x) %% bin.count, 1, 0)
    bin <- rep(1:bin.count, bin.size)
    if(order) {    
      bin <- bin[rank(x,ties.method="random")]
    }
    return(factor(bin, levels=1:bin.count, ordered=order))
  }
  dataset$c1 <- evenbins(as.numeric(dataset[,var.todisc]), bin.count = pctl)
  table(dataset$c1,dataset[,target])
  prop.table(table(dataset$c1,dataset[,target]),1)
  
  ##ChiMerge
  disc<-chiM(matrix(c(dataset$c1, dataset[,target]),ncol=2,dimnames=list(dataset[,id])),alpha = sl)
  disc[[1]]
  mat<-disc[[2]]
  
  ##Resultats de la discretisation
  table(mat[,1],mat[,2])
  t<-table(mat[,1],mat[,2])
  freq<-table(mat[,1])
  target.rate<-prop.table(t,1)[,2]
  
  ##Ajouter la variable discretisee dans le datasetframe + supprimer la variable decilee
  dataset[,paste(var.disc)] <- mat[,1]
  dataset<- subset(dataset, select =-c1)
  
  ##Ajouter le label dans le dataset
  result.disc<-summaryBy(as.formula(sprintf("%s~%s", var.todisc, var.disc)), data = dataset, 	
                         FUN = function(x) { c(min = min(x), max = max(x)) }, var.names = 'value')
  result.disc[,paste(var.disc.label)]<-paste(var.todisc,'_','[',result.disc$value.min,';',result.disc$value.max,']',sep='')
  result.disc<-result.disc[,c(paste(var.disc),paste(var.disc.label))]
  dataset <- merge(dataset, result.disc, by.x = paste(var.disc), by.y = paste(var.disc), sort=F)
  
  dataset <- arrange(dataset,dataset[,id])
  
  ##Synthese discretisation
  dataset$factor <- as.factor(dataset[,paste(var.disc.label)])
  result=smbinning.factor(df=dataset,y=paste(target),x="factor")
  print(paste("Discretisation ChiMerge :",var.todisc))
  print(result$ivtable)
  par(mfrow=c(2,2))
  smbinning.plot(result,option="dist",sub=paste(var.todisc))
  smbinning.plot(result,option="badrate",sub=paste(var.todisc))
  #smbinning.plot(result,option="WoE",sub=paste(var.name))
  par(mfrow=c(1,1))
  dataset <- subset(dataset, select = -c(factor))
  
  output <- dataset
  
  return(output)
}


train.clean <- disc.continue(data=train.clean,sl=0.05, pctl=20, var.todisc='Fare', target='Survived', id='PassengerId')
##Decoupage en 4 classes, taux de survie croissant avec le montant, 93% de deces sur la tranche 0-7
train.clean <- disc.continue(data=train.clean,sl=0.05, pctl=20, var.todisc='Age', target='Survived', id='PassengerId')
##Decoupage en 7 classes, pas de monotonie, taux de survie max sur <= 5 ans, min sur 18-21 et 22-32
#Regroupement modalites var. discretes
train.clean$SibSp.disc <- ifelse(train.clean$SibSp==0,'0',
                                 ifelse(train.clean$SibSp==1,'1',
                                        ifelse(train.clean$SibSp==2,'2','>2')))

train.clean$Parch.disc <- ifelse(train.clean$Parch==0,'0',
                                 ifelse(train.clean$Parch==1,'1',
                                        ifelse(train.clean$Parch==2,'2','>2')))

train.clean$Pclass <- as.factor(train.clean$Pclass)
train.clean$Fare.disc.label <- as.factor(train.clean$Fare.disc.label)
train.clean$Age.disc.label <- as.factor(train.clean$Age.disc.label)
train.clean$Parch.disc <- as.factor(train.clean$Parch.disc)
train.clean$SibSp.disc <- as.factor(train.clean$SibSp.disc)
train.clean$Sex <- as.factor(train.clean$Sex)
train.clean$Embarked <- as.factor(train.clean$Embarked)

#Echantillons apprentissage (80%) / validation (20%)
set.seed(36)
trainIndex <- sample.split(train.clean$Survived, SplitRatio=80/100)

Train.Sample <- train.clean[trainIndex,]
Valid.Sample  <- train.clean[!trainIndex,]

prop.table(table(train.clean$Survived))
prop.table(table(Train.Sample$Survived))
prop.table(table(Valid.Sample$Survived))

#Classifieur Bayesien Naif

  #Train sample
Train.Sample$Survived <- factor(Train.Sample$Survived, levels= c(1,0), labels = c("Yes", "No"))
Valid.Sample$Survived <- factor(Valid.Sample$Survived, levels= c(1,0), labels = c("Yes", "No"))

nb <- naiveBayes(Survived ~ Fare.disc.label + Age.disc.label + Parch.disc + SibSp.disc + Sex + 
                      Pclass + Embarked, data = Train.Sample)
Train.Sample$Survived_predNB <- predict(nb, Train.Sample)

table(Train.Sample$Survived_predNB,Train.Sample$Survived, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(Train.Sample$Survived_predNB == Train.Sample$Survived) / length(Train.Sample$Survived),2)
paste("Taux d'erreur NB - Apprentissage =",error,"%")
##Erreur Train = 20%

  #Validation sample
Valid.Sample$Survived_predNB <- predict(nb, Valid.Sample)

table(Valid.Sample$Survived_predNB,Valid.Sample$Survived, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(Valid.Sample$Survived_predNB == Valid.Sample$Survived) / length(Valid.Sample$Survived),2)
paste("Taux d'erreur NB - Validation =",error,"%")
##Erreur validation = 19%


#Adaboost
formula <- Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age + Embarked
vardep <- Train.Sample[ , as.character(formula[[2]])]
cntrl <- rpart.control(maxdepth = 1, minsplit = 0, cp = -1) 
mfinal <- 400
boostingBreimanTrue <- boosting(formula = formula, data = Train.Sample, mfinal = mfinal, 
                          coeflearn = "Breiman", boos = TRUE, control = cntrl)
table(boostingBreimanTrue$class, vardep, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(boostingBreimanTrue$class == vardep) / length(vardep),2)
paste("Taux d'erreur Boosting Breiman - Train =",error,"%")
##Erreur Train = 19%

predboostBreiman <- predict.boosting(boostingBreimanTrue, newdata = Valid.Sample) 
predboostBreiman$confusion
predboostBreiman$error
##Erreur Validation = 23%

par(mfrow=c(1,1))
errorevol.train <- errorevol(boostingBreimanTrue, Train.Sample) 
errorevol.test <- errorevol(boostingBreimanTrue, Valid.Sample) 
plot(errorevol.test[[1]], type = "l", ylim = c(0, 0.5), main = "Adaboost error versus number of trees", 
     xlab = "Iterations",ylab = "Error", col = "red", lwd = 2) 
lines(errorevol.train[[1]], cex = 0.5, col = "blue", lty = 1, lwd = 2) 
legend("topright", c("test", "train"), col = c("red", "blue"), lty = 1, lwd = 2) 
abline(h = min(errorevol.test[[1]]), col = "red", lty = 2, lwd = 2) 
abline(h = min(errorevol.train[[1]]), col = "blue", lty = 2, lwd = 2)