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
library(ggplot2)
library(Rmisc)
library(mice)
library(randomForest)
library(party)

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

# combine train and test set for feature transformations
train$test_train <- 'train'
test$test_train <- 'test'
combined<-merge(train, test, all.x =T, all.y = T)
# check the structure of the data
str(combined)
summary(combined)
# the id, class, sur should be factors
combined[,1]<-as.factor(combined[,1])
combined[,2]<-as.factor(combined[,2])
combined[,12]<-as.factor(combined[,12])


#Stats miss par colonne
miss.stat <- function(data=NULL,id=NULL){
  
  dataset <- data
  
  nb.na <- as.data.frame(sapply(dataset, function(x) sum(is.na(x))))
  nb.na$pct.miss <- nb.na[,1] / nrow(dataset) * 100
  nb.na <- cbind(row.names(nb.na),nb.na)
  nb.na <- arrange(nb.na,desc(nb.na$pct.miss))
  print(nb.na)
  
}
miss.stat(data = combined, id = 'PassengerId')

#the NA's for cabin and embarked are marked as empty values, change them to NA. 
combined$Cabin[which(combined$Cabin == "")] <- NA
combined$Embarked[which(combined$Embarked == "")] <- NA

#Excat Position from Cabin number
combined$CabinNum<-sapply(as.character(combined$Cabin),function(x) strsplit(x,'[A-Z]')[[1]][2])
combined$num<-as.numeric(combined$CabinNum)
num<-combined$num[!is.na(combined$num)]
Pos<-kmeans(num,3)
combined$CabinPos[!is.na(combined$num)]<-Pos$cluster
combined$CabinPos<-factor(combined$CabinPos)
levels(combined$CabinPos)<-c('Front','End','Middle')

# check the specific cabin levels
lvls<-levels(combined$Cabin)
table(lvls)
combined$Cabin<-as.character(combined$Cabin)
Cabin.levels<-LETTERS[1:7]
for(i in Cabin.levels){
  combined$Cabin[grep(i, combined$Cabin)]<-i
}
combined$Cabin<-as.factor(combined$Cabin) 

# check the structure now
table(combined$Cabin)
str(combined)

# person titles, extracting unique titles from name
combined$Title<-character(length = nrow(combined))
titles<-c("Mr.","Mrs.","Miss","Master","Special") # special is for Dr., Capt., Major., etc.
# replace all common titles
for(i in titles[1:4]){
  combined$Title[grep(i, combined$Name)] <- i
}
# replace others with all special titles
combined$Title[which(combined$Title == "")] <- "Special"
combined$Title<-factor(combined$Title, c("Mr.","Mrs.","Miss", "Master","Special"))
table(combined$Title)

# reorganize family features. family size = # of parent and child + # of sibings + 1
combined$Fam_size<-combined$SibSp + combined$Parch + 1
qplot(Fam_size, fill = Pclass, data = combined)

#FamilyId2
Surname<-sapply(as.character(combined$Name),function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(combined$Fam_size,Surname)
combined$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
combined$FamilyId2<-factor(FamilyId)

#Imputation Fare
fit.Fare<-rpart(Fare[!is.na(Fare)]~Pclass+Title+Sex+SibSp+Parch,data=combined[!is.na(combined$Fare),],
                method='anova')
# display the results
printcp(fit.Fare) 
predict(fit.Fare,combined[is.na(combined$Fare),])
summary(combined$Fare)
combined$Fare[is.na(combined$Fare)]<-predict(fit.Fare,combined[is.na(combined$Fare),])

#Imputation Embarked
table(combined$Embarked)
combined$Embarked[is.na(combined$Embarked)]<-'S'

#Imputation Age
fit.Age<-rpart(Age[!is.na(Age)]~Pclass+Title+Sex+SibSp+Parch+Fare,data=combined[!is.na(combined$Age),],
               method='anova')
summary(predict(fit.Age,combined[is.na(combined$Age),]))
summary(combined$Age)
combined$Age[is.na(combined$Age)]<-predict(fit.Age,combined[is.na(combined$Age),])


#Mother/Father/Child
combined$Mother<-0
combined$Mother[combined$Sex=='female' & combined$Parch>0 & combined$Age>18 & combined$Title!='Miss']<-1
combined$Father<-0
combined$Father[combined$Sex=='male' & combined$Parch>0 & combined$Age>18 & combined$Title!='Master']<-1
combined$Child<-0
combined$Child[combined$Parch>0 & combined$Age<=18]<-1

#factorize the categorical variables
combined<-transform(combined,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Father=factor(Father),
                Child=factor(Child),
                FamilyId2=factor(FamilyId2),
                SibSp=factor(SibSp),
                Parch=factor(Parch),
                Cabin=factor(Cabin),
                CabinPos=factor(CabinPos)
)

#split train/test data
train<-combined[combined$test_train=='train',]
test<-combined[combined$test_train=='test',]
train$Survived<-factor(train$Survived)

#Label cible
group.f <- factor(train$Survived, levels= c(1,0), labels = c("Yes", "No"))


#Stats descriptives sur var. continues

  #Scatterplot
dta <- train[,c('Fare','Age')] # get data
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r)
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" ) 

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

  #Distribution bivariee (Boxplot, Violinplot, Densites Kernel) 
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

dist.plot(data=train[,c('Fare','Age')],var.name='Fare')
dist.bivar.plot (data = train[,c('Fare','Age','Survived')], var.name = 'Fare', group.name = 'Survived')

dist.plot(data=train[,c('Fare','Age')],var.name='Age')
dist.bivar.plot (data = train[,c('Fare','Age','Survived')], var.name = 'Age', group.name = 'Survived')


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

train$Survived <- as.numeric(as.character(train$Survived)) #format numerique obligatoire
cross.bivar.plot (data = train,var.name = 'Survived', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Pclass', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Sex', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'SibSp', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Parch', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Embarked', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Mother', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Father', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Child', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Title', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'Cabin', group.name = 'Survived')
cross.bivar.plot (data = train, var.name = 'CabinPos', group.name = 'Survived')


#Echantillons apprentissage (80%) / validation (20%)
train$Survived <- factor(train$Survived) #obligatoire pour boosting

set.seed(36)
trainIndex <- sample.split(train$Survived, SplitRatio=80/100)

Train.Sample <- train[trainIndex,]
Valid.Sample  <- train[!trainIndex,]

prop.table(table(train$Survived))
prop.table(table(Train.Sample$Survived))
prop.table(table(Valid.Sample$Survived))


#Adaboost
  #BREIMAN & BOOTSTRAP = TRUE
formula <- Survived ~ Age + Fare + Pclass + Sex + SibSp + Parch + Embarked + Mother + Father + Child + Title + Cabin + CabinPos
formula <- Survived ~ Age + Fare + Pclass + Sex + SibSp + Parch + Embarked + Mother + Father + Child + Title + Cabin + CabinPos + FamilyId2 
vardep <- Train.Sample[ , as.character(formula[[2]])]
cntrl <- rpart.control(maxdepth = 1, minsplit = 0, cp = -1) 
mfinal <- 150

boostingBreimanTrue <- boosting(formula = Survived ~ Age, data = Train.Sample, mfinal = mfinal, 
                          coeflearn = "Breiman", boos = TRUE, control = cntrl)
barplot(sort(boostingBreimanTrue$importance, decreasing = TRUE), main = "Variables Relative Importance", 
        col = "lightblue", horiz = TRUE, las = 1, cex.names = .6, xlim = c(0, 100))
table(boostingBreimanTrue$class, vardep, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(boostingBreimanTrue$class == vardep) / length(vardep),2)
paste("Taux d'erreur Boosting Breiman - Train =",error,"%")
##Erreur Train = 20%

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

  #BREIMAN & BOOTSTRAP = FALSE
boostingBreimanFalse <- boosting(formula = formula, data = Train.Sample, mfinal = mfinal, 
                                coeflearn = "Breiman", boos = FALSE, control = cntrl)
barplot(sort(boostingBreimanFalse$importance, decreasing = TRUE), main = "Variables Relative Importance", 
        col = "lightblue", horiz = TRUE, las = 1, cex.names = .6, xlim = c(0, 100))
table(boostingBreimanFalse$class, vardep, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(boostingBreimanFalse$class == vardep) / length(vardep),2)
paste("Taux d'erreur Boosting Breiman - Train =",error,"%")
##Erreur Train = 18%

predboostBreiman <- predict.boosting(boostingBreimanFalse, newdata = Valid.Sample) 
predboostBreiman$confusion
predboostBreiman$error
##Erreur Validation = 18%

par(mfrow=c(1,1))
errorevol.train <- errorevol(boostingBreimanFalse, Train.Sample) 
errorevol.test <- errorevol(boostingBreimanFalse, Valid.Sample) 
plot(errorevol.test[[1]], type = "l", ylim = c(0, 0.5), main = "Adaboost error versus number of trees", 
     xlab = "Iterations",ylab = "Error", col = "red", lwd = 2) 
lines(errorevol.train[[1]], cex = 0.5, col = "blue", lty = 1, lwd = 2) 
legend("topright", c("test", "train"), col = c("red", "blue"), lty = 1, lwd = 2) 
abline(h = min(errorevol.test[[1]]), col = "red", lty = 2, lwd = 2) 
abline(h = min(errorevol.train[[1]]), col = "blue", lty = 2, lwd = 2)

  #FREUND & BOOTSTRAP = TRUE
boostingFreundTrue <- boosting(formula = formula, data = Train.Sample, mfinal = mfinal, 
                                 coeflearn = "Freund", boos = TRUE, control = cntrl)
barplot(sort(boostingFreundTrue$importance, decreasing = TRUE), main = "Variables Relative Importance", 
        col = "lightblue", horiz = TRUE, las = 1, cex.names = .6, xlim = c(0, 100))
table(boostingFreundTrue$class, vardep, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(boostingFreundTrue$class == vardep) / length(vardep),2)
paste("Taux d'erreur Boosting Freund - Train =",error,"%")
##Erreur Train = 19%

predboostFreund <- predict.boosting(boostingFreundTrue, newdata = Valid.Sample) 
predboostFreund$confusion
predboostFreund$error
##Erreur Validation = 20%

par(mfrow=c(1,1))
errorevol.train <- errorevol(boostingFreundTrue, Train.Sample) 
errorevol.test <- errorevol(boostingFreundTrue, Valid.Sample) 
plot(errorevol.test[[1]], type = "l", ylim = c(0, 0.5), main = "Adaboost error versus number of trees", 
     xlab = "Iterations",ylab = "Error", col = "red", lwd = 2) 
lines(errorevol.train[[1]], cex = 0.5, col = "blue", lty = 1, lwd = 2) 
legend("topright", c("test", "train"), col = c("red", "blue"), lty = 1, lwd = 2) 
abline(h = min(errorevol.test[[1]]), col = "red", lty = 2, lwd = 2) 
abline(h = min(errorevol.train[[1]]), col = "blue", lty = 2, lwd = 2)


  #FREUND & BOOTSTRAP = FALSE
boostingFreundFalse <- boosting(formula = formula, data = Train.Sample, mfinal = mfinal, 
                               coeflearn = "Freund", boos = FALSE, control = cntrl)
barplot(sort(boostingFreundFalse$importance, decreasing = TRUE), main = "Variables Relative Importance", 
        col = "lightblue", horiz = TRUE, las = 1, cex.names = .6, xlim = c(0, 100))
table(boostingFreundFalse$class, vardep, dnn = c("Predicted Class", "Observed Class"))
error <- round(1 - sum(boostingFreundFalse$class == vardep) / length(vardep),2)
paste("Taux d'erreur Boosting Freund - Train =",error,"%")
##Erreur Train = 16%

predboostFreundFalse <- predict.boosting(boostingFreundFalse, newdata = Valid.Sample) 
predboostFreundFalse$confusion
predboostFreundFalse$error
##Erreur Validation = 19%

par(mfrow=c(1,1))
errorevol.train <- errorevol(boostingFreundFalse, Train.Sample) 
errorevol.test <- errorevol(boostingFreundFalse, Valid.Sample) 
plot(errorevol.test[[1]], type = "l", ylim = c(0, 0.5), main = "Adaboost error versus number of trees", 
     xlab = "Iterations",ylab = "Error", col = "red", lwd = 2) 
lines(errorevol.train[[1]], cex = 0.5, col = "blue", lty = 1, lwd = 2) 
legend("topright", c("test", "train"), col = c("red", "blue"), lty = 1, lwd = 2) 
abline(h = min(errorevol.test[[1]]), col = "red", lty = 2, lwd = 2) 
abline(h = min(errorevol.train[[1]]), col = "blue", lty = 2, lwd = 2)



#Prediction echantillon TEST : modele Adaboost + Freund + Bootstrap = FALSE
boostingFreundFalse <- boosting(formula = formula, data = rbind(Train.Sample,Valid.Sample), mfinal = mfinal, 
                                coeflearn = "Freund", boos = FALSE, control = cntrl)
predboostFreund <- predict.boosting(boostingFreundFalse, newdata = test)
test <- cbind(test,predboostFreund$class,predboostFreund$prob)
test <- rename(test,c("predboostFreund$class" = "boostingFreundFalse.class"))
Survived <- ifelse(test$boostingFreundFalse.class=="0",0,1)


#Fichier Submission
submission <- data.frame(test$PassengerId,Survived)
submission <- rename(submission, c("test.PassengerId"="PassengerId"))
table(Survived)

write.csv(submission, file = paste(chemin,"submission.csv"),row.names = FALSE)



#cforest (conditional inference tree) method
fit.cf<-cforest(Survived~FamilyId2+CabinPos+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+
                  Title+Mother+Child,data=rbind(Train.Sample,Valid.Sample),
                controls=cforest_unbiased(ntree=500, mtry=3))

#write submission
test$Survived<-as.numeric(as.character(predict(fit.cf,test,OOB=TRUE,type='response')))
submission<-data.frame(test$PassengerId,test$Survived)
submission <- rename(submission, c("test.PassengerId"="PassengerId","test.Survived"="Survived"))
write.csv(submission,file = paste(chemin,"submission_cf.csv"),row.names = FALSE)
