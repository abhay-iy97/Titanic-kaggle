setwd("C://Users//MAHE//Desktop//TITANIC")
library(ggplot2)
train_input<-read.csv("train.csv",stringsAsFactors = FALSE)
test_input<-read.csv("test.csv",stringsAsFactors = FALSE)

train_input$IsTrainSet<-TRUE
test_input$IsTrainSet<-FALSE  


test_input$Survived<-'NA'
titanic.full<-rbind(train_input,test_input)

#CLEAN THE DATA
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S' #Replace NA with mode of col

age.median<- median(titanic.full$Age,na.rm=TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median  #Rep,ace NA with median

#fare.median<- median(titanic.full$Fare,na.rm=TRUE)
#titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median  

upper.whisker<-boxplot.stats(titanic.full$Fare)$stats[5]
upper.whisker

outlier.filter<-titanic.full$Fare < upper.whisker

titanic.full[outlier.filter,]
fare.equation="Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model<-lm(
  formula=fare.equation,
  data=titanic.full[outlier.filter,])


fare.row<-titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]

fare.predictions <-predict(fare.model,newdata=fare.row)

titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.predictions
#Categorical casting i.e using factors to create categories
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)  #Check if using Parch and SibSp improves model



#Split it back
train_input<-titanic.full[titanic.full$IsTrainSet==TRUE,]
test_input<-titanic.full[titanic.full$IsTrainSet==FALSE,]

train_input$Survived<-as.factor(train_input$Survived)

#Creating Predictor model

survived.equation<-"Survived~Pclass + Sex + Age + SibSp + Parch + Fare"
survived.formula<-as.formula(survived.equation)

k=5


train_input$ClusterId <- sample(1:k, nrow(train_input), replace = TRUE)
list <- 1:k
# prediction and test set data frames that we add to with each iteration over
# the folds
prediction <- data.frame()
testsetCopy <- data.frame()

library(randomForest)
#function for k fold
for(i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(train_input, ClusterId %in% list[-i])
  testset <- subset(train_input, ClusterId %in% c(i))
  
  #run a random forest model
  mymodel <- randomForest(formula = survived.equation, data=trainingset, ntree=500, mtry=3, nodesize=0.01*nrow(testset), na.action = na.exclude)
  
  #remove response column 1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
}

Survived<- predict(mymodel,newdata=test_input)
PassengerId<-test_input$PassengerId

output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

write.csv(output.df,file="kaggle_submission.csv",row.names = FALSE)