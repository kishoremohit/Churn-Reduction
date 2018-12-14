# Loading Important Libraries for The Project
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x, require, character.only = TRUE)
rm(x)

# Reading Data for Analysis
library('readxl')
Chunk_Test <- read.csv('Churn_Test.csv')
Chunk_Train <- read.csv('Churn_Train.csv')

str(Chunk_Train)
# Missing Data Analysis in Both Test and Train data
missing <- data.frame(apply(Chunk_Train,2,function(x){sum(is.na(x))}))
missing.test <- data.frame(apply(Chunk_Test,2,function(x){sum(is.na(x))}))

# Convertion of Variable to Numeric
Chunk_Train$phone.number <- as.numeric(Chunk_Train$phone.number)
Chunk_Test$phone.number <- as.numeric(Chunk_Test$phone.number)

# Convertion of Factor And Categorical data into Factor for Both Test and Train data
Factor_Name = c("state","international.plan","voice.mail.plan","Churn")

for(i in Factor_Name){
  if(class(Chunk_Train[,i])== 'factor'){
    Chunk_Train[,i] = factor(Chunk_Train[,i], labels = (1:length(levels(factor(Chunk_Train[,i])))))
  }
}

for(i in Factor_Name){
  if(class(Chunk_Test[,i])== 'factor'){
    Chunk_Test[,i] = factor(Chunk_Test[,i], labels = (1:length(levels(factor(Chunk_Test[,i])))))
  }
}
str(Chunk_Train)

# Storing Numerical Variable in Factor_Data for further analysis
Numeric_Index = sapply(Chunk_Train,is.numeric) #selecting only numeric
Numeric_Data = Chunk_Train[,Numeric_Index]
Numerical = colnames(Numeric_Data)

Numeric_IndexTest = sapply(Chunk_Test,is.numeric)    #test data
Numeric_TestData = Chunk_Test[,Numeric_IndexTest]
Numerical_Test=colnames(Numeric_TestData)

# Correlation Plot for Feature Selection
library('corrgram')

corrgram(Chunk_Train[,Numeric_Index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

corrgram(Chunk_Test[,Numeric_IndexTest], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot Test")

# Storing Factor Variable in Factor_Data for further analysis
Factor_Index = sapply(Chunk_Train,is.factor)
Factor_Data = Chunk_Train[,Factor_Index]

options(warn = -1)

# For loop to calculate Chi Sq. for Factor Variable in Dataset for Feature Selection
for (i in 1:4)
{
  print(names(Factor_Data)[i])
  print(chisq.test(table(Factor_Data$Churn,Factor_Data[,i]))) 
}

# Dropping all Unneccessary data based on Correlation Plot and method
Churn_Train = subset(Chunk_Train, select = -c(total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes, phone.number))
Churn_Test = subset(Chunk_Test, select = -c(total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes, phone.number))


# Creating a Lisr containing all the Numerical or Continuous variable in the data set
Continuous_Name = c("account.length","area.code","number.vmail.messages","total.day.calls","total.day.charge",
           "total.eve.calls","total.eve.charge","total.night.calls","total.night.charge","total.intl.calls", "total.intl.charge", 
           "number.customer.service.calls")

# Normalization for Train Data
for(i in Continuous_Name){
  print(i)
  Churn_Train[,i] = (Churn_Train[,i] - min(Churn_Train[,i]))/
    (max(Churn_Train[,i] - min(Churn_Train[,i])))
}

# Normalization for Test Data
for(i in Continuous_Name){
  print(i)
  Churn_Test[,i] = (Churn_Test[,i] - min(Churn_Test[,i]))/
    (max(Churn_Test[,i] - min(Churn_Test[,i])))
}

#-----------------------------------------------------------------------------#   
  #### Histogram Plot For Continuous Data After Normalization Train Data ####
#-----------------------------------------------------------------------------#   

qqnorm(Churn_Train$account.length)
hist(Churn_Train$account.length)
qqnorm(Churn_Train$number.vmail.messages)
hist(Churn_Train$number.vmail.messages)
qqnorm(Churn_Train$total.day.calls)
hist(Churn_Train$total.day.calls)
qqnorm(Churn_Train$total.day.charge)
hist(Churn_Train$total.day.charge)
qqnorm(Churn_Train$total.eve.calls)
hist(Churn_Train$total.eve.calls)
qqnorm(Churn_Train$total.eve.charge)
hist(Churn_Train$total.eve.charge)
qqnorm(Churn_Train$total.night.calls)
hist(Churn_Train$total.night.calls)
qqnorm(Churn_Train$total.night.charge)
hist(Churn_Train$total.night.charge)
qqnorm(Churn_Train$total.intl.charge)
hist(Churn_Train$total.intl.charge)
qqnorm(Churn_Train$total.intl.calls)
hist(Churn_Train$total.intl.calls)
qqnorm(Churn_Train$number.customer.service.calls)
hist(Churn_Train$number.customer.service.calls)

#-----------------------------------------------------------------------------#   
#### Histogram Plot For Continuous Data After Normalization Test Data ####
#-----------------------------------------------------------------------------#   

qqnorm(Churn_Test$account.length)
hist(Churn_Test$account.length)
qqnorm(Churn_Test$number.vmail.messages)
hist(Churn_Test$number.vmail.messages)
qqnorm(Churn_Test$total.day.calls)
hist(Churn_Test$total.day.calls)
qqnorm(Churn_Test$total.day.charge)
hist(Churn_Test$total.day.charge)
qqnorm(Churn_Test$total.eve.calls)
hist(Churn_Test$total.eve.calls)
qqnorm(Churn_Test$total.eve.charge)
hist(Churn_Test$total.eve.charge)
qqnorm(Churn_Test$total.night.calls)
hist(Churn_Test$total.night.calls)
qqnorm(Churn_Test$total.night.charge)
hist(Churn_Test$total.night.charge)
qqnorm(Churn_Test$total.intl.charge)
hist(Churn_Test$total.intl.charge)
qqnorm(Churn_Test$total.intl.calls)
hist(Churn_Test$total.intl.calls)
qqnorm(Churn_Test$number.customer.service.calls)
hist(Churn_Test$number.customer.service.calls)

rmExcept(c("Churn_Train","Churn_Test"))

# Copying Churn_Train to train and Churn_Test to test For better understanding of Data for ML Alg.
train = Churn_Train
test =  Churn_Test

  ###                          ###
 # Decision Tree Classification #
###                          ###

# Decision Tree Model On C5.0
C50_Dtree = C5.0(Churn ~., train, trials = 50, rules = TRUE)
summary(C50_Dtree)

# Writing all summary and Rules in Txt for better understanding of Model
write(capture.output(summary(C50_Dtree)), "C50_Dtree.txt")

# Prediction On Test Data
Prediction_C50 = predict(C50_Dtree, test[,-16], type = "class")

# Confusion Matrix For DT
CM_C50 = table(test$Churn, Prediction_C50)
confusionMatrix(CM_C50, positive = '2')

Accuracy_DT = (CM_C50[1,1]+CM_C50[2,2])/(CM_C50[1,1]+CM_C50[2,2]+CM_C50[1,2]+CM_C50[2,1])

#----- ACCURACY: 88.9 % FNR: 32.5 %   -------#

#--------------------------------------------------#
######## CROSS VALIDATION DECISION TREE ########
#--------------------------------------------------#

library('caret')
Folds = createFolds(train$Churn, k = 10)
CV_DT = lapply(Folds, function(x){
  train_fold = train[-x, ] 
  test_fold = train[x, ]
  C50_Dtree = C5.0(Churn ~., train_fold, trials = 50, rules = TRUE)
  Prediction_C50 = predict(C50_Dtree, test_fold[,-16], type = "class")
  CM_C50 = table(test_fold$Churn, Prediction_C50)
  confusionMatrix(CM_C50, positive = '2')
  accuracy_dt = (CM_C50[1,1]+CM_C50[2,2])/(CM_C50[1,1]+CM_C50[2,2]+CM_C50[1,2]+CM_C50[2,1])
  return(accuracy_dt)
  })

CV_Accuracy_DT = mean(as.numeric(CV_DT))

# CV ACCURACY DT: 95.4 %


#---------------------------------------------#
      ##### CLASS IMBALANCE PROBLEM #####
#--------------------------------------------#

#-------- BarPlot to visualize Class Imbalance Problem -------#
barplot(prop.table(table(train$Churn)),
        col = rainbow(2),
        main = "Class Distribution")

#-------- Importing ROSE (Random Over-Sampling Examples) Library for Class Imbalance -------#
library(ROSE)

#-------- Over Fitting of Class "2" for Better Sesitivity -------#
over <- ovun.sample(Churn~., data = train, method = "over", N = 5700)$data 
table(over$Churn)
#-------- Model Creation and Confusion Matrix-------#
C50_Over <- C5.0(Churn ~., data = over, trials = 50, rules = TRUE)
confusionMatrix(predict(C50_Over, test), test$Churn, positive = '2')

#-------- Under Fitting of Class "1" for Better Sesitivity -------#
under <- ovun.sample(Churn~., data = train, method = "under", N = 966)$data
table(under$Churn)
#-------- Model Creation and Confusion Matrix-------#
C50_Under <- C5.0(Churn ~., data = under, trials = 50, rules = TRUE)
confusionMatrix(predict(C50_Under, test), test$Churn, positive = '2')

#-------- Applying Both Under and Over Fitting for Better Sesitivity -------#
both <- ovun.sample(Churn~., data = train, method = "both",
                    N = 3333)$data
table(both$Churn)
#-------- Model Creation and Confusion Matrix-------#
C50_Both <- C5.0(Churn ~., data = both, trials = 50, rules = TRUE)
confusionMatrix(predict(C50_Both, test), test$Churn, positive = '2')

#-------- Applying Both Under and Over Fitting for Better Sesitivity Using ROSE -------#
rose <-  ROSE(Churn~., data = train, N = 3333)$data
table(rose$Churn)
#-------- Model Creation and Confusion Matrix-------#
C50_Rose <- C5.0(Churn ~., data = rose, trials = 50, rules = TRUE)
confusionMatrix(predict(C50_Rose, test), test$Churn, positive = '2')


  ###                          ###
 # Random Forest Classification #
###                          ###


# Random Forest Model based on 200 Tree observed by Plotting RF_Model
RF_Model <- randomForest(Churn ~.,data = train,
                         importance = TRUE,
                         proximity = T,
                         ntree = 500)
print(RF_Model)
attributes(RF_Model)
plot(RF_Model)

RF_Model <- randomForest(Churn ~.,data = train,
                         importance = TRUE,
                         proximity = T,
                         ntree = 200)

# Histogram For Number Of Nodes For the Trees
hist(treesize(RF_Model),
     main = "No. of Nodes for the Trees",
     col = "blue")

# Plottting Important Variable
varImpPlot(RF_Model,
           sort = T,
           main = 'Imp. Variable')

# Important Variable in Random Forest Based on MDAccuracy and Gini
importance(RF_Model)

# Most Varible Used While Creating Model
varUsed(RF_Model)

getTree(RF_Model,1,labelVar = T)

# Prediction On Test Data
Prediction_RF = predict(RF_Model, test[,-16])

# Confusion Matrix For Random Forest
confusionMatrix(Prediction_RF,test$Churn, positive = '2')

#----- ACCURACY: 87.8 %  FNR: 42.01 %  -------#

#---------------------------------------------------#
  ######## CROSS VALIDATION RANDOM FOREST ########
#---------------------------------------------------#
  
CV_RF = lapply(Folds, function(x){
  train_fold_RF = train[-x, ] 
  test_fold_RF = train[x, ]
  RF_Model <- randomForest(Churn ~.,data = train_fold_RF,
                           importance = TRUE,
                           proximity = T,
                           ntree = 200)
  Prediction_RF = predict(RF_Model, test_fold_RF[,-16])
  CM_RF = table(test_fold_RF$Churn, Prediction_RF)
  confusionMatrix(CM_RF, positive = '2')
  accuracy_rf = (CM_RF[1,1]+CM_RF[2,2])/(CM_RF[1,1]+CM_RF[2,2]+CM_RF[1,2]+CM_RF[2,1])
  return(accuracy_rf)
})

CV_Accuracy_RF = mean(as.numeric(CV_RF))

# CV ACCURACY RF: 94.1 %



#t <- tuneRF(train[,-16],train[,16],
#       stepFactor = 0.5,
#       plot = TRUE,
#       ntreeTry = 300,
#       trace = TRUE,
#       improve = 0.05)

treeList <- RF2List(RF_Model)
Ext_Rule = extractRules(treeList, train[,-16])
Ext_Rule[1:2,]
Readable = presentRules(Ext_Rule, colnames(train))
Readable[1:2,]
Rule_Metric = getRuleMetric(Ext_Rule, train[,-16], train$Churn)
Rule_Metric[1:2,]


  ###                                ###
 # Logistic Regression Classification #
###                                ###

# Logistic Regression Model
Log_Model <- glm(Churn ~.,data = train, family = 'binomial')
summary(Log_Model)

#Prediction Based on Test Data
Predict_Log <- predict(Log_Model,test,type = 'response')
pred_MissTest <- ifelse(Predict_Log > 0.5,2,1)

# Confusion Matrix For Logistic Regression
CM_LR <- table(Predicted = pred_MissTest, Actual = test$Churn)
CM_LR
1-sum(diag(CM_LR))/sum(CM_LR)
confusionMatrix(CM_LR, positive = '2')

# Error Rate
with(Log_Model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))


Accuracy_LR = (CM_LR[1,1]+CM_LR[2,2])/(CM_LR[1,1]+CM_LR[2,2]+CM_LR[1,2]+CM_LR[2,1])

#----- ACCURACY:86.3 %   FNR: 51.1 % -------#

#---------------------------------------------------------#
  ######## CROSS VALIDATION LOGISTIC REGRESSION ########
#---------------------------------------------------------#
  
CV_LR = lapply(Folds, function(x){
  train_fold_LR = train[-x, ] 
  test_fold_LR = train[x, ]
  Log_Model <- glm(Churn ~., data = train_fold_LR, family = 'binomial')
  Predict_Log <- predict(Log_Model, test_fold_LR,type = 'response')
  pred_MissTest <- ifelse(Predict_Log > 0.5,2,1)
  CM_LR <- table(Predicted = pred_MissTest, Actual = test_fold_LR$Churn)
  confusionMatrix(CM_LR,positive = '2')
  accuracy_lr = (CM_LR[1,1]+CM_LR[2,2])/(CM_LR[1,1]+CM_LR[2,2]+CM_LR[1,2]+CM_LR[2,1])
  return(accuracy_lr)
})

CV_Accuracy_LR = mean(as.numeric(CV_LR))

# CV LOG_REG ACCURACY: 85.8%


  ###                ###
 # KNN Classification #
###                ###

library(class)

#Creating A List Of NULL For Prediction and Error Rate
Prediction_KNN = NULL
Error.Rate = NULL

# For Loop to find Error Rate Based On K Between 1 to 20
for(i in 1:20){
  Prediction_KNN = knn(train[, 1:15], test[, 1:15], train$Churn, k=i)
  Error.Rate[i] = mean(test$Churn != Prediction_KNN)
}
print(Error.Rate)
K.Values <- 1:20
Error.DF <- data.frame(Error.Rate,K.Values)

# Plotting Of K Value And Error Rate For Best Suited K Value
ggplot(Error.DF ,aes(x=K.Values, y=Error.Rate)) + geom_point()+ geom_line(lty="dotted",color='red')

#Predioction On Test Data 
Prediction_KNN = knn(train[, 1:15], test[, 1:15], train$Churn, k = 5)
head(Prediction_KNN)

# Evaluation model for trained data and analysis of misclassification error rate.
mean(test$Churn != Prediction_KNN)

#Confusion matrix
CM_KNN = table(Prediction_KNN , test$Churn)
confusionMatrix(CM_KNN, positive = '2')

Accuracy_KNN = (CM_KNN[1,1]+CM_KNN[2,2])/(CM_KNN[1,1]+CM_KNN[2,2]+CM_KNN[1,2]+CM_KNN[2,1])

#----- ACCURACY: 86.08%   FNR: 54.2 %  -------#

#-----------------------------------------#
 ######## CROSS VALIDATION KNN ########
#-----------------------------------------#

CV_KNN = lapply(Folds, function(x){
  train_fold_KNN = train[-x, ] 
  test_fold_KNN = train[x, ]
  Prediction_KNN = knn(train_fold_KNN[, 1:15], test_fold_KNN[, 1:15], train_fold_KNN$Churn, k = 3)
  mean(test_fold_KNN$Churn != Prediction_KNN)
  CM_KNN = table(Prediction_KNN , test_fold_KNN$Churn)
  confusionMatrix(CM_KNN,positive = '2')
  accuracy_knn = (CM_KNN[1,1]+CM_KNN[2,2])/(CM_KNN[1,1]+CM_KNN[2,2]+CM_KNN[1,2]+CM_KNN[2,1])
  return(accuracy_knn)
})

CV_Accuracy_KNN = mean(as.numeric(CV_KNN))

#CV_Accuracy_KNN: 84.8 %

  ###                ###
 # SVM Classification #
###                ###

#SVM Model Creation
SVM_Model <- svm(Churn ~ ., data=train)
summary(SVM_Model)

# Prediction On Test Data
Prediction_SVM <- predict(SVM_Model,test[,-16])

# Confusion Matrix For SVM
SVM_Tab <- table(Prediction_SVM,test$Churn)
confusionMatrix(SVM_Tab, positive = '2')

Accuracy_SVM = (SVM_Tab[1,1]+SVM_Tab[2,2])/(SVM_Tab[1,1]+SVM_Tab[2,2]+SVM_Tab[1,2]+SVM_Tab[2,1])

#----- ACCURACY:88.9%   FNR: 88.75 %  -------#


#-----------------------------------------#
  ######## CROSS VALIDATION SVM ########
#-----------------------------------------#
  
  CV_SVM = lapply(Folds, function(x){
    train_fold_SVM = train[-x, ] 
    test_fold_SVM = train[x, ]
    SVM_Model <- svm(Churn ~ ., data=train_fold_SVM)
    Prediction_SVM <- predict(SVM_Model,test_fold_SVM[,-16])
    SVM_Tab <- table(Prediction_SVM,test_fold_SVM$Churn)
    confusionMatrix(SVM_Tab,positive = '2')
    accuracy_svm = (SVM_Tab[1,1]+SVM_Tab[2,2])/(SVM_Tab[1,1]+SVM_Tab[2,2]+SVM_Tab[1,2]+SVM_Tab[2,1])
    return(accuracy_svm)
  })

CV_Accuracy_SVM = mean(as.numeric(CV_SVM))

# CV_Accuracy_SVM: 87.2%


  ###                         ###
 # NAIVE BAYES Classification  #
###                         ###

library(e1071)

# Naive Bayes Model Accuracy
NB_Model = naiveBayes(Churn ~ ., data = train)

# Prediction On Test Data
NB_Prediction = predict(NB_Model, test[,1:15], type = 'class')

# Confusion Matrix For Naive Bayes
CM_NB = table(observed = test[,16], predicted = NB_Prediction)
confusionMatrix(CM_NB, positive = '2')

Accuracy_NB = (CM_NB[1,1]+CM_NB[2,2])/(CM_NB[1,1]+CM_NB[2,2]+CM_NB[1,2]+CM_NB[2,1])

#----- ACCURACY: 87.1%   FNR: 53.5 %  -------#


#-------------------------------------------------#
  ######## CROSS VALIDATION NAIVE BAYES ########
#-------------------------------------------------#
  
  CV_NB = lapply(Folds, function(x){
    train_fold_NB = train[-x, ] 
    test_fold_NB = train[x, ]
    NB_Model = naiveBayes(Churn ~ ., data = train_fold_NB)
    NB_Prediction = predict(NB_Model, test_fold_NB[,1:15], type = 'class')
    CM_NB = table(observed = test_fold_NB[,16], predicted = NB_Prediction)
    confusionMatrix(CM_NB,positive = '2')
    accuracy_nb = (CM_NB[1,1]+CM_NB[2,2])/(CM_NB[1,1]+CM_NB[2,2]+CM_NB[1,2]+CM_NB[2,1])
    return(accuracy_nb)
  })

CV_Accuracy_NB = mean(as.numeric(CV_NB))

# CV_Accuracy_NB: 87.3 %