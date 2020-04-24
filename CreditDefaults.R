# Setting up Path Directory

setwd("E:/MSc BUSINESS ANALYTICS/Final Year Project/Datasets")
Credit <- read.csv("application_trainsetv1.1.csv",na.strings = c("NA"," ","NaN","XNA"))
str(Credit)
summary(Credit)

# Loading the Librarires
install.packages("caTools")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("caret")
install.packages("Metrics")
install.packages("randomForest")

library(caTools)
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(stringr)
library(caret)
library(corrplot)
library(Metrics)
library(randomForest)

### Checking the number of missing values in each column
missing <- sapply(Credit, function(x) {sum(is.na(x))})
missing = as.data.frame(sort(subset(missing,missing>0),decreasing = TRUE))

missing <- sapply(Credit, function(x) {(sum(is.na(x))/length(x))*100})
missing = as.data.frame(sort(subset(missing,missing>150000),decreasing = TRUE))
#missing %>% gather(data = missing,k)


### Below are the columns in descending order with the number of missing values respectively #######


"LIVINGAPARTMENTS_AVG        LIVINGAPARTMENTS_MODE        LIVINGAPARTMENTS_MEDI      YEARS_BUILD_AVG          YEARS_BUILD_MODE 
210199                       210199                       210199                       204488                       204488 
YEARS_BUILD_MEDI            OWN_CAR_AGE                 LANDAREA_AVG                LANDAREA_MODE                LANDAREA_MEDI 
204488                       202929                       182590                       182590                       182590 
EXT_SOURCE_1               LIVINGAREA_AVG              LIVINGAREA_MODE             LIVINGAREA_MEDI      YEARS_BEGINEXPLUATATION_AVG 
173378                       154350                       154350                       154350                       150007 
YEARS_BEGINEXPLUATATION_MODE YEARS_BEGINEXPLUATATION_MEDI TOTALAREA_MODE            EXT_SOURCE_3        AMT_REQ_CREDIT_BUREAU_HOUR 
150007                       150007                       148431                        60965                        41519 
AMT_REQ_CREDIT_BUREAU_DAY   AMT_REQ_CREDIT_BUREAU_WEEK    AMT_REQ_CREDIT_BUREAU_MON    AMT_REQ_CREDIT_BUREAU_QRT   AMT_REQ_CREDIT_BUREAU_YEAR 
41519                        41519                        41519                        41519                        41519 
EXT_SOURCE_2              AMT_GOODS_PRICE                  AMT_ANNUITY              CNT_FAM_MEMBERS       DAYS_LAST_PHONE_CHANGE 
660                          278                           12                            2                            1 "


#Taking a copy of our original dataset

CreditNew <- Credit
####################### Removing NA value which are greater than 50%     #######################################

CreditNew$LIVINGAPARTMENTS_AVG <- NULL
CreditNew$LIVINGAPARTMENTS_MODE <- NULL
CreditNew$LIVINGAPARTMENTS_MEDI <- NULL

CreditNew$YEARS_BUILD_AVG <- NULL
CreditNew$YEARS_BUILD_MODE <- NULL
CreditNew$YEARS_BUILD_MEDI <- NULL

CreditNew$OWN_CAR_AGE <- NULL
CreditNew$LANDAREA_AVG <- NULL
CreditNew$LANDAREA_MODE <- NULL
CreditNew$LANDAREA_MEDI <- NULL


CreditNew$LIVINGAREA_AVG <- NULL
CreditNew$LIVINGAREA_MODE <- NULL
CreditNew$LIVINGAREA_MEDI <- NULL
CreditNew$YEARS_BEGINEXPLUATATION_AVG <- NULL
CreditNew$YEARS_BEGINEXPLUATATION_MODE <- NULL
CreditNew$YEARS_BEGINEXPLUATATION_MDEI <- NULL

###############################################################################################################


# AMT_GOODS_PRICE, DAYS_EMPLOYED

### Converting days value which is 365243 i.e infinity to NA
CreditNew$DAYS_EMPLOYED = na_if(CreditNew$DAYS_EMPLOYED,365243);

" Removing the NA values with mean for all the individual columns, the one in commented are the one's whose missing value percentage
  are very large, more than 50% and hance we should think before deleting those stuffs, if it is required or not ###########################"

CreditNew$EXT_SOURCE_1           = ifelse(is.na(CreditNew$EXT_SOURCE_1),mean(CreditNew$EXT_SOURCE_1,na.rm = TRUE),CreditNew$EXT_SOURCE_1)
CreditNew$DAYS_LAST_PHONE_CHANGE = ifelse(is.na(CreditNew$DAYS_LAST_PHONE_CHANGE),mean(CreditNew$DAYS_LAST_PHONE_CHANGE,na.rm = TRUE),CreditNew$DAYS_LAST_PHONE_CHANGE)
CreditNew$CNT_FAM_MEMBERS        = ifelse(is.na(CreditNew$CNT_FAM_MEMBERS),mean(CreditNew$CNT_FAM_MEMBERS,na.rm = TRUE),CreditNew$CNT_FAM_MEMBERS)
CreditNew$AMT_ANNUITY            = ifelse(is.na(CreditNew$AMT_ANNUITY),mean(CreditNew$AMT_ANNUITY,na.rm = TRUE),CreditNew$AMT_ANNUITY)
CreditNew$AMT_GOODS_PRICE        = ifelse(is.na(CreditNew$AMT_GOODS_PRICE),mean(CreditNew$AMT_GOODS_PRICE,na.rm = TRUE),CreditNew$AMT_GOODS_PRICE)
CreditNew$EXT_SOURCE_2           = ifelse(is.na(CreditNew$EXT_SOURCE_2),mean(CreditNew$EXT_SOURCE_2,na.rm = TRUE),CreditNew$EXT_SOURCE_2)
CreditNew$AMT_REQ_CreditNew_BUREAU_YEAR = ifelse(is.na(CreditNew$AMT_REQ_CreditNew_BUREAU_YEAR),mean(CreditNew$AMT_REQ_CreditNew_BUREAU_YEAR,na.rm = TRUE),CreditNew$AMT_REQ_CreditNew_BUREAU_YEAR)
CreditNew$AMT_REQ_CreditNew_BUREAU_QRT  = ifelse(is.na(CreditNew$AMT_REQ_CreditNew_BUREAU_QRT),mean(CreditNew$AMT_REQ_CreditNew_BUREAU_QRT,na.rm = TRUE),CreditNew$AMT_REQ_CreditNew_BUREAU_QRT)
CreditNew$AMT_REQ_CreditNew_BUREAU_MON  = ifelse(is.na(CreditNew$AMT_REQ_CreditNew_BUREAU_MON),mean(CreditNew$AMT_REQ_CreditNew_BUREAU_MON,na.rm = TRUE),CreditNew$AMT_REQ_CreditNew_BUREAU_MON)
CreditNew$AMT_REQ_CreditNew_BUREAU_WEEK = ifelse(is.na(CreditNew$AMT_REQ_CreditNew_BUREAU_WEEK),mean(CreditNew$AMT_REQ_CreditNew_BUREAU_WEEK,na.rm = TRUE),CreditNew$AMT_REQ_CreditNew_BUREAU_WEEK)
CreditNew$AMT_REQ_CreditNew_BUREAU_DAY  = ifelse(is.na(CreditNew$AMT_REQ_CreditNew_BUREAU_DAY),mean(CreditNew$AMT_REQ_CreditNew_BUREAU_DAY,na.rm = TRUE),CreditNew$AMT_REQ_CreditNew_BUREAU_DAY)
CreditNew$LIVINGAREA_MODE               = ifelse(is.na(CreditNew$LIVINGAREA_MODE),mean(CreditNew$LIVINGAREA_MODE,na.rm = TRUE),CreditNew$LIVINGAREA_MODE)
CreditNew$AMT_REQ_CreditNew_BUREAU_HOUR = ifelse(is.na(CreditNew$AMT_REQ_CreditNew_BUREAU_HOUR),mean(CreditNew$AMT_REQ_CreditNew_BUREAU_HOUR,na.rm = TRUE),CreditNew$AMT_REQ_CreditNew_BUREAU_HOUR)
CreditNew$EXT_SOURCE_3                  = ifelse(is.na(CreditNew$EXT_SOURCE_3),mean(CreditNew$EXT_SOURCE_3,na.rm = TRUE),CreditNew$EXT_SOURCE_3)
CreditNew$TOTALAREA_MODE                = ifelse(is.na(CreditNew$TOTALAREA_MODE),mean(CreditNew$TOTALAREA_MODE,na.rm = TRUE),CreditNew$TOTALAREA_MODE)
CreditNew$YEARS_BEGINEXPLUATATION_MEDI  = ifelse(is.na(CreditNew$YEARS_BEGINEXPLUATATION_MEDI),median(CreditNew$YEARS_BEGINEXPLUATATION_MEDI,na.rm = TRUE),CreditNew$YEARS_BEGINEXPLUATATION_MEDI)

#Replacing with median for some of the NA values
CreditNew$ORGANIZATION_TYPE             = as.numeric(CreditNew$ORGANIZATION_TYPE)
CreditNew$ORGANIZATION_TYPE             = ifelse(is.na(CreditNew$ORGANIZATION_TYPE),median(CreditNew$ORGANIZATION_TYPE, na.rm = TRUE),
                                                  CreditNew$ORGANIZATION_TYPE)
CreditNew$ORGANIZATION_TYPE             = as.factor(CreditNew$ORGANIZATION_TYPE)

CreditNew$AMT_REQ_CREDIT_BUREAU_HOUR    = ifelse(is.na(CreditNew$AMT_REQ_CREDIT_BUREAU_HOUR),median(CreditNew$AMT_REQ_CREDIT_BUREAU_HOUR, na.rm = TRUE),CreditNew$AMT_REQ_CREDIT_BUREAU_HOUR)
CreditNew$AMT_REQ_CREDIT_BUREAU_DAY     = ifelse(is.na(CreditNew$AMT_REQ_CREDIT_BUREAU_DAY),median(CreditNew$AMT_REQ_CREDIT_BUREAU_DAY, na.rm = TRUE),CreditNew$AMT_REQ_CREDIT_BUREAU_DAY)
CreditNew$AMT_REQ_CREDIT_BUREAU_WEEK    = ifelse(is.na(CreditNew$AMT_REQ_CREDIT_BUREAU_WEEK),median(CreditNew$AMT_REQ_CREDIT_BUREAU_WEEK, na.rm = TRUE),CreditNew$AMT_REQ_CREDIT_BUREAU_WEEK)
CreditNew$AMT_REQ_CREDIT_BUREAU_MON     = ifelse(is.na(CreditNew$AMT_REQ_CREDIT_BUREAU_MON),median(CreditNew$AMT_REQ_CREDIT_BUREAU_MON, na.rm = TRUE),CreditNew$AMT_REQ_CREDIT_BUREAU_MON)

CreditNew$AMT_REQ_CREDIT_BUREAU_QRT     = ifelse(is.na(CreditNew$AMT_REQ_CREDIT_BUREAU_QRT),median(CreditNew$AMT_REQ_CREDIT_BUREAU_QRT, na.rm = TRUE),CreditNew$AMT_REQ_CREDIT_BUREAU_QRT)
CreditNew$AMT_REQ_CREDIT_BUREAU_YEAR    = ifelse(is.na(CreditNew$AMT_REQ_CREDIT_BUREAU_YEAR),median(CreditNew$AMT_REQ_CREDIT_BUREAU_YEAR, na.rm = TRUE),CreditNew$AMT_REQ_CREDIT_BUREAU_YEAR)

CreditNew$DAYS_EMPLOYED                 = ifelse(is.na(CreditNew$DAYS_EMPLOYED),median(CreditNew$DAYS_EMPLOYED, na.rm = TRUE),CreditNew$DAYS_EMPLOYED)

CreditNew$CODE_GENDER                   = as.numeric(CreditNew$CODE_GENDER)
CreditNew$CODE_GENDER                   = ifelse(is.na(CreditNew$CODE_GENDER),median(CreditNew$CODE_GENDER, na.rm = TRUE),CreditNew$CODE_GENDER)
CreditNew$CODE_GENDER                   = as.factor(CreditNew$CODE_GENDER)


##################################################################################################################################3

#-------------------------------------          Convert Categorical into factors  ------------------------------------------------#

#convert <- c(3:6, 12:16,28,38,43:44,46:47)
#CreditNew[,convert] <- data.frame(apply(CreditNew[convert], 2, as.factor))

str(CreditNew)


# 1. Random Forest Algorithm

set.seed(101)
training1$TARGET = as.factor(training1$TARGET)
 
install.packages("caTools")
library(caTools)

split     = sample.split(CreditNew, SplitRatio = 0.4)
training1 = subset(CreditNew,split==TRUE)
training2 = subset(CreditNew,split==FALSE)

#Applying Random Forest
model_rf<-randomForest(TARGET ~ ., data = training1)
varImp(model_rf)


# 2. LASSO Logistics Regression Algorithm
install.packages("glmnet")
library(glmnet)

summary(training1)
Credit_X = model.matrix(~.-TARGET, training1)
Credit_Y = training1$TARGET
lasso_fit = glmnet(x = Credit_X, y = Credit_Y,family ="binomial",pmax = 30,alpha = 1 )
plot(lasso_cv, xvar = "lambda", label = TRUE)
coef(lasso_fit)



lasso_cv = cv.glmnet(x = Credit_X, y = Credit_Y,family ="binomial",type.measure = "class", nfolds = 5)

lasso_fit = glmnet(x = Credit_X, y = Credit_Y,family ="binomial",alpha = 1,pmax = 30, lambda = lasso_cv$lambda.min)

#List non zero coefficients
lasso_fit$beta[,1]

coef(lasso_cv, s= "lambda.min") 



coefficients = coef(lasso_cv, s= "lambda.min") 


#3. Logistic Regression
install.packages("mlbench")
library("mlbench")
library(caret)
model_glm <- glm(TARGET ~.-SK_ID_CURR, training1, family = "binomial")
summary(model_glm)

# To find out top variables with importance value

imp <- as.data.frame(varImp(model_glm))
imp <- data.frame(overall = imp$Overall,
                  names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]



#### 4. SVM RFE ###
 
split = sample.split(CreditNew, SplitRatio = 0.1)
training1 = subset(CreditNew,split==TRUE)
training2 = subset(CreditNew,split==FALSE)

training1 <- training1[, -nearZeroVar(training1)] ###makes diff

training3 = na.omit(training1)
training3$TARGET = as.factor(training3$TARGET)

is.na(training1)

set.seed(7)
rfeControl <- rfeControl(functions=rfFuncs,method="cv", number=10)
results <- rfe(training3[,-2], training3[,2],sizes = c(1:10), rfeControl=rfeControl,  method="svmRadial")
results

varImpPlot(results)

#The top 5 variables (out of 47):
#  EXT_SOURCE_2, EXT_SOURCE_1, OCCUPATION_TYPE, ORGANIZATION_TYPE, AMT_INCOME_TOTAL


print(results)
predictors <- predictors(results)

colnames(training3)[unique(which(is.na(training3[,-2]), arr.ind = TRUE)[,2])]


#The top 5 variables (out of 47):
#  EXT_SOURCE_2, EXT_SOURCE_1, OCCUPATION_TYPE, ORGANIZATION_TYPE, AMT_INCOME_TOTAL


print(results)
predictors <- predictors(results)



###############################     MAKING FINAL DECISION        ###########################################
# From the 4 models we have decided to take those variables that are common to atleast 3 of it.

# EXT_SOURCE_2, EXT_SOURCE_1, EXT_SOURCE_1, DAYS_EMPLOYED, DAYS_ID_PUBLISH, DAYS_LAST_PHONE_CHANGE, OCCUPATION_TYPE, 
# REGION_RATING_CLIENT_W_CITY, NAME_INCOME_TYPE, AMT_INCOME_TOTAL,DAYS_BIRTH,AMT_CREDIT,AMT_GOODS_PRICE,TARGET


###################################### Plot corrplot analysis #######################

install.packages("corrplot")
library(corrplot)

coorelation = corrplot(cor(CreditData),type = "upper",method="number",t1.cex=0.02,number.cex = 0.7, diag = FALSE)


############################################################################################################
#############                     MACHINE LEARNING MODELS                                   ################
#############                                                                               ################
############################################################################################################


############## 1. logistics Regression ###############################


combData3 <- cbind(CreditNew$EXT_SOURCE_1,CreditNew$EXT_SOURCE_2,CreditNew$EXT_SOURCE_3,CreditNew$DAYS_EMPLOYED,CreditNew$DAYS_ID_PUBLISH,
                   CreditNew$DAYS_LAST_PHONE_CHANGE,CreditNew$OCCUPATION_TYPE, CreditNew$REGION_RATING_CLIENT_W_CITY,
                   CreditNew$NAME_INCOME_TYPE,CreditNew$AMT_INCOME_TOTAL,CreditNew$DAYS_BIRTH,CreditNew$AMT_CREDIT,CreditNew$AMT_GOODS_PRICE,CreditNew$TARGET)

CreditData3 = as.data.frame(combData3)

CreditData_oldnames = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14")
CreditData3_newnames = c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","DAYS_EMPLOYED","DAYS_ID_PUBLISH",
             "DAYS_LAST_PHONE_CHANGE","OCCUPATION_TYPE","REGION_RATING_CLIENT_W_CITY",
             "NAME_INCOME_TYPE","AMT_INCOME_TOTAL","DAYS_BIRTH","AMT_CREDIT","AMT_GOODS_PRICE","TARGET")

for(i in 1:14) names(CreditData3)[names(CreditData3) == CreditData_oldnames[i]] = CreditData3_newnames[i]



### Splitting the data into Train and Testing Set

library(caTools)
result=sample.split(CreditData3$TARGET,SplitRatio = 0.7)

CreditData_train=CreditData3[result==TRUE,]

CreditData_test=CreditData3[result==FALSE,]

########### Generating the model ##########################

model_LR = glm(CreditData_train$TARGET~.,data =CreditData_train,family = "binomial")

### Prediction
modelLR_Predicted = predict(model_LR, newdata = CreditData_test[,-14],type = "response")

model.error <- mean((modelLR_Predicted - CreditData_test$TARGET) ^ 2)
model.error

#### Building Confusion Matrix #############################
modelPredDirection = rep(0,92253) 
modelPredDirection[modelLR_Predicted > 0.5] = 1

# Storing the value from confusion matrix to variable AccuracyLR
AccuracyLR = table(modelPredDirection,CreditData_test$TARGET)

install.packages("ROCR")
library("ROCR")    
pred <- prediction(modelLR_Predicted, CreditData_test$TARGET)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve ", xlab="Specificity", 
     ylab="Sensitivity")
abline(0, 1)


#################### Validation Set ###################################

CreditTestData = read.csv("application_test.csv",na.strings = c("NA"," ","NaN","XNA"))
TestData1 <- cbind(CreditTestData$EXT_SOURCE_1,CreditTestData$EXT_SOURCE_2,CreditTestData$EXT_SOURCE_3,CreditTestData$DAYS_EMPLOYED,CreditTestData$DAYS_ID_PUBLISH,
                   CreditTestData$DAYS_LAST_PHONE_CHANGE,CreditTestData$OCCUPATION_TYPE, CreditTestData$REGION_RATING_CLIENT_W_CITY,
                   CreditTestData$NAME_INCOME_TYPE,CreditTestData$AMT_INCOME_TOTAL,CreditTestData$DAYS_BIRTH,CreditTestData$AMT_CREDIT,CreditTestData$AMT_GOODS_PRICE)

summary(TestData1)

CreditTestData$EXT_SOURCE_1 = ifelse(is.na(CreditTestData$EXT_SOURCE_1),mean(CreditTestData$EXT_SOURCE_1,na.rm = TRUE),CreditTestData$EXT_SOURCE_1)
CreditTestData$EXT_SOURCE_2 = ifelse(is.na(CreditTestData$EXT_SOURCE_2),mean(CreditTestData$EXT_SOURCE_2,na.rm = TRUE),CreditTestData$EXT_SOURCE_2)
CreditTestData$EXT_SOURCE_3 = ifelse(is.na(CreditTestData$EXT_SOURCE_3),mean(CreditTestData$EXT_SOURCE_3,na.rm = TRUE),CreditTestData$EXT_SOURCE_3)


TestData = as.data.frame(TestData1)

oldnames = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13")
newnames = c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","DAYS_EMPLOYED","DAYS_ID_PUBLISH",
             "DAYS_LAST_PHONE_CHANGE","OCCUPATION_TYPE","REGION_RATING_CLIENT_W_CITY",
             "NAME_INCOME_TYPE","AMT_INCOME_TOTAL","DAYS_BIRTH","AMT_CREDIT","AMT_GOODS_PRICE")

for(i in 1:13) names(TestData)[names(TestData) == oldnames[i]] = newnames[i]


modelLR_Predict = predict(model_LR, newdata = TestData,type = "response")


############## 3. Artificial Neural Network ###############################

set.seed(1234567890)

library("neuralnet")

## build the neural network (NN)
creditnet <- neuralnet(TARGET~EXT_SOURCE_1+EXT_SOURCE_2+EXT_SOURCE_3+DAYS_EMPLOYED+DAYS_ID_PUBLISH+
                         DAYS_LAST_PHONE_CHANGE+OCCUPATION_TYPE+ REGION_RATING_CLIENT_W_CITY+
                         NAME_INCOME_TYPE+AMT_INCOME_TOTAL+DAYS_BIRTH+AMT_CREDIT+AMT_GOODS_PRICE,
                       data = CreditData_train, 
                       hidden = 2, lifesign = "minimal",
                       linear.output = FALSE, threshold = 0.1)


install.packages("ROCR")
library("ROCR")    
pred <- prediction(creditnet.results$net.result, CreditData_test$TARGET)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve for ANN Model ", xlab="Specificity", 
     ylab="Sensitivity")



## plot the NN
plot(creditnet, rep = "best")

## Testing the model generated

creditnet.results <- compute(creditnet, CreditData_test[,-14])

creditnet.results$net.result = as.data.frame(creditnet.results$net.result+0.85)

str(creditnet.results)



#Let's have a look at what the neural network produced:
  
Results <- data.frame(actual = CreditData_test$TARGET, prediction = creditnet.results$net.result)

Results[100:115, ]

#We can round to the nearest integer to improve readability:
  
Results$prediction <- round(Results$prediction)
Results[100:115, ]


#### Building Confusion Matrix #############################
roundedresults<-sapply(Results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)

# Storing the value from confusion matrix to variable AccuracyANN
AccuracyANN = table(actual,prediction)

############## 4. Classification Tree ###############################


library(rpart)


set.seed(101)
# ning1$TARGET = as.factor(training1$TARGET)
#CreditNew$TARGET = as.factor(CreditNew$TARGET)
combData3 <- cbind(CreditNew$EXT_SOURCE_1,CreditNew$EXT_SOURCE_2,CreditNew$EXT_SOURCE_3,CreditNew$DAYS_EMPLOYED,CreditNew$DAYS_ID_PUBLISH,
                   CreditNew$DAYS_LAST_PHONE_CHANGE,CreditNew$OCCUPATION_TYPE, CreditNew$REGION_RATING_CLIENT_W_CITY,
                   CreditNew$NAME_INCOME_TYPE,CreditNew$AMT_INCOME_TOTAL,CreditNew$DAYS_BIRTH,CreditNew$AMT_CREDIT,CreditNew$AMT_GOODS_PRICE,CreditNew$TARGET)

CreditData3 = as.data.frame(combData3)

CreditData_oldnames = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14")
CreditData3_newnames = c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","DAYS_EMPLOYED","DAYS_ID_PUBLISH",
                         "DAYS_LAST_PHONE_CHANGE","OCCUPATION_TYPE","REGION_RATING_CLIENT_W_CITY",
                         "NAME_INCOME_TYPE","AMT_INCOME_TOTAL","DAYS_BIRTH","AMT_CREDIT","AMT_GOODS_PRICE","TARGET")

for(i in 1:14) names(CreditData3)[names(CreditData3) == CreditData_oldnames[i]] = CreditData3_newnames[i]



### Splitting the data into Train and Testing Set

library(caTools)
result=sample.split(CreditData3$TARGET,SplitRatio = 0.7)

CreditData_train=CreditData3[result==TRUE,]

CreditData_test=CreditData3[result==FALSE,]

library(rpart)

#Applying Classifiction tree

model_tree<-rpart(TARGET ~ ., method="class",data = CreditData_train)

predict_Tree <- predict(model_tree, data = CreditData_test[,-14], type = "class")
table(predict_Tree,CreditData_test$TARGET)


CreditData_test$TARGET = as.factor(CreditData_test$TARGET)

#### Building Confusion Matrix #############################
confusionMatrix(predict_Tree,CreditData_test$TARGET)

# Storing the result value obtained directly from confusion matrix to variable AccuracyDT
AccuracyDT

library("ROCR")
pred <- prediction(predict(rp, type = "prob")[, 2], kyphosis$Kyphosis)

pred <- prediction(predict_Tree, CreditData_test$TARGET)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)

############## 4. Support Vector Machine ###############################
library("e1071")

combData3 <- cbind(CreditNew$EXT_SOURCE_1,CreditNew$EXT_SOURCE_2,CreditNew$EXT_SOURCE_3,CreditNew$DAYS_EMPLOYED,CreditNew$DAYS_ID_PUBLISH,
                   CreditNew$DAYS_LAST_PHONE_CHANGE,CreditNew$OCCUPATION_TYPE, CreditNew$REGION_RATING_CLIENT_W_CITY,
                   CreditNew$NAME_INCOME_TYPE,CreditNew$AMT_INCOME_TOTAL,CreditNew$DAYS_BIRTH,CreditNew$AMT_CREDIT,CreditNew$AMT_GOODS_PRICE,CreditNew$TARGET)

CreditData3 = as.data.frame(combData3)

CreditData_oldnames = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14")
CreditData3_newnames = c("EXT_SOURCE_1","EXT_SOURCE_2","EXT_SOURCE_3","DAYS_EMPLOYED","DAYS_ID_PUBLISH",
                         "DAYS_LAST_PHONE_CHANGE","OCCUPATION_TYPE","REGION_RATING_CLIENT_W_CITY",
                         "NAME_INCOME_TYPE","AMT_INCOME_TOTAL","DAYS_BIRTH","AMT_CREDIT","AMT_GOODS_PRICE","TARGET")

for(i in 1:14) names(CreditData3)[names(CreditData3) == CreditData_oldnames[i]] = CreditData3_newnames[i]

### Splitting the data into Train and Testing Set

library(caTools)
result=sample.split(CreditData3$TARGET,SplitRatio = 0.1)

CreditData_train=CreditData3[result==TRUE,]

CreditData_test=CreditData3[result==FALSE,]

#Fit a model. 

model_svm <- svm(TARGET ~ . , CreditData_train)

#Use the predictions on the data

pred <- predict(model_svm, CreditData_test[,-14], type="class")

table(CreditData_test[,14],pred)
CreditData_test$TARGET = as.factor(CreditData_test$TARGET)

#### Building Confusion Matrix #############################

modelPredDirection = rep(0,276760) 
modelPredDirection[pred > 0.5] = 1
table(as.factor(modelPredDirection),CreditData_test$TARGET)

# Storing the result value obtained from confusion matrix to variable AccuracySVM
AccuracySVM = table(as.factor(modelPredDirection),CreditData_test$TARGET)

############################################################################################################
#############          SELECTION OF THE BEST MACHINE LEARNING MODELS                         ################
#############                                                                               ################
############################################################################################################

Machine_Learning_Models = c("Logistics Regression","ANN Model","Decision Tree Model","SVM Model")
Accuracy = c(AccuracyLR,AccuracyANN,AccuracyDT,AccuracySVM)

df = data.frame(Machine_Learning_Models,Accuracy)
df

# It has beenfound that though all the 4 models predcited with close accuracies, since Neural network predicted better than
# other models, hence we believe this neural network can be a better algorithm.




