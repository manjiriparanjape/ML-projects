############################ MNIST Digit Recoognition ############################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building and Cross Validation with tuning - Linear SVM
# 4. Model Building and Cross Validation with tuning - Non-Linear - Polynomial SVM
# 4. Model Building and Cross Validation with tuning - Non-Linear - Radial SVM

#####################################################################################

# 1. Business Understanding: 

# The handwritten digits are provided as dataset with the pixel values ( 28 X 28 = 784) configured as features

# Goal is to develop a model that can correctly identify the digit (between 0-9) written in an image


#####################################################################################

#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")

remove(list=ls())


library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

setwd("~/Assignment-SVM")
# ***************************************************************************************************
# 2 - DATA UNDERSTANDING AND PREPARTION

#****************************************************************************************************

#LOAD rar file provided @ https://learn.upgrad.com/v/course/126/session/18277/segment/92911 to local directory
# unzip to get train and test csv files in local directory

#Load the training dataset from local directory, account for no header condition while loading
train <- read.csv("mnist_train.csv" , header = FALSE) 

#Check dataset dimensions
dim(train) #Dataset is huge with 60,000 rows and 785 columns 

#Check structure of the dataset
str(train) # V1 is the target variable while rest all (784) are features

#Check the data distribution for target variable
table(train$V1) # the data is approximately similar for all target variables
#0    1    2    3    4    5    6    7    8    9 
#5923 6742 5958 6131 5842 5421 5918 6265 5851 5949

# ***************************************************************************************************
# 3 - DATA UNDERSTANDING AND PREPARTION

#****************************************************************************************************

# Since the dataset provided is huge, we will sample 15% of the data for training purpose by using random sampling
set.seed(101)
train1 <- sample_n(train, 9000, replace=FALSE)

# Confirm there is enough training data for each target variable
table(train1$V1) # good sampling for all values of target variable confirmed
#   0    1    2    3    4    5    6    7    8    9 
# 915 1025  901  928  833  786  862  966  872  912 


#Load test data set and again sample 15% of our testing
test <- read.csv("mnist_test.csv", header = FALSE)
dim(test)
test1 <- sample_n(test, 1500, replace=FALSE)

#Check for NA values
sapply(train1, function(x) sum(is.na(x))) #No NA values found

#Check duplicates
sum(duplicated(train1)) #No duplicates found

#Check all 0 - then the data is invalid
apply(train1,1, sum) #No such rows found

# Feature Normalization for train and test data
max(train1[ ,2:ncol(train)]) # max pixel value is 255, lets use this to scale data
train1[ , 2:ncol(train1)] <- train1[ , 2:ncol(train1)]/255
test1 [ , 2:ncol(test1)]<- test1[ , 2:ncol(test1)]/255

#Convert target variable to factor variable for train and test data
train1[,'V1']<-factor(train1[,'V1'])  
test1[,'V1']<-factor(test1[,'V1'])

#********************************************************************************************************
#********************************************************************************************************
# Start with using Simple Linear Kernel SVM to get a feel for accuracy that can be obtained using 
# default parameters C=1

#?ksvm
model_linear <- ksvm(V1~ ., data = train1, method = "svmLinear" ,scaled = FALSE, C=1) #Accuracy 0.962

# Predicting the model results 
eval_linear<- predict(model_linear, test1)

# Confusion Matrix - Finding accuracy, Sensitivity and specificity
confusionMatrix(eval_linear, test1$V1)

# Change C =5

model_linear <- ksvm(V1~ ., data = train1, scaled = FALSE, method = "svmLinear", C=5) # Accuracy 0.9733, so better fit
eval_linear<- predict(model_linear, test1)
confusionMatrix(eval_linear, test1$V1)

# Cross validation - Use train function in package caret
# Set parameters for train function - trainControl, C, metric

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy" # our Evaluation metric is Accuracy
grid <- expand.grid(C=c( 0.01, 0.1, 0.2, 0.5, 1 ,2 ,5 ,10))

fit.svm <- train(V1~., data=train1,  scaled = FALSE, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

#  C      Accuracy   Kappa    
#0.01  0.9167777  0.9074768
#0.10  0.9191108  0.9100644
#0.20  0.9148878  0.9053679
#0.50  0.9061072  0.8956028
#1.00  0.9011077  0.8900440
#2.00  0.8992201  0.8879432
#5.00  0.8988864  0.8875718
#10.00  0.8988864  0.8875718
#The final value used for the model was C = 0.1
print(fit.svm)
plot(fit.svm)


# Use cross validation results to predict outcome on test data

model_1<- predict(fit.svm, test1)
confusionMatrix(model_1, test1$V1) 
#Accuracy 0.944 , sensitivity: 0.897 - 0.9948 , specificity: 0.991 - 0.995
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9800   0.9948  0.89796  0.96212  0.91724  0.92958  0.97163  0.95425  0.92424   0.8982
#Specificity            0.9956   0.9969  0.99187  0.99269  0.99188  0.99264  0.99411  0.99406  0.99415   0.9940

# OVERALL, THE LINEAR SVM WORKS GOOD(Accuracy 0.944 for test data) 
#BUT LETS CHECK IF WE CAN IMPROVE THE PERFORMANCE USING NON LINEAR SVM

#********************************************************************************************************
#********************************************************************************************************
# Polynomial kernel 

str(train1)
model_poly <- ksvm(V1 ~ ., data =train1, scaled=FALSE, kernel = "polydot")

# Predicting the model results 
eval_poly<- predict(model_poly, test1)

#confusion matrix - RBF Kernel
confusionMatrix(eval_poly,test1$V1) #Accuracy = .9313 , sensitivity and specificity  in range 0.877 - 0.994


trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy" # our Evaluation metric is Accuracy
#set.seed(101)

# Making grid of C, degree and scale values. 
grid = expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(2, 3),scale = c( -10, -1, 1, 10))

# Performing 5-fold cross validation with parallel processing
#install.packages("parallel")
#install.packages("doParallel")

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

trainControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

fit.svm_poly <- train(V1 ~ ., data = train1, metric = metric, scaled = FALSE, method = "svmPoly",tuneGrid = grid,
                      trControl = trainControl, preProcess = NULL)

#De-register parallel processing cluster
stopCluster(cluster)
registerDoSEQ()

# Printing cross validation result
print(fit.svm_poly)
#C    degree  scale  Accuracy     Kappa     
#0.01  2       -10    0.953889001   0.9487342
#0.01  2        -1    0.953555359   0.9483629
#0.01  2         1    0.954000421   0.9488582
#0.01  2        10    0.953777890   0.9486106
#0.01  3       -10    0.002222408  -0.1224690
#0.01  3        -1    0.002333457  -0.1222946
#0.01  3         1    0.951222025   0.9457661
#0.01  3        10    0.950333259   0.9447770
#0.10  2       -10    0.953889001   0.9487342
#0.10  2        -1    0.953555359   0.9483629
#0.10  2         1    0.954000421   0.9488582
#0.10  2        10    0.953777890   0.9486106
#0.10  3       -10    0.002222408  -0.1224690
#0.10  3        -1    0.002333457  -0.1222946
#0.10  3         1    0.951222025   0.9457661
#0.10  3        10    0.950333259   0.9447770
#1.00  2       -10    0.953889001   0.9487342
#1.00  2        -1    0.953555359   0.9483629
#1.00  2         1    0.954000421   0.9488582
#1.00  2        10    0.953777890   0.9486106
#1.00  3       -10    0.002222408  -0.1224690
#1.00  3        -1    0.002333457  -0.1222946
#1.00  3         1    0.951222025   0.9457661
#1.00  3        10    0.950333259   0.9447770
#10.00  2       -10    0.953889001   0.9487342
#10.00  2        -1    0.953555359   0.9483629
#10.00  2         1    0.954000421   0.9488582
#10.00  2        10    0.953777890   0.9486106
#10.00  3       -10    0.002222408  -0.1224690
#10.00  3        -1    0.002333457  -0.1222946
#10.00  3         1    0.951222025   0.9457661
#10.00  3        10    0.950333259   0.9447770

#The final values used for the model were degree = 2, scale = 1 and C = 0.01 ; Accuracy = 0.954


# Plotting model results
plot(fit.svm_poly)
model_3<- predict(fit.svm_poly, test1) #Accuracy : 0.968 which is improvement over the Linear SVM
confusionMatrix(model_3, test1$V1)
#Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9800   0.9948  0.94558  0.98485  0.95862  0.96479  0.99291   0.9804  0.95455   0.9222
#Specificity            1.0000   0.9954  0.99778  0.99488  0.99631  0.99558  0.99558   0.9963  0.99488   0.9977

#*****************************************************************************************
#****************************************************************************************

#RBF Kernel

#Specify kernel as rbfdot with default value for C=1 and sigma=0.1
model_rbf <- ksvm(V1 ~ ., data =train1, scaled=FALSE, kernel = "rbfdot")

# Predicting the model results 
eval_rbf<- predict(model_rbf, test1)

#confusion matrix - RBF Kernel
confusionMatrix(eval_rbf,test1$V1) #Accuracy = 0.962, sensitivity and specificity also look good in range 0.95 - 0.99

#Hyperparameter tuning and Cross Validation - Non-Linear - RBF Kernel 

#Set the parameters for using train function of caret package

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy" 
#set.seed(101)
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=c(1,2,5,10,100))

# Performing 5-fold cross validation with parallel processing
#library(parallel)
#library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

trainControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

fit.svm_radial <- train(V1~., data=train1, scaled = FALSE, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

#De-register parallel processing cluster
stopCluster(cluster)
registerDoSEQ()

print(fit.svm_radial) 
plot(fit.svm_radial)


#sigma  C    Accuracy   Kappa    
#0.01     1  0.9492247  0.9435525
#0.01     2  0.9552233  0.9502199
#0.01     5  0.9580014  0.9533079
#0.01    10  0.9583349  0.9536788
#0.01   100  0.9572242  0.9524444
#0.02     1  0.9595569  0.9550393
#0.02     2  0.9636678  0.9596088
#0.02     5  0.9644465  0.9604745
#0.02    10  0.9644467  0.9604748
#0.02   100  0.9644467  0.9604748
#0.03     1  0.9612228  0.9568919
#0.03     2  0.9645571  0.9605982
#0.03     5  0.9645576  0.9605989
#0.03    10  0.9645576  0.9605989
#0.03   100  0.9645576  0.9605989
#0.04     1  0.9616668  0.9573852
#0.04     2  0.9640006  0.9599793
#0.04     5  0.9642234  0.9602274
#0.04    10  0.9642234  0.9602274
#0.04   100  0.9642234  0.9602274
#0.05     1  0.9591115  0.9545453
#0.05     2  0.9602228  0.9557806
#0.05     5  0.9604452  0.9560280
#0.05    10  0.9604452  0.9560280
#0.05   100  0.9604452  0.9560280
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.03 and C = 5. -- 0.9645

model_2<- predict(fit.svm_radial, test1) #Accuracy : 0.9773; Sensitivity : 0.94-1:00, Specificity : 0.9969-1:00
confusionMatrix(model_2, test1$V1)
#Statistics by Class:
  
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity            0.9800   0.9948  0.96599  1.00000  0.95862  0.97183   1.0000   0.9804  0.98485   0.9401
#Specificity            1.0000   0.9969  0.99630  0.99708  0.99779  0.99779   0.9978   0.9970  0.99635   0.9977

#********************************************************************************************************
#********************************************************************************************************
#FINAL MODEL
#Comparing Models to Get the Best Fit Model

# Linear SVM
# Final C = 0.1 ; Accuracy = 0.944 (Test Data)

# Poly SVM
# Final degree = 2, scale = 1 and C = 0.01 ; Accuracy =0.968 (Test Data)

# Radial SVM
# Final sigma = 0.03 and C = 5; Accuracy 0.9773 (Test Data)

final_model = fit.svm_radial

#Looking at above models, Radial SVM looks like the best model with highest accuaracy. Further fine tuning of 
# sigma may help improve accuracy but it would require more computations and time.

#*********************************************************************************************************
#********************************************************************************************************