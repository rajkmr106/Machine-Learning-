# Improve the accuracy of Models

# Step 1. Find the Best accuracy models for the given dataset using trail and error
# step 2. Once mix of models are selected, use Parameter tuning to increase the results
# step 3. Another way to improve the accuracy of the models is comibing the predictive power of different models. 


# from the above points, step 3 is called as ensemble predictions. The most popular methods used for ensemble predictions are : 

# 1. Bagging
# 2. Boosting
# 3. Stacking

#Note: 
#library(caret)
#names(getModelInfo())


# Usecase: in this example, we demonstate each of the ensemple methods using ionosphere dataset

# data setup

dataset <- Ionosphere
dataset <- dataset[, -2]
#View(dataset)
str(dataset)
dataset$V1 <- as.numeric(as.character(dataset$V1))


#Boosting usecase:  example uses the following algs. 
# C5.0
# Stochastic Gradient Boosting Algorithm
library(caret)
tControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
seed = 7
metric = 'Accuracy'

#C5.0
set.seed(seed)
fit.c5 = train(Class ~ ., data = dataset, method = 'C5.0', metric = metric, trControl = tControl)

#Stochastic Gradient Boosting
#install.packages('caretEnsemble')
library(caretEnsemble)
set.seed(seed)
fit.gbm = train(Class ~ . , data = dataset, method = 'gbm', metric = metric, trControl = tControl, verbose = FALSE)

boostingResults = resamples(list(C50 = fit.c5, GBM = fit.gbm))
summary(boostingResults)
dotplot(boostingResults)

# Bagging Usecase: examples use the following alogs
# Bagged CART
# Random Forest

#install.packages("ipred")
#install.packages("prodlim", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
set.seed(seed)
fit.bcart = train(Class~., data = dataset, method = 'treebag', metric = metric, trControl = tControl)

set.seed(seed)
fit.rf = train(Class~., data = dataset, method = 'rf', metric = metric, trControl = tControl)

baggingResults <- resamples(list(TCART=fit.bcart, RF = fit.rf))
summary(baggingResults)
dotplot(baggingResults)


#Stacking Algorithms
# In this process, we will create a higher order functions by combining the predictions of multiple models. 

# Note: caretEnsemble package provide a function called caretStack() which will generate a higher order model by combing different caret models.

#Usecase: In this usecase, we will combine different models and build a higher order model 
#  Models used are: 
# Linear Discriminant Analysis (LDA)
# Classification and Regression Tree(CART)
# K-Nearest Neighbors (KNN)
# Support Vectors Machines (SVM Radial kernel)
# Logistic Regression (Generalized Linear Model or GLM)

library(caretEnsemble)
set.seed(seed)
tControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3, savePredictions = TRUE, classProbs = TRUE)
algStack = c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
models = caretList(Class~., data = dataset, trControl = tControl, methodList = algStack)
results = resamples(models)
summary(results)
dotplot(results)

modelCor(results)
splom(results)

#Creating Supervised model with GLM
sControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
set.seed(seed)
sGLM = caretStack(models, trControl = sControl, method = 'glm', metric = 'Accuracy')
print(sGLM)

#creating Supervised Model with RF
set.seed(seed)
sRF = caretStack(models, trControl = sControl, method = 'rf', metric = 'Accuracy')
print(sRF)




