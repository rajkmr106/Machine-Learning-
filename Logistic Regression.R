######################
##Logistic Regression#
######################
#Linear regression is used to predict continuous Y variables, logistic regression is used for binary classification.
###############################################################################################################

#Use Case: 
#Problem: Predict if an individual will earn more than $50K using logistic regression 

#Step 1: Import the data
#Step 2: Check for class bias
#Step 3: Create training and test samples
#Step 4: Compute information value to find out important variables
#Step 5: Build logit models and predict on test data
#Step 6: Model diagnostics

###############################################################################################################

#Step1: Import the data
getwd()
setwd("D://Madhu")
inputData = read.csv("adult.csv")
View(inputData)
str(inputData)
nrow(inputData)

#Step 2: Check for class bias
table(inputData$ABOVE50K)
#Note: Clearly, there is a class bias, a condition observed when the proportion of events is much smaller than proportion of non-events

#Step 3: Data Splitting: Create training and test samples
#Note: One way to address the problem of 'class bias' is to draw the 0's and 1's for the trainingData (development sample) in equal proportions

# set seed for repeatability of samples
set.seed(100)  

# 1's for training
salaryAbove50K <- inputData[which(inputData$ABOVE50K == 1), ]  
salaryAbove50KTrainIndex <- sample(1:nrow(salaryAbove50K), 0.7*nrow(salaryAbove50K))  
nrow(salaryAbove50K)

# 0's for training. Pick as many 0's as 1's
salaryBelow50K <- inputData[which(inputData$ABOVE50K == 0), ]
salaryBelow50KTrainIndex <- sample(1:nrow(salaryBelow50K), 0.7*nrow(salaryBelow50K))  
nrow(salaryBelow50K)

# Create Training Data
salaryAbove50KTrainData <- salaryAbove50K[salaryAbove50KTrainIndex, ]
salaryBelow50KTrainData <- salaryBelow50K[salaryBelow50KTrainIndex, ]
# row bind the 1's and 0's 
trainingData <- rbind(salaryAbove50KTrainData, salaryBelow50KTrainData)
nrow(trainingData)

# Create Test Data
salaryAbove50KTestData <- salaryAbove50K[-salaryAbove50KTrainIndex, ]  
salaryBelow50KTestData <- salaryBelow50K[-salaryBelow50KTrainIndex, ]
# row bind the 1's and 0's 
testData <- rbind(salaryAbove50KTestData, salaryBelow50KTestData)  
nrow(testData)

#View(trainingData)
#View(testData)

#Step 4: Compute information value to find out important variables
#Create Weight of Evidence(WOE) for categorical variables 
if(FALSE){
  for(factor_var in factor_vars){
    inputData[[factor_var]] <- WOE(X=inputData[, factor_var], Y=inputData$ABOVE50K)
  }
  head(inputData) 
}


#Compute Information Value(IV)
#Note: The smbinning::smbinning function converts a continuous variable into a categorical variable using recursive partitioning.

# segregate continuous and factor variables
#Textual data as factor/category variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")

#Numerical data as cotinuous variables
continuous_vars <- c("AGE", "FNLWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

# init for IV results
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  
iv_df

#install.packages("smbinning")
library(smbinning)
# compute IV for categoricals
for(factor_var in factor_vars){
  # WOE table
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  
  # check if some error occured
  if(class(smb) != "character"){ 
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

# sort
iv_df <- iv_df[order(-iv_df$IV), ] 
iv_df

#Step 5: Build logit model and predict on test data
#logit model
logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))

# predicted scores
predicted <- plogis(predict(logitMod, testData))

# predicted scores
predicted2 <- predict(logitMod, testData, type="response")  

#Note: plogis() vs predict
#

#install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
optCutOff

#Step 6: Model diagnostics
#Summary
summary(logitMod)

#VIF
car::vif(logitMod)

#Misclassification Error
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)

#ROC - Receiver Operating Characteristics
# ROC Curve traces the percentage of true positives accurately predicted by
# a given logit model as the prediction probability cutoff is lowered from 1 to 0. 
# For a good model, as the cutoff is lowered, it should mark more of actual 1's as positives and lesser of actual 0's as 1's. 
# So for a good model, the curve should rise steeply, indicating that the TPR (Y-Axis) increases faster than the FPR (X-Axis) as the cutoff score decreases.
# Greater the area under the ROC curve, better the predictive ability of the model.

plotROC(testData$ABOVE50K, predicted)
#Note: The above model has area under ROC curve 88.78%, which is pretty good.

#Concordance

#Ideally, the model-calculated-probability-scores of all actual Positive's, (aka Ones) should be greater 
#than the model-calculated-probability-scores of ALL the Negatives (aka Zeroes). Such a model is said to be perfectly concordant and a highly reliable one. 
#This phenomenon can be measured by Concordance and Discordance.

#In simpler words, of all combinations of 1-0 pairs (actuals), Concordance is the percentage of pairs, whose scores of actual positive's are greater than the scores of actual negative's. 
#For a perfect model, this will be 100%. So, the higher the concordance, the better is the quality of model.

Concordance(testData$ABOVE50K, predicted)

#Note: The above model with a concordance of 89.2% is indeed a good quality model.

#Specificity and Sensitivity

#Sensitivity (or True Positive Rate) is the percentage of 1's (actuals) correctly predicted by the model, 
#while, 
#specificity is the percentage of 0's (actuals) correctly predicted. 
sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)

#Specificity can also be calculated as 1 ??? False Positive Rate.
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)

#Confusion Matrix

confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)
