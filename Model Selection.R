#This usecase explores the way to select bestmodel from different set of models created on the same dataset. 


#Data Preparation
getwd()
setwd("D://Madhu")
inputData <- read.csv("ozone2.csv")
# Y variable
y_df <- inputData['ozone_reading']  
y_df
nrow(y_df)
#X variables
x_df <- inputData[, !names(inputData) %in% "ozone_reading" ]  
x_df
nrow(x_df)
head(inputData)


#Step Wise Regression 
#In this process, we pass a full model to step function, and it iterates though 
#full scope of response variables(backwords direction  by deafault)
baseModel <- lm(ozone_reading ~ . , data = inputData)
summary(baseModel)

#In each iteration, multiple models are built by dropping each of the x variable at a time. 
#For every model, AIC is calculated and the model with least AIC is retained for next iteration. 
finalModel <- step(baseModel)
summary(finalModel)


#vif of each response variable after step wise regression 
vifs <- car::vif(finalModel)
print(vifs)

#Multicollinearity and Statistical Significance

#Significant response variables after step wise regression
signifVars <- names(vifs)
signifVars

##Recursively remove variables with VIF > 4
# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(vifs > 4)){
  # get the var with max vif
  var_with_max_vif <- names(which(vifs == max(vifs))) 
  
  # remove
  signifVars <- signifVars[!(signifVars) %in% var_with_max_vif]  
  
  # new formula
  myForm <- as.formula(paste("ozone_reading ~ ", paste (signifVars, collapse=" + "), sep=""))  
  print(myForm)
  # re-build model with new formula
  finalModel <- lm(myForm, data=inputData)  
  vifs <- car::vif(finalModel)
}
summary(finalModel)

car::vif(finalModel)

#Note: 
#The VIFs of all the response variables are below 2 now. 
#So, the condition of multicollinearity is satisfied.


#The variable wind_speed in the model with p value > .1 is not statistically significant. 
#For this specific case, we could just re-build the model without wind_speed and check all variables are statistically significant. 
#Recursively remove non-significant variables
# names of all response variables
all_vars <- names(finalModel[[1]])[-1] 

# Get the non-significant vars
# model summary
modelSummary <- summary(finalModel)  
modelSummary
# get all p values
pvals <- modelSummary[[4]][, 4]  
# init variables that aren't statsitically significant
notSignificantVars <- character()  
notSignificantVars <- names(which(pvals > 0.1))
# remove 'intercept'. Optional!
notSignificantVars <- notSignificantVars[!notSignificantVars %in% "(Intercept)"]  
notSignificantVars

# If there are any non-significant variables, 
while(length(notSignificantVars) > 0){
  all_vars <- all_vars[!all_vars %in% notSignificantVars[1]]
  myForm <- as.formula(paste("ozone_reading ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
  finalModel <- lm(myForm, data=inputData)  # re-build model with new formula
  
  # Get the non-significant vars.
  summ <- summary(finalModel)
  pvals <- summ[[4]][, 4]
  notSignificantVars <- character()
  notSignificantVars <- names(which(pvals > 0.1))
  notSignificantVars <- notSignificantVars[!notSignificantVars %in% "(Intercept)"]
}
summary(finalModel)
car::vif(finalModel)

###############
##Best subsets#
###############

#Best subsets is a technique that relies on stepwise regression to search, find and visualise regression models. 
#But unlike stepwise regression, you have more options to see what variables were included in various 
#shortlisted models, force-in or force-out some of the explanatory variables and also visually inspect the model's performance w.r.t Adj R-sq.
#install.packages("leaps")
library(leaps)
View(x_df)
View(y_df)
install.packages("mice")
mice::md.pattern(x_df) 
mice::md.pattern(y_df)
regsubsetsObj <- regsubsets(x=x_df ,y=inputData$ozone_reading, nbest = 2, really.big = T)
# regsubsets plot based on R-sq
plot(regsubsetsObj, scale = "adjr2") 

#Note: How to interpret the regsubsets plot?
#The regsubsets plot shows the adjusted R-sq along the Y-axis for many models created by combinations of variables shown on the X-axis.
#For instance, draw an imaginary horizontal line along the X-axis from any point along the Y-axis. That line would correspond to a linear model, where, the black boxes that line touches form the X variables. 
#The caveat however is that it is not guaranteed that these models will be statistically significant.


##############
#####Leaps####
##############
#Leaps is similar to best subsets but is known to use a better algorithm to shortlist the models.
# criterion could be one of "Cp", "adj R^2", "R^2". 
#Note: Works for max of 32 predictors.
leapSet <- leaps(x=x_df, y=inputData$ozone_reading, nbest = 1, method = "adjr2")
leapSet$which
leapSet$adjr2

# Suppose, we want to choose a model with 4 variables.
# pick selected vars
selectVarsIndex <- leapSet$which[4, ]  
selectVarsIndex
# new data for building selected model
newData <- cbind(y_df, x_df[, selectVarsIndex])  
# build model
selectedMod <- lm(ozone_reading ~ ., data=newData)  
summary(selectedMod)

###########################
#RegBest() from FactoMineR#
###########################
install.packages("FactoMineR")
library(FactoMineR)
regMod <- RegBest(y=inputData$ozone_reading, x = x_df)
# summary of best model of all sizes based on Adj A-sq
regMod$all  
# best model
regMod$best  

#########################
###Simulated Annealing###
#########################
#Given a set of variables, a simulated annealing algorithm seeks a k-variable subset which is optimal, as a surrogate(substitute)
#for the whole set, with respect to a given criterion. 
#Annealing offers a method of finding the best subsets of predictor variables. 
#Since the correlation or covariance matrix is a input to the anneal() function, only continuous variables are used to compute the best subsets.
install.packages("subselect")
library(subselect)
# perform annealing<
results <- anneal(cor(x_df), kmin=1, kmax=ncol(x_df)-1, nsol=4, niter=10, setseed=TRUE) 
print(results$bestsets)

num_vars <- 3
selectVarsIndex <- results$bestsets[num_vars, 1:num_vars]
# new data for building selected model
newData <- cbind(response_df, predictors_df[, selectVarsIndex])  
# build model
selectedMod <- lm(ozone_reading ~ ., data=newData) 
summary(selectedMod)

###############################
##Comparing Models Using ANOVA#
###############################
#If you have two or more models that are subsets of a larger model, you can use anova() to
#check if the additional variable(s) contribute to the predictive ability of the model.
#In below example, the baseMod is a model built with 7 explanatory variables, 
#while, mod1 through mod5 contain one predictor less than the previous model.
# ANOVA
baseMod <- lm(ozone_reading ~ Month + pressure_height + Humidity + Temperature_Sandburg + Temperature_ElMonte + Inversion_base_height + Wind_speed, data=inputData)
mod1 <- lm(ozone_reading ~ Month + pressure_height + Humidity + Temperature_Sandburg + Temperature_ElMonte + Inversion_base_height, data=inputData)
mod2 <- lm(ozone_reading ~ Month + pressure_height + Humidity + Temperature_Sandburg + Temperature_ElMonte, data=inputData)
mod3 <- lm(ozone_reading ~ Month + pressure_height + Humidity + Temperature_ElMonte, data=inputData)
mod4 <- lm(ozone_reading ~ Month + pressure_height + Temperature_ElMonte, data=inputData)
anova(baseMod, mod1, mod2, mod3, mod4)

#For each row in the output, the anova() tests a hypothesis comparing two models. For instance, row 2 compares baseMod (Model 1) and mod1 (Model 2) in the output. The null hypothesis is that the two models are equal in fitting the data (i.e. the Y variable), while, the alternative hypothesis is that the full model is better (i.e. the additional X variable improves the model).

#So what's the inference? Except for row 2, all other rows have significant p values. This means all the additional variables in models 1, 2 and 3 are contributing to respective models.
#From row 1 output, the Wind_speed is not making the baseMod (Model 1) any better. So the best model we have amongst this set is mod1 (Model1).