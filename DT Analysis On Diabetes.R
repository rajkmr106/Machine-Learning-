#########################
######Decision Tree######
#########################


#Analysis on Diabetes data

getwd()
setwd("D:\\achi\\R-DS")
getwd()

#Load diabetes data and create dataframe
dbData <- read.csv("Diabetes.csv")
head(dbData)

#number of rows in dataset
nrow(dbData)
set.seed(2)
dbData$ind <- sample(2, nrow(dbData), replace=TRUE, prob=c(0.7, 0.3))
head(dbData$ind)
trainData <- dbData[(dbData$ind == 1), ]
testData <- dbData[(dbData$ind == 2), ]
nrow(trainData)
head(testData)


library(rpart)
str(trainData)
head(trainData)

###############
#Scenario 1
###############
# Construct Decision Tree with minsplit = 10
dt1 <- rpart(trainData$Class.variable ~., 
            data = trainData, 
            control = rpart.control(minsplit = 10))
dt1
summary(dt1)
plot(dt1)
text(dt1)

#Validate the DT or Predict against test data
pred1 <- predict(dt1,testData, type=c("class"))


#Display the validation results
cbind(as.character(testData$Class.variable), as.character(pred1))

#Confusion Matrix/Classification Matrix
cmatrix = table(as.character(testData$Class.variable), as.character(pred1))
cmatrix
# Frequencies & Percentages relative to total columns in test data
prop.table(cmatrix)
round(prop.table(cmatrix), 2)*100

# Frequencies relative to total yes/no columns in test data
prop.table(cmatrix,1)
round(prop.table(cmatrix, 1), 2)*100

#Predict probability for each row
predP <- predict(dt1, testData, type=c("prob"))
head(predP)

#Total Accuracy
accuracy1 <- (cmatrix[1] + cmatrix[2,2]) / sum(cmatrix)

attributes(dt1)
dt1$variable.importance
accuracy1


###############
#Scenario 2
###############
# Construct Decision Tree with minsplit = 1
dt2 <- rpart(trainData$Class.variable ~., 
            data = trainData, 
            control = rpart.control(minsplit = 1))
dt2
plot(dt2)
text(dt2)

#Validate the DT or Predict against test data
pred2 <- predict(dt2,testData, type=c("class"))
pred2

#Display the validation results
#cbind(as.character(testData$Class.variable), as.character(pred2))

#Confusion Matrix/Classification Matrix
cmatrix = table(as.character(testData$Class.variable), as.character(pred2))
cmatrix
# Frequencies & Percentages relative to total columns in test data
prop.table(cmatrix)
round(prop.table(cmatrix), 2)*100

# Frequencies relative to total yes/no columns in test data
prop.table(cmatrix,1)
round(prop.table(cmatrix, 1), 2)*100

#Total Accuracy
accuracy2 <- (cmatrix[1] + cmatrix[2,2]) / sum(cmatrix)
accuracy2

dt2$variable.importance
accuracy1
accuracy2

###############
#Scenario 3
###############
#Prediction against new data()
newTestData <- read.csv("Diabetes-New.csv")
newTestData$ind <- 2
head(newTestData)
pred3 <- predict(dt1, newTestData, type=c("class"))
pred3


############
#Scenario 4#
############
#Build new DT Model omitting the unimportant attributes from the previous DT Model
dt1$variable.importance
dt4 <- rpart(Class.variable ~ Number.of.times.pregnant
            +Plasma.glucose.concentration
            +Diastolic.blood.pressure
            +X2.Hour.serum.insulin,
            data = trainData, 
            control=rpart.control(minsplit=10))

dt4
plot(dt4)
text(dt4)

####Predict ####
pred4 = predict(dt4, testData, type=c("class"))
#Confusion Matrix/Classification Matrix
cmatrix = table(as.character(testData$Class.variable), as.character(pred4))
cmatrix
# Frequencies & Percentages relative to total columns in test data
prop.table(cmatrix)
round(prop.table(cmatrix), 2)*100

# Frequencies relative to total yes/no columns in test data
prop.table(cmatrix,1)
round(prop.table(cmatrix, 1), 2)*100

#Total Accuracy
accuracy4 <- (cmatrix[1] + cmatrix[2,2]) / sum(cmatrix)
accuracy4

############
#Scenario 5#
############
#Build new DT Model omitting the unimportant attributes from the previous DT Model
dt1$variable.importance
dt <- rpart(Class.variable ~ Number.of.times.pregnant
            +Plasma.glucose.concentration
            +Diastolic.blood.pressure,
            data = trainData, 
            control=rpart.control(minsplit=10))

dt
plot(dt)
text(dt)

####Predict ####
pred5 = predict(dt, testData, type=c("class"))

#Confusion Matrix/Classification Matrix
cmatrix = table(as.character(testData$Class.variable), as.character(pred5))
cmatrix
# Frequencies & Percentages relative to total columns in test data
prop.table(cmatrix)
round(prop.table(cmatrix), 2)*100

# Frequencies relative to total yes/no columns in test data
prop.table(cmatrix,1)
round(prop.table(cmatrix, 1), 2)*100

#Total Accuracy
accuracy5 <- (cmatrix[1] + cmatrix[2,2]) / sum(cmatrix)
accuracy5


accuracy1
accuracy2
accuracy4
accuracy5