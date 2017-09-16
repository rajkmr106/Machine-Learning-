#Saving and Finalizing the model

# In this usecase, we will see how to 

# use the trained model to make predictions on unseen data. 
# create a well performing model from caret as a standalone model
# Save the finalized model to a file and load back to the environment to use it on to predict from unseen data

# 1. Make predictions on unseen data
library(mlbench)
data("PimaIndiansDiabetes")
trainIndex = createDataPartition(PimaIndiansDiabetes$diabetes, p=.80, list=FALSE)
trainData <- PimaIndiansDiabetes[trainIndex, ]
testData <- PimaIndiansDiabetes[-trainIndex, ]

set.seed(9)
trainControl = trainControl(method='cv', number = 10)
fit.lda = train(diabetes~., method = 'lda', data = PimaIndiansDiabetes, metric = 'Accuracy', trControl = trainControl)

print(fit.lda)
print(fit.lda$finalModel)

predictions = predict(fit.lda, newdata = testData)
confusionMatrix(predictions, testData$diabetes)

# 2. Finalizing the model

data(Sonar)
sonarTrainIndex = createDataPartition(Sonar$Class, p=.80, list = FALSE)
sonarTrainData <- Sonar[sonarTrainIndex, ]
sonarTestData <- Sonar[-sonarTrainIndex, ]

set.seed(7)
trainControl = trainControl(method = 'cv', number = 10)
fit.rf = train(Class~., data = sonarTrainData, method = 'rf', metric = 'Accuracy', trainControl = trainControl, ntree = 2000)