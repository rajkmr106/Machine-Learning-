#Binary Classification Usecase

# Step 1: Defining a problem
library(mlbench)
library(caret)
memory.limit()
memory.limit(size=3000)
data(BreastCancer)
dataset = BreastCancer

# data partition
trainIndex = createDataPartition(dataset$class, p = .8, list = FALSE)
trainData <- dataset[trainIndex, ]
testData <- dataset[-trainIndex, ]

# Step 2: Analyze Data

# 2.1 Descriptive Statistics
# peek at data
head(trainData)

# Summary of data
summary(trainData)

#Srtucture of data
str(trainData)

#Attribute data type
sapply(trainData, class)

# Remove Id column
trainData <- trainData[, -1]

# convert texutal data to numeric data
dim(trainData)
for(i in 1:9)
  trainData[, i] <- as.numeric(as.character(trainData[, i]))
summary(trainData)

#class imbalance
cbind(freq = table(trainData$Class), percentage = prop.table(table(trainData$Class))*100)

#Summarize the correlation between variables
complete_cases <- complete.cases(trainData)
cor(trainData[complete_cases, 1:9])

# 2.1 Data Visualizations
#  Univariate Visualization
# Histogram

par(mfrow = c(3,3))
for(i in 1:9)
  hist(trainData[, i], main = names(trainData)[i])

#Density plot
par(mfrow = c(3, 3))
complete_cases = complete.cases(trainData)
for(i in 1:9){
  plot(density(trainData[complete_cases, i]), main = names(trainData)[i])
}

#Box plot
par(mfrow = c(3,3))
for(i in 1:9)
  boxplot(trainData[, i], main=names(trainData)[i])

# scatterplot matrix
jittered_x <- sapply(trainData[,1:9], jitter)
pairs(jittered_x, names(trainData[,1:9]), col=trainData$Class)

# scatterplot matrix
pairs(trainData, names(trainData[,1:9]), col=trainData$Class)