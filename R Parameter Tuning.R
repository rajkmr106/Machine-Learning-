library(mlbench)
data(Sonar)
dataset = Sonar
nrow(Sonar)
str(Sonar)
x = Sonar[, 1:60]
y = Sonar[, 61]
#View(Sonar)

#Test Model (Random Forest)
seed = 7
metric = 'Accuracy'
mtry = floor(sqrt(ncol(Sonar)))
mtry
tGrid = expand.grid(.mtry = mtry)
tGrid
library(caret)
tControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3)
fit.rf = train(Class~., data=dataset, method = 'rf', metric = metric, tuneGrid = tGrid,  trControl = tControl)
print(fit.rf)
#summary(fit.rf)

# Tune Rf using caret Package
# 1. Random Search
randomSearchTC = trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'random')
seed = 7
randomSearhRF = train(Class~., data = dataset, method = 'rf', metric = metric, tuneLength = 15,  trControl = randomSearchTC)
print(randomSearhRF)

# 2. Grid Search
gridSearchTC = trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')
set.seed(seed)
tGrid <- expand.grid(.mtry = c(1:15))
gridSearchRF = train(Class~., data = dataset, method = 'rf', metric = metric, tuneGrid = tGrid, trControl = gridSearchTC)
print(gridSearchTC)

plot(randomSearhRF)
plot(gridSearchRF)

# Tune using algorithm search
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor =  1.5, improve = 1e-5, ntree = 500)
print(bestmtry)