trainModels <- function(methodList, dataset, metric, tcontrol){
  modelList = list()
  count = 0
  for(method in methodList){
    count = count+1
    cat('\nMethod ', count, ': ', method)
    set.seed(7)
    modelList[[method]] <- caret::train(medv~., data = dataset, method = method, metric = metric, trControl = tControl)
    
  }
  modelList
}
getRMSEValues <- function(modelsSummary){
  rmse = as.data.frame(modelsSummary$statistics[1])
  rmse = rmse[order(rmse$RMSE.Mean), ]
  rmse
}



getRSquaredValues <- function(modelsSummary){
  r_squared = as.data.frame(modelsSummary$statistics[2])
  r_squared = r_squared[order(r_squared$Rsquared.Mean), ]
  r_squared
}

memory.limit(size=4000)
memory.limit()
library(caret)
library(mlbench)
data(BostonHousing)
dataset <- BostonHousing
set.seed(7)

trainIndex <- createDataPartition(BostonHousing$medv, p=0.80, list=FALSE)
# select 20% of the data for validation
testData <- BostonHousing[-trainIndex,]
# use the remaining 80% of data to training and testing the models
trainData <- BostonHousing[trainIndex,]

tControl <- trainControl(method='repeatedcv', number=10, repeats = 3)
metric <- 'RMSE'

transformation <- preProcess(trainData, method=c('center', 'scale', 'BoxCox'))
transformedTrainData <- predict(transformation, trainData)

methods1 <- c("ANFIS","treebag","logicBag","bagEarth","bagEarthGCV","bag","bartMachine","bayesglm","brnn","bridge")
methods2 <- c("blassoAveraged","gamboost","glmboost","BstLm","bstSm","blackboost","bstTree","rpart","rpart1SE","rpart2")
methods3 <- c("cforest","ctree","ctree2","cubist","DENFIS","enet","enpls","enpls.fs","randomGLM","xgbLinear")
methods4 <- c("xgbTree","elm","FIR.DM","GFS.FR.MOGUL","GFS.THRIFT","gaussprLinear","gaussprPoly","gaussprRadial","gamLoess","bam")
methods5 <- c("gam","gamSpline","glm","glmStepAIC","GFS.LT.RS","gbm_h2o","glmnet","glmnet_h2o","HYFIS","icr")
methods6 <- c("kknn","knn","pythonKnnReg","svmLinear3","lars","lars2","lm","leapBackward","leapForward","leapSeq")
methods7 <- c("lmStepAIC","logreg","avNNet","M5Rules","M5","mlp","mlpWeightDecay","mlpWeightDecayML","mlpML","mlpSGD")
methods8 <- c("earth","gcvEarth","glm.nb","neuralnet","nnet","pcaNNet","rqnc","nnls","parRF","partDSA")
methods9 <- c("kernelpls","pls","simpls","widekernelpls","plsRglm","penalized","ordinalNet","krlsPoly","pcr","ppr")
methods10 <- c("qrf","qrnn","rqlasso","krlsRadial","rbf","rbfDDA","ranger","Rborist","rf","extraTrees")
methods11 <- c("rfRules","Boruta","RRF","RRFglobal","relaxo","rvmLinear","rvmPoly","rvmRadial","ridge","foba")
methods12 <- c("rlm","bdk","xyf","FS.HGD","spls","spikeslab","dnn","gbm","SBC","superpc")
methods13 <- c("svmBoundrangeString","svmExpoString","svmLinear","svmLinear2","svmPoly","svmRadial","svmRadialCost","svmRadialSigma","svmSpectrumString","blasso")
methods14 <- c("lasso","evtree","nodeHarvest","WM")


#Lazy:
lazyRegressionMethods <- c('extraTrees',"nodeHarvest", "ANFIS","bartMachine","DENFIS","randomGLM","FIR.DM","GFS.FR.MOGUL","FS.HGD","WM","rqnc","HYFIS")

#Error: 
errRegressionMethods <- c("logicBag", "bag", "gamboost","bstSm", "enpls", "enpls.fs","xgbLinear" ,"GFS.THRIFT", "GFS.LT.RS", "gbm_h2o","glmnet_h2o", "lmStepAIC","logreg","mlp", "glm.nb", "ordinalNet", "qrnn","rbf","rqlasso", "Boruta", "bdk","xyf","gamSpline", "svmBoundrangeString", "svmExpoString","svmSpectrumString", "ranger")

regressionMethods1 <- c("avNNet", "bagEarth", "bagEarthGCV", "bam", "bayesglm", "blackboost", "blasso", "blassoAveraged",
                "bridge", "brnn", "BstLm", "bstTree", "cforest", "ctree", "ctree2", "cubist", "dnn", "earth",
                "elm", "enet", "evtree", "foba", "gam", "gamLoess", "gaussprLinear", "gaussprPoly",
                "gaussprRadial", "gbm", "gcvEarth", "glm", "glmboost", "glmnet", "glmStepAIC", "icr", "kernelpls",
                "kknn", "knn", "krlsPoly", "krlsRadial", "lars", "lars2", "lasso", "leapBackward", "leapForward",
                "leapSeq", "lm", "M5", "M5Rules", "mlpML", "mlpSGD", "mlpWeightDecay", "mlpWeightDecayML",
                "neuralnet", "nnet")
regressionMethods2 <- c("nnls", "parRF", "partDSA", "pcaNNet", "pcr", "penalized", "pls",
                "plsRglm", "ppr", "qrf",  "rbfDDA", "Rborist", "relaxo", "rf", "rfRules", "ridge", "rlm",
                "rpart", "rpart1SE", "rpart2", "RRF", "RRFglobal", "rvmLinear", "rvmPoly", "rvmRadial", "SBC", "simpls",
                "spikeslab", "spls", "superpc", "svmLinear", "svmLinear2", "svmLinear3", "svmPoly", "svmRadial",
                "svmRadialCost", "svmRadialSigma", "treebag", "widekernelpls", "xgbTree")
regressionResults1 = trainModels(regressionMethods1, trainData, metric, tcontrol)
print(regressionResults1)
regressionResults2 = trainModels(regressionMethods2, trainData, metric, tcontrol)
totalResults = append(regressionResults1, regressionResults2)
regressionResults = resamples(regressionResults)
modelsSummary = summary(regressionResults)
rmseResults = getRMSEValues(modelsSummary)
rmseResults


methods14 <- c('cubist', 'treebag')
results14 = trainModels(methods14, trainData, metric, tcontrol)
s = summary(results14)
rmse = as.data.frame(s$statistics[1])
rmse = rmse[order(rmse$RMSE.Mean), ]
rmse