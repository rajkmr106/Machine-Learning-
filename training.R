trainModels <- function(methodList, dataset, metric, tcontrol){
  modelList = list()
  for(method in methodList){
    print(method)
    set.seed(7)
    modelList[[method]] <- caret::train(medv~., data = dataset, method = method, metric = metric, trControl = tControl)
  }
  results = resamples(modelList)
  results
}

memory.limit(size=3500)
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
lazyRegressionMethods <- c("ANFIS","bartMachine","DENFIS","randomGLM","FIR.DM","GFS.FR.MOGUL","FS.HGD","WM","rqnc","HYFIS")

#Error: 
errRegressionMethods <- c("logicBag", "bag", "gamboost","bstSm", "enpls", "enpls.fs","xgbLinear" ,"GFS.THRIFT", "GFS.LT.RS", "gbm_h2o","glmnet_h2o", "lmStepAIC","logreg","mlp", "glm.nb", "ordinalNet", "qrnn","rbf","rqlasso", "Boruta", "bdk","xyf","gamSpline", "svmBoundrangeString", "svmExpoString","svmSpectrumString")


regressionMethods <- c("treebag","bagEarth","bagEarthGCV","bayesglm","brnn","bridge", "blassoAveraged",
               "glmboost","BstLm","blackboost","bstTree","rpart","rpart1SE","rpart2", "cforest",
               "ctree","ctree2","cubist","enet", "xgbTree","elm","gaussprLinear","gaussprPoly",
               "gaussprRadial","gamLoess","bam", "gam","glm","glmStepAIC","glmnet",
               "icr", "kknn","knn","svmLinear3","lars","lars2","lm","leapBackward","leapForward",
               "leapSeq", "avNNet","M5Rules","M5","mlpWeightDecay","mlpWeightDecayML","mlpML","mlpSGD",
               "earth","gcvEarth","neuralnet","nnet","pcaNNet","nnls","parRF","partDSA", "kernelpls","pls",
               "simpls","widekernelpls","plsRglm","penalized","krlsPoly","pcr","ppr", "qrf","krlsRadial",
               "rbfDDA","ranger","Rborist","rf","extraTrees", "rfRules","RRF","RRFglobal","relaxo","rvmLinear",
               "rvmPoly","rvmRadial","ridge","foba", "rlm","spls","spikeslab","dnn","gbm","SBC","superpc",
               "svmLinear","svmLinear2","svmPoly","svmRadial","svmRadialCost","svmRadialSigma","blasso",
               "lasso","evtree","nodeHarvest")
regressionResults = trainModels(regressionMethods, trainData, metric, tcontrol)
modelsSummary = summary(regressionResults)
rmse = as.data.frame(modelsSummary$statistics[1])
rmse = rmse[order(rmse$RMSE.Mean), ]
rmse

r_squared = as.data.frame(modelsSummary$statistics[2])
r_squared = r_squared[order(r_squared$Rsquared.Mean), ]
r_squared



methods14 <- c('cubist', 'treebag')
results14 = trainModels(methods14, trainData, metric, tcontrol)
s = summary(results14)
rmse = as.data.frame(s$statistics[1])
rmse = rmse[order(rmse$RMSE.Mean), ]
rmse