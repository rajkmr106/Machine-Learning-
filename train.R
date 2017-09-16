trainModels <- function(methodList, metric, localDataset, preProcess, trainControl) {
  # Train models
  #LR
  set.seed(7)
  #print(methodList)
  modelList = list()
  for (method in methodList) {
    print(method)
    modelList[[method]] <- train(
      medv ~ .,
      method = method,
      data = localDataset,
      metric = metric,
      preProc = preProcess,
      trControl = trainControl
    )
  }
  results = resamples(modelList)
  results
}

#library(fastAdaboost)
#library(adabag)
#library(mlbench)
data("BostonHousing")
dataset = BostonHousing

# step 2: Split dataset for training and validation
#install.packages("Rcpp")
library(caret)
trainIndex = createDataPartition(dataset$medv, p = 0.8, list = FALSE)
trainData <- dataset[trainIndex,]
testData <- dataset[-trainIndex]


#install.packages("fastAdaboost,adabag,plyr,adaptDA,frbs,VGAM,ipred,e1071,earth,mda,logicFS,caret,bartMachine,arm,brnn,monomvn,binda,ada,mboost,bst,caTools,party,RWeka,C50,rpart,rpartScore,CHAID,Cubist,deepboost,sparsediscrim,kerndwd,kernlab,elasticnet,enpls,randomGLM,xgboost,elmNN,HiDimDA,gam,mgcv,MASS,gpls,h2o,glmnet,Matrix,proxy,protoclass,hda,HDclassif,fastICA,kknn,rPython,LiblineaR,class,lars,klaR,leaps,LogicReg,bnclassify,nnet,RSNNS,FCNN4R,pamr,neuralnet,rqPen,nnls,obliqueRF,oblique.tree,snn,randomForest,foreach,partDSA,pls,plsRglm,penalizedLDA,penalized,stepPlr,ordinalNet,KRLS,quantregForest,qrnn,rFerns,ranger,Rborist,extraTrees,inTrees,Boruta,RRF,relaxo,foba,rrcov,robustDA,rrlda,rrcovHD,rocc,rotationForest,kohonen,sda,sdwd,sparseLDA,spls,spikeslab,deepnet,SDDA,gbm,superpc,evtree,nodeHarvest,vbmp,wsrf")

metric = "RMSE"
preProcess = c("center", "scale")
tControl = trainControl(method = "cv", number = 2 )
errorMethods = c('ANFIS', 'logicBag', 'bag')
methods = c("treebag","bagEarth","bagEarthGCV","bartMachine","bayesglm","brnn","bridge","blassoAveraged","gamboost","glmboost","BstLm","bstSm","blackboost","bstTree","rpart","rpart1SE","rpart2","cforest","ctree","ctree2","cubist","DENFIS","enet","enpls","enpls.fs","randomGLM","xgbLinear","xgbTree","elm","FIR.DM","GFS.FR.MOGUL","GFS.THRIFT","gaussprLinear","gaussprPoly","gaussprRadial","gamLoess","bam","gam","gamSpline","glm","glmStepAIC","GFS.LT.RS","gbm_h2o","glmnet","glmnet_h2o","HYFIS","icr","kknn","knn","pythonKnnReg","svmLinear3","lars","lars2","lm","leapBackward","leapForward","leapSeq","lmStepAIC","logreg","avNNet","M5Rules","M5","mlp","mlpWeightDecay","mlpWeightDecayML","mlpML","mlpSGD","earth","gcvEarth","glm.nb","neuralnet","nnet","pcaNNet","rqnc","nnls","parRF","partDSA","kernelpls","pls","simpls","widekernelpls","plsRglm","penalized","ordinalNet","krlsPoly","pcr","ppr","qrf","qrnn","rqlasso","krlsRadial","rbf","rbfDDA","ranger","Rborist","rf","extraTrees","rfRules","Boruta","RRF","RRFglobal","relaxo","rvmLinear","rvmPoly","rvmRadial","ridge","foba","rlm","bdk","xyf","FS.HGD","spls","spikeslab","dnn","gbm","SBC","superpc","svmBoundrangeString","svmExpoString","svmLinear","svmLinear2","svmPoly","svmRadial","svmRadialCost","svmRadialSigma","svmSpectrumString","blasso","lasso","evtree","nodeHarvest","WM")
results1 = trainModels(methods, metric, trainData, preProcess, tControl)
summary(results1)

install.packages('kernlab')
install.packages('Rcpp')
install.packages('ggplot2')





