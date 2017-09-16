#Linear algorithms

library(mlbench)
data("BostonHousing")

# lm functon from stats package
sfit <- lm(medv~., data=BostonHousing)
predictions <- predict(sfit, BostonHousing)
print(BostonHousing$medv)
summary(predictions)
#Summarize accuracy
mse = mean((BostonHousing$medv - predictions)^2)
print(mse)


#
set.seed(7)
tc <- trainControl(method='cv', number = 5)
cFit <- train(medv~., data=BostonHousing, tcControl = tc, method='lm', metric = 'RMSE', preProc = c('center', 'scale'))
summary(cFit)
summary(sfit)


#Logistic Regression 

#using stats package: glm
data("PimaIndiansDiabetes")
slfit <- glm(diabetes~., data=PimaIndiansDiabetes, family=binomial(link='logit'))
print(slfit)
probabilities <- predict(slfit, PimaIndiansDiabetes[, 1:8], type='response')
predictions <- ifelse(probabilities > .5, 'pos', 'neg')
table(predictions, PimaIndiansDiabetes$diabetes)

# using carat package: glm
set.seed(7)
tnControl <- trainControl(method = 'cv', number = 5)
fit.glm <- train(diabetes~., data=PimaIndiansDiabetes, trControl = tnControl, 
                 method = 'glm', preProc = c('center', 'scale'), metric = 'Accuracy')
print(fit.glm)


#Linear Discriminant analysis
#using MASS package: lda function
fit.lda1 <- lda(diabetes~., data=PimaIndiansDiabetes)
print(fit.lda1)
predictions <- predict(fit.lda1, PimaIndiansDiabetes[, 1:8])$class
table(predictions, PimaIndiansDiabetes$diabetes)

#using carat package: lda argument
tnControl <- trainControl(method = 'cv', number = 5)
fit.lad2 <- train(diabetes ~ ., data = PimaIndiansDiabetes, method = 'lda', metric = 'Accuracy', trControl = tnControl, preProc = c('center', 'scale'))
print(fit.lad2)


#Regularized Regression
#glmnet() of glmnet package can be used for regression and classification

#classification example: glmnet function
install.packages('glmnet')
library(glmnet)

x <- as.matrix(PimaIndiansDiabetes[, 1:8])
y <- as.matrix(PimaIndiansDiabetes[9])

fit.glmnet <- glmnet(x, y, family = 'binomial', alpha = 0.5, lambda = .001)
predictions <- predict(fit.glmnet, x, type='class')
table(predictions, PimaIndiansDiabetes$diabetes)


#regression example : glmnet() function
BostonHousing$chas <- as.numeric(as.character(BostonHousing$chas))
x <- as.matrix(BostonHousing[, 1:13])
y <- as.matrix(BostonHousing[14])
fit.glmnet2 <- glmnet(x, y, family='gaussian', lambda = .001,alpha = 0.5)
predictions <- predict(fit.glmnet2, x, type='link')
mean <- mean((y-predictions)^2)
print(mean)

#classification example: glmnet arg
data("PimaIndiansDiabetes")
trainControl = trainControl(method = 'cv', number = 5)
fit.glmnet = train(diabetes~., data=PimaIndiansDiabetes,  method = 'glmnet', trControl = trainControl, metric = 'Accuracy', preProc = c('center', 'scale') )
predictions = predict(fit.glmnet, PimaIndiansDiabetes[, 1:8])
table(predictions, PimaIndiansDiabetes$diabetes)

#regression example: glmnet arg
data("BostonHousing")
str(BostonHousing)
regFit.glmnet = train(medv~., data = BostonHousing, method = 'glmnet', preProc = c('center', 'scale'), metric = 'RMSE', trControl = trainControl)
summary(regFit.glmnet)

# Non Linear Algorithms

