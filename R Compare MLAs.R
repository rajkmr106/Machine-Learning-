# Comparing Model performance
# Steps 
    # 1. Data load
    # 2. Train Models
    # 3. Comparing models

# step 1: Data Loading
pid <- PimaIndiansDiabetes

# step 2: Train Models

# Train Control
library(caret)
tControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3)



#KNN classifier
set.seed(7)
fit.knn <- train(diabetes~., data = pid, method = 'knn', trControl = tControl)

set.seed(7)
fit.lda <- train(diabetes~., data = pid, method = 'lda', trControl = tControl)

set.seed(7)
fit.cart <- train(diabetes~., data = pid, method = 'rpart', trControl = tControl)

set.seed(7)
fit.svm <- train(diabetes~., data = pid, method = 'svmRadial', trControl = tControl)

#set.seed(7)
#fit.nb <- train(diabetes~., data = pid, method = 'nb', trControl = tControl)

set.seed(7)
fit.rf <- train(diabetes~., data = pid, method = 'rf', trControl = tControl)


results <- resamples(list(rf = fit.rf, svm = fit.svm, cart = fit.cart, lda  = fit.lda, knn = fit.knn))

# Step 3: Compare models

# 3.1 Table Summary
summary(results)

# 3.2 Box and Wisker Plot

scales <- list(x = list(relation = 'free'), y = list(relation = 'free'))
bwplot(results, scales)


# 3.3 Density plot
scales <- list(x = list(relation = 'free'), y = list(relation = 'free'))
densityplot(results, scales, pch = "|")

# 3.4 Dot plot
dotplot(results, scale)

# 3.5 Parallel plot
parallelplot(results)


# 3.6 scatter plot matrix
splom(results)

# 3.7 pairwise xy plots
xyplot(results, models=c('lda', 'svm'))

# 3.8 statistical significant tests
diffs = diff(results)
summary(diffs)

