# Dataset: iris
# Classification Problem: Predicting the iris flower species from flower measurements
# Input: Numeric, Output: Categorical , three class labels
data(iris)
head(iris)

# Dataset: longley economic regression data
# Regressin Problem: Predict no of people employed from economic variables. 
# Input: Numeric, Output: Numeric
data(longley)
head(longley)
#View(longley)

#Machine learning Bench mark problems
install.packages('mlbench')
library(mlbench)
library(help = 'mlbench')

# Dataset: Boston Housing Data
# Regressinon Problem: Predict the median house price for 1000 for suburbs in Boston
# Input: Numeric, Output: Numeric
data(BostonHousing)
head(BostonHousing)

# Dataset: Wisconsin Breast Cancer Data
# Binary Classification Problem: Predict whether a tissue sample is malignant or benigh for the given tissue sample
# Input: Integer (Nominal), Output: two class labels
data(BreastCancer)
head(BreastCancer)

# Dataset: Glass identification database
# Regression problem: Predict the glass type from a chemical properties
# Input: Numeric, Output: 7 categorical class labels
data("Glass")
head(Glass)

# Dataset: Johns Hopkins university Ionosphere database
# Regression Problem: Predict High energy structure in atmosphere from antenna data
# #Input: Numeric, Output: 2 categorical class labels
data("Ionosphere")
head(Ionosphere)

# Dataset: Pima Indians diabetes Database
# Binary Classification Problem: Predict the onset of diabets in female pima Indians from medical record data
# Input: Numeric, Outout: 2 class labels
data("PimaIndiansDiabetes")
head(PimaIndiansDiabetes)

# Dataset: Sonar, Mines and Rock dataset
# Binary Classification Problem: Predict metal or rock returns from sonar returns data
# Input: Numeric, Output = 2 categorical labels
data(Sonar)
head(Sonar)

# Dataset: Soybean database
# Multi class classification
# Input: Numerics Output: 19 categorical values
data("Soybean")
head(Soybean)

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)

# DataSet: Abalone Database
# Regression Problem: Predict the abalone data from abalone measurements
# Input: Numerica, categorical, Output: Numeric
data(abalone)
head(abalone)
