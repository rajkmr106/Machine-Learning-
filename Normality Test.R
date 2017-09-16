################
#Normality test#
################
#This is the preliminary test that is used to verify the normal distribution of dataset as it is the one of the basic assmuption 
#for parametric tests(Correlation, Regression, T-Test, ANOVA etc..)
#dplyr for data manipulation
#install.packages("dplyr")
?dplyr

library("dplyr")
library("ggpubr")

my_data <- ToothGrowth
set.seed(1234)
dplyr::sample_n(my_data, 10) # Take 10 random records for sample
View(my_data)

#######################################
#Assess the normality of the data in R#
#######################################

#We want to test if the variable len (tooth length) is normally distributed.

#Case of large sample sizes
#If the sample size is large enough (n > 30), we can ignore the distribution of the data and use parametric tests.

#The central limit theorem tells us that no matter what distribution things have, the sampling distribution tends to be normal if the sample is large enough (n > 30).

#However, to be consistent, normality can be checked by visual inspection [normal plots (histogram), Q-Q plot (quantile-quantile plot)] or by significance tests].

#Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.
ggdensity(my_data$len, 
          main = "Density plot of tooth length",
          xlab = "Tooth length")


#Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between a given sample and the normal distribution. A 45-degree reference line is also plotted.
ggqqplot(my_data$len)
library("car")
qqPlot(my_data$len)