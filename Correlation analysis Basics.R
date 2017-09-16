#correlation  analysis of mtcars data
# wt vs mpg
my_data <- mtcars
head(my_data, 6)

#R base graph package for scatter plot
library("ggpubr")
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

#Step 1
#Is the covariation linear? 
#Yes, form the plot above, the relationship is linear.
#In the situation where the scatter plots show curved patterns, we are dealing with nonlinear association between the two variables.

#Step 2
#Are the data from each of the 2 variables (x, y) follow a normal distribution?
#Use Shapiro-Wilk normality test -> R function: shapiro.test()
#and look at the normality plot -> R function: ggpubr::ggqqplot()


#Shapiro-Wilk normality test for mpg
shapiro.test(my_data$mpg) # => p = 0.1229
# Shapiro-Wilk normality test for wt
shapiro.test(my_data$wt) # => p = 0.09


#From the output, the two p-values are greater than the significance level 0.05 
#that implies that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.


library("ggpubr")
# mpg
ggqqplot(my_data$mpg, ylab = "MPG")
# wt
ggqqplot(my_data$wt, ylab = "WT")

#Note : From the normality plots, we conclude that both populations may come from normal distributions.

#Note : if the data are not normally distributed, it's recommended to use the non-parametric correlation, 
#including Spearman and Kendall rank-based correlation tests.

#2.1
###Pearson correlation test###

result <- cor.test(my_data$wt, my_data$mpg, 
                method = "pearson")
result

#In the result above :

# t is the t-test statistic value (t = -9.559),
# df is the degrees of freedom (df= 30),
# p-value is the significance level of the t-test (p-value = 1.29410^{-10}).
# conf.int is the confidence interval of the correlation coefficient at 95% (conf.int = [-0.9338, -0.7441]);
# sample estimates is the correlation coefficient (Cor.coeff = -0.87).

#Intrepretatoin
#The p-value of the test is 1.29410^{-10}, which is less than the significance level alpha = 0.05. 
#We can conclude that wt and mpg are significantly correlated with a correlation coefficient of -0.87 and p-value of 1.29410^{-10} .


#2.2
###Kendall rank correlation testt###

#The Kendall rank correlation coefficient or Kendall's tau statistic is used to estimate a rank-based measure of association.
#This test may be used if the data do not necessarily come from a bivariate normal distribution.
result2 <- cor.test(my_data$wt, my_data$mpg,  method="kendall")
result2

#The correlation coefficient between x and y are -0.7278 and the p-value is 6.70610^{-9}.

#2.3
###Spearman rank correlation coefficient###

#Spearman's rho statistic is also used to estimate a rank-based measure of association. 
#This test may be used if the data do not come from a bivariate normal distribution.

result3 <-cor.test(my_data$wt, my_data$mpg,  method = "spearman")
result3

#The correlation coefficient between x and y are -0.8864 and the p-value is 1.48810^{-11}.


#Step 3
#Interpret correlation coefficient

# Correlation coefficient is comprised between -1 and 1:#   
#   
#  -1 indicates a strong negative correlation : this means that every time x increases, y decreases (left panel figure)
#   0 means that there is no association between the two variables (x and y) (middle panel figure)
#   1 indicates a strong positive correlation : this means that y increases with x (right panel figure)


#Step 4
####Summary#####
#Use the function cor.test(x,y) to analyze the correlation coefficient between two variables and to get significance level of the correlation.
#Three possible correlation methods using the function cor.test(x,y): pearson, kendall, spearman