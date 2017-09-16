######Descriptive statistics & Graphics#####

#Import your data into R
?iris
my_data <- iris
View(my_data)
#Insepect the dataset
head(my_data, 6)
tail(my_data, 6)

#R functions for computing descriptive statistics for a single group

#####STEP 1######
# Measure of central tendency: mean, median, mode
# Roughly speaking, the central tendency measures the "average" or the "middle" of your data. The most commonly used measures include:
#   
#   mean: the average value. It's sensitive to outliers.
#   median: the middle value. It's a robust alternative to mean.
#   mode: the most frequent value

# In R,The function mean() and median() can be used to compute the mean and the median, respectively;
# The function mfv() [in the modeest R package] can be used to compute the mode of a variable.
# Compute the mean value
mean(my_data$Sepal.Length)

#Compute the median value
median(my_data$Sepal.Length)

# Compute the mode
install.packages("modeest")
require(modeest)
mfv(my_data$Sepal.Length)

#####STEP 2######
#Measure of variablity 'Range: minimum & maximum'
# Compute the minimum value
min(my_data$Sepal.Length)

# Compute the maximum value
max(my_data$Sepal.Length)

# Range
range(my_data$Sepal.Length)


#####STEP 3######
#Interquartile range
#Recall that, quartiles divide the data into 4 parts.
#Note that, the interquartile range (IQR) - corresponding to the difference between the first and third quartiles - is sometimes used as a robust alternative to the standard deviation.

quantile(my_data$Sepal.Length)
#Note: By default, the function returns the minimum, the maximum and three quartiles (the 0.25, 0.50 and 0.75 quartiles).

quantile(my_data$Sepal.Length, seq(0, 1, 0.1))
#Note: The above function is used to the compute deciles like (0.1, 0.2, 0.3, .., 0.9)

#To compute the interquartile range: 
  IQR(my_data$Sepal.Length)
  
  #####Step 4####
  #Variance and standard deviation
  
  #The variance represents the average squared deviation from the mean. 
  #The standard deviation is the square root of the variance. 
  #It measures the average deviation of the values, in the data, from the mean value.
  
  # Compute the variance
  var(my_data$Sepal.Length)
  
  # Compute the standard deviation = square root of th variance
  sd(my_data$Sepal.Length)
  
  #### Step 4########### 
  #Median absolute deviation
  #The median absolute deviation (MAD) measures the deviation of the values, in the data, from the median value.
  
  # Compute the median
  median(my_data$Sepal.Length)
  
  # Compute the median absolute deviation
  mad(my_data$Sepal.Length)
  
  
  ###################################################################################
  # Which measure to use?
  # 
  # Range: It's not often used because it's very sensitive to outliers.
  # Interquartile range: It's pretty robust to outliers. It's used a lot in combination with the median.
  # Variance: It's completely uninterpretable because it doesn't use the same units as the data. It's almost never used except as a mathematical tool
  # Standard deviation(SD): This is the square root of the variance. It's expressed in the same units as the data. The standard deviation is often used in the situation where the mean is the measure of central tendency.
  # Median absolute deviation(MAD): It's a robust way to estimate the standard deviation, for data with outliers. It's not used very often.
  #############################################################################
  
  #Note:  In summary, the IQR and the standard deviation(SD) are the two most common measures used to report the variability of the data.
  
  
  #Summary() function
  #Computing an overall summary of a variable and an entire data frame
  #The function summary() can be used to display several statistic summaries of either one variable or an entire data frame.
  #Summary of a single variable. Five values are returned: the mean, median, 25th and 75th quartiles, min and max in one single line call:
  summary(my_data$Sepal.Length)
  
  #Summary of a data frame. In this case, the function summary() is automatically applied to each column. The format of the result depends on the type of the data contained in the column. For example:
  #If the column is a numeric variable, mean, median, min, max and quartiles are returned.
  #If the column is a factor variable, the number of observations in each group is returned.
  summary(my_data)
  
  ###sapply() function
  #It's also possible to use the function sapply() to apply a particular function over a list or vector. For instance, we can use it, to compute for each column in a data frame, the mean, sd, var, min, quantile, .
  # Compute the mean of each column
  sapply(my_data[, -5], mean)
  #View(my_data[, -5])
  
  # Compute quartiles
  sapply(my_data[, -5], quantile)
  
  
  #stat.desc() function
  
  #The function stat.desc() [in pastecs package], provides other useful statistics including:
    
  # the median
  # the mean
  # the standard error on the mean (SE.mean)
  # the confidence interval of the mean (CI.mean) at the p level (default is 0.95)
  # the variance (var)
  # the standard deviation (std.dev)
  # and the variation coefficient (coef.var) defined as the standard deviation divided by the mean
  
  #install.packages("pastecs")
  library(pastecs)
  res <- stat.desc(my_data[, -5])
  round(res, 2)
  
  
  #Case of missing values
  #Note: when the data contains missing values, some R functions will return errors or NA even if just a single value is missing.
  
  #For example, the mean() function will return NA if even only one value is missing in a vector. This can be avoided using the argument na.rm = TRUE,
  #which tells to the function to remove any NAs before calculations.
  
  mean(my_data$Sepal.Length, na.rm = TRUE)
  
  ####################################################################
  #######################Graphical display of distributions
  ####################################################################
  install.packages("ggpubr")
  ??ggpubr
  library(ggpubr)
  
  #Box plots
  ggboxplot(my_data, y = "Sepal.Length", width = 0.5)
  
  #Histogram
  gghistogram(my_data, x = "Sepal.Length", bins = 9, 
              add = "mean")
  
  #Empirical cumulative distribution function (ECDF)
  #Note: ECDF is the fraction of data smaller than or equal to x.
  ggecdf(my_data, x = "Sepal.Length")
  
  
  ##########
  #Q-Q plots
  ##########
  #QQ plots is used to check whether the data is normally distributed.
  ggqqplot(my_data, x = "Sepal.Length")
  
  #################################
  #Descriptive statistics by groups
  #################################
  
  #To compute summary statistics by groups, the functions group_by() and summarise() [in dplyr package] can be used.
  
  # We want to group the data by Species and then:
  # compute the number of element in each group. R function: n()
  # compute the mean. R function mean()
  # and the standard deviation. R function sd()
  
  install.packages("dplyr")
  
  #Descriptive statistics by groups:
  library(dplyr)
  group_by(my_data, Species) %>% 
    summarise(
      count = n(), 
      mean = mean(Sepal.Length, na.rm = TRUE),
      sd = sd(Sepal.Length, na.rm = TRUE)
    )
  
  #Graphics for grouped data:
  library("ggpubr")
  # Box plot colored by groups: Species
  ggboxplot(my_data, x = "Species", y = "Sepal.Length",
            color = "Species",
            palette = c("#00AFBB", "#E7B800", "#FC4E07"))
  
  # Stripchart colored by groups: Species
  ggstripchart(my_data, x = "Species", y = "Sepal.Length",
               color = "Species",
               palette = c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "mean_sd")
  
  #Note that, when the number of observations per groups is small, it's recommended to use strip chart compared to box plots.
  
  
  ###################
  #Frequency tables
  ###################
  
  #A frequency table (or contingency table) is used to describe categorical variables. 
  #It contains the counts at each combination of factor levels.
  # Hair/eye color data
  df <- as.data.frame(HairEyeColor)
  tail(df)
  hair_eye_col <- df[rep(row.names(df), df$Freq), 1:3]
  rownames(hair_eye_col) <- 1:nrow(hair_eye_col)
  tail(hair_eye_col)
  
  # #replica 
  # rep(1:4, 2)
  # rep(1:4, each = 2)       # not the same.
  # rep(1:4, c(2,2,2,2))
  # rep(1:4, c(2,1,2,1))
  # rep(1:4, each = 2, len = 4)    # first 4 only.
  # rep(1:4, each = 2, len = 10)   # 8 integers plus two recycled 1's.
  # rep(1:4, each = 2, times = 3)  # length 24, 3 complete replications
  #?rep
  
  hair <- hair_eye_col$Hair
  eye <- hair_eye_col$Eye
  
  #Simple frequency distribution: one categorical variable
  table(hair)
  table(eye)
  
  # Compute table and convert as data frame
  df <- as.data.frame(table(hair))
  df
  # Visualize using bar plot
  #install.packages(ggpubr)
  library(ggpubr)
  ggbarplot(df, x = "hair", y = "Freq")
  
  
  #Two-way contingency table: Two categorical variables
  tbl2 <- table(hair , eye)
  tbl2
  
  # xtabs(), which will create cross tabulation of data frames with a formula interface.
  
  xtabs(~ Hair + Eye, data = hair_eye_col)
  df <- as.data.frame(tbl2)
  ggbarplot(df, x = "hair", y = "Freq",
            color = "eye", 
            palette = c("brown", "blue", "gold", "green"))
  
  # position dodge
  ggbarplot(df, x = "hair", y = "Freq",
            color = "eye", position = position_dodge(),
            palette = c("brown", "blue", "gold", "green"))
  
  
  #######Multiway tables: More than two categorical variables
  #Hair and Eye color distributions by sex using xtabs():
  xtabs(~Hair + Eye + Sex, data = hair_eye_col)
  xtabs(~Eye + Sex + Hair, data = hair_eye_col)
  xtabs(~Sex + Hair + Eye, data = hair_eye_col)
  
  
  #function ftable() [for flat contingency tables].
  #It returns a nice output compared to xtabs() when you have more than two variables:
  ftable(Hair + Eye ~ Sex, data = hair_eye_col)
  ftable(Eye + Sex ~ Hair, data = hair_eye_col)
  ftable(Sex + Hair ~ Eye, data = hair_eye_col)
  
  ####Compute table margins and relative frequency####
  #Table margins correspond to the sums of counts along rows or columns of the table. 
  #Relative frequencies express table entries as proportions of table margins (i.e., row or column totals).
  #The function margin.table() and prop.table() can be used to compute table margins and relative frequencies,
  
  he.table <- table(hair, eye)
  he.table
  margin.table(he.table, 1)
  margin.table(he.table, 2)
  
  #Compute relative frequencies:
  # Frequencies relative to row total
  prop.table(he.table, 1)
  # Table of percentages
  round(prop.table(he.table, 1), 2)*100
  
  #To express the frequencies relative to the grand total, use this:
  
  he.table/sum(he.table)
  sum(he.table)
  