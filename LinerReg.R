# The predictor vector.
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)

# The resposne vector.
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
lreg <- lm(y~x)
lreg

# Find weight of a person with height 170.
a <- data.frame(x = 170)
result <-  predict(lreg,a)
print(result)

#Visualize the Regression Graphically
# Give the chart file a name.
png(file = "linearregression.png")

?plot
# Plot the chart.
plot(y,x,col = "blue",main = "Height & Weight Regression",
     cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")


plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

