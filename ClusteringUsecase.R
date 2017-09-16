#Clustering usecase
getwd()
setwd("D://achi//R-DS")

ins <- read.table("clust.csv", head=T, sep=",")
View(ins)
head(ins,10)

?kmeans
library(animation)
kmeans.ani(ins, 5)
km <- kmeans(ins, 4)

plot(ins$Income, ins$Age, col=km$cluster)
plot(ins$Age, ins$Income, col=km$cluster)

km$cluster
km$centers
km$totss
km$withinss
km$betweenss
km$size

ins2 <- cbind(ins, km$cluster)
head(ins2)
View(ins2)
plot(ins$Income, ins2$age, col=km$cluster)
write.csv(ins2, "Output.csv")