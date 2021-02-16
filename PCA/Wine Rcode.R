install.packages("gdata")
library(gdata)
install.packages("plyr")
library(plyr)

#overview stats
summary(wine)

ncol(wine)
nrow(wine)
class(wine)
str(wine)

#converting int data type int (contains whole nos) to numeric(decimals & whole nos)
wine$Magnesium <- as.numeric(wine$Magnesium)
wine$Proline <- as.numeric(wine$Proline)

str(wine)
head(wine)

#scaling/normarlizing dataset
mydata <- scale(wine[,2:13])

#No NA values in dataset
sum(is.na(mydata)) 

boxplot(mydata)

pca <- princomp(wine [,2:13], cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca) #check variance and cumulative

pca$scores #transformed dataset

plot(pca)

plot(pca$scores[,1:3], col = "Blue", pch = 18, cex = 0.3, lwd = 3)
windows()
text(pca$scores[,1:7], labels = c(1:178), cex = 1)

#Hierarclical clustering

#Calculating euclidean distance
d <- dist(mydata, method = "euclidean")
d
mydata

#Method used (Average, Complete, Single, Centriod)
fit <- hclust(d, method = "complete") #tried average, centriod unable to form good clusters

#Display dendogram
plot(fit)
plot(fit, hang = -1)

#Cutting dendogram into 7 clusters
groups <- cutree(fit, k=7)

#Applying borders
rect.hclust(fit, k=7,border = "red")



#Kmeans Clustering
wss <- c()
for (i in 2:13) wss[i] <- sum(kmeans(mydata, centers = i)$withinss)
plot(2:14, wss, type = "b", xlab = "No. of clusters", ylab = "Avg Distance") #4 0r 8 clusters
