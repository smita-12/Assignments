#K-Means Clustering crime_data

install.packages("plyr")
library(plyr)
summary(crime_data)
mydata <- scale(crime_data[,2:5])
plot(crime_data)
str(crime_data)

km <- kmeans(mydata,4)
install.packages("animation")
library(animation)
windows()
km <- kmeans.ani(crime_data [,2:5],4)
wss <- c()
for (i in 2:5) wss[i] <- sum(kmeans(mydata, centers = i)$withinss)  
plot(1:5, wss, type = "b", xlab = "No. of clusters", ylab = "Avg Distance")

fit <- kmeans(mydata,4)
finalmodel<-data.frame(mydata,fit$cluster)
View(finalmodel)
