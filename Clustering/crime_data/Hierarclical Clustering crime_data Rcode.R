#Hierarchical Clustering crime_data
summary(crime_data)
mydata <- scale(crime_data[,2:5])
d <- dist(mydata, method = "euclidean")
d
boxplot(mydata)
fit <- hclust(d,method = "complete") #tried avaerage & centriod method 
plot(fit,hang = -1)
rect.hclust(fit, k = 4, border = "red")
groups <- cutree(fit, k = 4)
table(groups)
clusters = data.frame(crime_data = mydata[,1] cluster = groups)

