#Hierarclical Clustering

summary(EastWestAirlines)
boxplot(EastWestAirlines)
is.na(EastWestAirlines)
plot(EastWestAirlines)
cor(EastWestAirlines)
str(EastWestAirlines)
mydata <- scale(EastWestAirlines [2:12])
d <- dist(mydata, method = "euclidean")
d
fit <- hclust(d, method = "ward.D2")
plot(fit, hang = -1)
rect.hclust(fit, k=4, border = "red")
