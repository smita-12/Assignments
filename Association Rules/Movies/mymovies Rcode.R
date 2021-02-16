install.packages("arules") #Apriori Algo association
install.packages("arulesViz") #Visualization for frequency plots

library(arules)
library(arulesViz)

summary(my_movies)
str(my_movies)

#Building algorithm with support 0.2, conf 0.7 & min len 1
mrules <- apriori(as.matrix(my_movies[,6:15],parameter = list(support = 0.2, confidence =0.7, minlen = 1)))
arules::inspect(mrules) ################### 76 rules
mrules.sorted <- sort(mrules, by = "lift")
arules::inspect(mrules.sorted)

inspect(head(sort(mrules, by = "lift")))
inspect(head(sort(mrules, by = "confidence")))
inspect(head(sort(mrules, by = "support")))
inspect(head(sort(mrules, by = c("count","lift"))))

head(quality(mrules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(mrules,method = "scatterplot",jitter=0,col=colfunc(30))

#Plot methods
plot(mrules, method = "scatterplot")
plot(mrules, method = "grouped")
plot(mrules, method = "paracoord")
plot(mrules, method = "graph")
plot(mrules, method = "matrix")
plot(mrules, method = "two-key plot", jitter=0)


