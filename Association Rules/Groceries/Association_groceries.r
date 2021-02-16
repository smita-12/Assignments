install.packages("arules") #Apriori Algo association
install.packages("arulesViz") #Visualization for frequency plots

library(arules)
library(arulesViz)

summary(groceries)
str(groceries)

#checking NA values
colSums(is.na(groceries))

#Apriori Algorithm 
grules <- apriori(groceries)
arules::inspect(grules)
grules.sorted <- sort(grules, by = "lift") 
arules::inspect(grules.sorted)

#Apriori Algorithm with Support 0.002, Conf 0.05 & min len as 3
grules1 <- apriori(groceries, parameter = list(support=0.005, confidence=0.05, minlen=3)) #130 rules
arules::inspect(grules1) ########################## 130 rules
grules.sorted <- sort(grules1, by = "lift")
arules::inspect(grules.sorted)

inspect(head(sort(grules1, by = "lift")))
inspect(head(sort(grules1, by = "confidence")))
inspect(head(sort(grules1, by = "support")))
inspect(head(sort(grules1, by = c("count","lift"))))

head(quality(grules1))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(grules1,method = "scatterplot",jitter=0,col=colfunc(30))

#Plot methods
plot(grules1, method = "scatterplot")
plot(grules1, method = "grouped")
plot(grules1, method = "paracoord")
plot(grules1, method = "graph")
plot(grules1, method = "matrix")
plot(grules1, method = "two-key plot", jitter=0)



