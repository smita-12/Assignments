install.packages("arules") #Apriori Algo association
install.packages("arulesViz") #Visualization for frequency plots

library(arules)
library(arulesViz)

summary(book)
str(book)

#checking NA values
colSums(is.na(book))

#Apriori Algorithm
brules <- apriori(book)
arules::inspect(brules)
brules.sorted <- sort(brules, by = "lift") 
arules::inspect(brules.sorted)

#Apriori Algorithm with support 0.02, conf 0.5 & minlen 5
brules <- apriori(as.matrix(book),parameter=list(support=0.02, confidence = 0.5,minlen=5))
arules::inspect(brules)
brules.sorted <- sort(brules, by = "lift")
arules::inspect(brules.sorted)

inspect(head(sort(brules, by = "lift")))
inspect(head(sort(brules, by = "confidence")))
inspect(head(sort(brules, by = "support")))
inspect(head(sort(brules, by = c("count","lift"))))

head(quality(brules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(brules,method = "scatterplot",jitter=0,col=colfunc(30))

#Plot methods
plot(brules, method = "scatterplot")
plot(brules, method = "grouped")
plot(brules, method = "paracoord")
plot(brules, method = "graph")
plot(brules, method = "matrix")
plot(brules, method = "two-key plot", jitter=0)
