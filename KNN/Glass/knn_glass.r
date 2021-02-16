summary(glass)

sum(is.na(glass)) #Checking NA null values
str(glass) #datatype
table(glass$Type) #getting proportion
prop.table(table(glass$Type)) #getting proportion in percentage
prop.table(table(glass$Type))*100 
round(prop.table(table(glass$Type))*100, digits = 1) #rounding off 
boxplot(glass) #checking outliers

#converting class varibale
glass$Type <- as.factor(glass$Type)
levels(glass$Type)
glass_data <- factor(glass$Type, levels = c("1","2","3","5","6","7"), labels = c("1","2","3","5","6","7"))
str(glass)

#normalizing dataset
glass_norm <- scale(glass[,1:9])

#spliting the dataset into train & test
library(caret)
library(gmodels)
set.seed(100)
glass_train <- glass_norm[1:149,]
glass_test <- glass_norm[150:214,]

#get labels for train and test
glass_train_labels <- glass[1:149,10]
glass_test_labels <- glass[150:214,10]

#model building
library(class)
#KNN = 3
glass_pred_train <- knn(train = glass_train, test = glass_train, cl = glass_train_labels, k = 3)
mean(glass_pred_train==glass_train_labels) #training accurary 0.91%

glass_pred_test <- knn(train = glass_test, test = glass_test, cl = glass_test_labels, k = 3)
mean(glass_pred_test==glass_test_labels) #test accuracy %0.93

glass_pred_train_test <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k = 3)

#creating cross tabulation of predicted vs actual
CrossTable(x = glass_test_labels, y = glass_pred_train_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

#trying different values of K
glass_pred_train_test <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k = 6)
CrossTable(x = glass_test_labels, y = glass_pred_train_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

glass_pred_train_test <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k = 9)
CrossTable(x = glass_test_labels, y = glass_pred_train_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

#finding best value for k
glass_train_acc <- NULL
glass_test_acc<- NULL
for (i in seq(2,50,2))
{
  glass_pred_train <- knn(train = glass_train,test = glass_train,cl = glass_train_labels,k = i) 
  glass_train_acc<- c(glass_train_acc,mean(glass_pred_train==glass_train_labels))
  glass_pred_test <- knn(train = glass_test,test = glass_test,cl = glass_test_labels,k = i)
  glass_test_acc<-c(glass_test_acc,mean((glass_pred_test==glass_test_labels)))
}

accuracy <- data.frame(list(glass_train_acc=glass_train_acc, glass_test_acc=glass_test_acc,k_value=seq(2,50,2)))

#Plot
library(ggplot2)
ggplot(accuracy,aes(x=accuracy$k_value))+
  geom_line(aes(y=accuracy$glass_train_acc,colour="train_acc"),lwd=1)+
  geom_line(aes(y=accuracy$glass_test_acc,colour="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#optimal value of k can be 20 because after that accuracy is decreasing with all values of k
