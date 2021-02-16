summary(Zoo)
zoo <- Zoo[,-1] #dropping column 1
sum(is.na(zoo)) #checking null values
table(zoo$type)
prop.table(table(zoo$type))
prop.table(table(zoo$type))*100 
round(prop.table(table(zoo$type))*100, digits = 1) #rounding off 
boxplot(zoo)
str(zoo)

zoo$type <- as.factor(zoo$type)
levels(zoo$type)
zoo_data <- factor(zoo$type, levels = c("1","2","3","4","5","6","7"), labels = c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean"))
str(zoo)

#normalizing dataset
zz_norm <- scale(zoo[,1:16])

#spliting dataset into train and test
zoo_train <- zz_norm[1:75,]
zoo_test <- zz_norm[76:101,]

#get labels for train and test dataset
zoo_train_labels <- zoo[1:75,17]
zoo_test_labels <- zoo[76:101,17]

#model building
library(class)
zoo_pred_train <- knn(train = zoo_train, test = zoo_train, cl = zoo_train_labels,k = 2)
mean(zoo_pred_train==zoo_train_labels) #train accuracy 0.98%

zoo_pred_test <- knn(train = zoo_test, test = zoo_test,cl = zoo_test_labels,k = 2)
mean(zoo_pred_test==zoo_test_labels) #test accuracy 0.92%

zoo_pred_train_test <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k = 2)

#creating cross tabulation of predicted vs actual
CrossTable(x = zoo_test_labels, y = zoo_pred_train_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

#trying different values of K
zoo_pred_train_test <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k = 3)
CrossTable(x = zoo_test_labels, y = zoo_pred_train_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

zoo_pred_train_test <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k = 5)
CrossTable(x = zoo_test_labels, y = zoo_pred_train_test, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

#finding best value for k
zoo_train_acc <- NULL
zoo_test_acc<- NULL
for (i in seq(2,20,2))
{
  zoo_pred_train <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_labels,k=i) 
  zoo_train_acc<- c(zoo_train_acc,mean(zoo_pred_train==zoo_train_labels))
  zoo_pred_test <- knn(train=zoo_test,test=zoo_test,cl=zoo_test_labels,k=i)
  zoo_test_acc<-c(zoo_test_acc,mean((zoo_pred_test==zoo_test_labels)))
}

accuracy <- data.frame(list(zoo_train_acc=zoo_train_acc, zoo_test_acc=zoo_test_acc,k_value=seq(2,20,2)))

#Plot
library(ggplot2)
ggplot(accuracy,aes(x=accuracy$k_value))+
  geom_line(aes(y=accuracy$zoo_train_acc,colour="train_acc"),lwd=1)+
  geom_line(aes(y=accuracy$zoo_test_acc,colour="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


#optimal value of k can be 12 because after that accuracy is decreasing with all values of k
