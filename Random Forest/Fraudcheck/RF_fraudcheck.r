library(randomForest) #random forest
library(caret) #confusion matrix
library(gmodels) #cross table

summary(Fraud_check)
str(Fraud_check)

#checking NA values
sum(is.na(Fraud_check))
colSums(is.na(Fraud_check))

#renaming dataset
fraud <- Fraud_check

range(fraud$Taxable.Income)

#as per problem statement converting y var into categorical var
fraudtaxinc <- ifelse(fraud$Taxable.Income<=30000, "Risky", "Not Risky")
fraud <- data.frame(fraud, fraudtaxinc)
fraud <- fraud[-3]
str(fraud)
table(fraud$fraudtaxinc)
barplot(table(fraud$fraudtaxinc))
boxplot(fraud)

#spliting the data into train & test
set.seed(101)
inTraininglocal <- createDataPartition(fraud$fraudtaxinc, p = .7, list = F)
fraud_train <- fraud[inTraininglocal,]
fraud_test <- fraud[-inTraininglocal,]

#checking the distribution accuracy
prop.table(table(fraud_train$fraudtaxinc))
prop.table(table(fraud_test$fraudtaxinc))

#Building model 1
set.seed(101)
model1 <- randomForest(fraudtaxinc~., data = fraud_train, importance=TRUE)
pred1 <- predict(model1,fraud_test[,-6])
table(fraud_test$fraudtaxinc,pred1)
mean(fraud_test$fraudtaxinc==pred1) #75% accuracy
confusionMatrix(fraud_test$fraudtaxinc,pred1) #throws errror

print(model1)
print(importance(model1))

#Building model 2
set.seed(101)
model2 <- randomForest(fraudtaxinc~., data = fraud_train, ntree = 500)
pred2 <- predict(model2,fraud_test[,-6])
table(fraud_test$fraudtaxinc,pred2)
mean(fraud_test$fraudtaxinc==pred2) #77% accuracy
confusionMatrix(fraud_test$fraudtaxinc,pred2) #throws errror

print(model2)
print(importance(model2))
