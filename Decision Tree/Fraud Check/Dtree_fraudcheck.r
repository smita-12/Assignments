library(C50)
library(caret)
library(gmodels)

summary(Fraud_check)
str(Fraud_check)

#checking NA values
sum(is.na(Fraud_check))

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

#spliting the data into train & test
inTraininglocal <- createDataPartition(fraud$fraudtaxinc, p = .8, list = F)
fraud_train <- fraud[inTraininglocal,]
fraud_test <- fraud[-inTraininglocal,]

#checking the distribution accuracy
prop.table(table(fraud_train$fraudtaxinc))
prop.table(table(fraud_test$fraudtaxinc))

#Building model1 on train_data
model1 <- C5.0(fraudtaxinc~., data = fraud_train)
pred1 <- predict.C5.0(model1,fraud_test[,-6])
table(fraud_test$fraudtaxinc,pred1)
CrossTable(fraud_test$fraudtaxinc,pred1)
mean(fraud_test$fraudtaxinc==pred1)
confusionMatrix(fraud_test$fraudtaxinc,pred1) #throws error

#Building model2 on train_data
model2 <- C5.0(fraudtaxinc~., data = fraud_train, trials = 20)
pred2 <- predict.C5.0(model2,fraud_test[,-6])
table(fraud_test$fraudtaxinc,pred2)
mean(fraud_test$fraudtaxinc==pred2)

#Building model3 on train_data
model3 <- C5.0(fraudtaxinc~., data = fraud_train, trials = 20)
pred3 <- predict.C5.0(model3,fraud_test[,-6])
table(fraud_test$fraudtaxinc,pred3)
mean(fraud_test$fraudtaxinc==pred3) #80% accuracy
