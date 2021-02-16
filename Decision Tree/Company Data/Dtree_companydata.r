install.packages("C50")
library(C50) #descision tree
library(caret) #confusion matrix
library(gmodels) #cross table

summary(Company_Data)
str(Company_Data)

#checking NA values
sum(is.na(Company_Data))

range(Company_Data$Sales)

#renaming dataset
company <- Company_Data

#as per problem statement converting y var sales into categorical var
companysales <- ifelse(company$Sales>8, "High","Low")
company <- data.frame(company,companysales)
company <- company[-1]
str(company)

#spliting the data into train & test
inTraininglocal <- createDataPartition(company$companysales, p = .7, list = F)
company_train <- company[inTraininglocal,]
company_test <- company[-inTraininglocal,]

#checking distribution accuracy
prop.table(table(company_train$companysales))
prop.table(table(company_test$companysales))

#Building model1 on train dataset
model1 <- C5.0(company_train$companysales~., data = company_train)
pred <- predict.C5.0(model1, company_test[,-11])
table(company_test$companysales, pred)
mean(company_test$companysales==pred) #73% accuracy

#Building model2 on train dataset
model2 <- C5.0(companysales~., data = company_train, trials = 20)
pred2 <- predict.C5.0(model2, company_test[,-11])
table(company_test$companysales, pred2)
mean(company_test$companysales==pred2) #78%accuracy

#Building model3 on train dataset
model3 <- C5.0(companysales~., data = company_train, trials = 100)
pred3 <- predict.C5.0(model3, company_test[,-11])
table(company_test$companysales, pred3)
mean(company_test$companysales==pred3) #79%accuracy

plot(model1)
plot(model2)
plot(model3)

#random sampling for 50 times in for loop
for( i in 1:50){
  model3 <- C5.0(companysales~.,data = company_train,trials=i)
  pred3 <- predict(model3,company_test)
  print(paste( i , mean(company_test$companysales==pred3)))
  }
