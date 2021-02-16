install.packages("randomForest")
library(randomForest)
library(caret)

summary(Company_Data)
str(Company_Data)

#checking NA values
sum(is.na(Company_Data))

range(Company_Data$Sales)

#renaming dataset
company <- Company_Data

#checking ouliers
boxplot(company)

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

#Building model
set.seed(101)
model1 <- randomForest(companysales~., data = company_train)
pred1 <- predict(model1, company_test[,-11])
table(company_test$companysales, pred1)
mean(company_test$companysales==pred1) #87% accuracy

print(model1)
print(importance(model1))
plot(model1)

#Building simple model2
set.seed(101)
model2 <- randomForest(companysales~., data = company)
pred2 <- predict(model2,company[,-11])
table(company$companysales, pred2)  
mean(company$companysales==pred2)

print(model2)
print(importance(model2))
plot(model2)

#Building model3 with 500 tree
model3 <- randomForest(companysales~., data = company, ntree = 500)
pred3 <- predict(model3,company[,-11])
table(company$companysales, pred3)  
mean(company$companysales==pred3)

print(model3)
print(importance(model3))
plot(model3)


