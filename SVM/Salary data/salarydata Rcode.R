install.packages("kernlab")
library(kernlab)

summary(test_salarydata)
summary(train_salarydata)

str(test_salarydata)
str(train_salarydata)

is.na(test_salarydata)
is.na(train_salarydata)

boxplot(train_salarydata)
boxplot(train_salarydata$age)
boxplot(train_salarydata$capitalgain,train_salarydata$Salary)


plot(train_salarydata$age, train_salarydata$Salary)
plot(train_salarydata$workclass,train_salarydata$Salary)
plot(train_salarydata$education, train_salarydata$Salary)




#tranîng a model on data
salaryclassifier <- ksvm(train_salarydata$Salary~., data = train_salarydata, kernel = "vanilladot")

#predictions on testing data
salaryprediction <- predict(salaryclassifier,test_salarydata)

head(salaryprediction)

#creating matrix
table(salaryprediction)

#checking actual no of misclassification
agreement <- salaryprediction == test_salarydata$Salary
table(agreement)

#finding the percentage of misclassification
prop.table(table(agreement)) #  FALSE      TRUE 
                            #0.1537185 0.8462815 

#Improving the model performance
salaryclassifier_rbf <- ksvm(train_salarydata$Salary~., data = train_salarydata, kernel = "rbfdot")

salaryprediction_rbf <- predict(salaryclassifier_rbf, test_salarydata)

head(salaryprediction_rbf)

table(salaryprediction_rbf)

agreement_rbf <- salaryprediction_rbf == test_salarydata$Salary

prop.table(table(agreement_rbf)) # FALSE      TRUE 
                                #0.1456175 0.8543825 

