sal_train <- SalaryData_Train
sal_test <- SalaryData_Test

summary(sal_train)
summary(sal_test)

#checking null values
sum(is.na(sal_train))
sum(is.na(sal_test))

#checking proportion
table(sal_train$Salary)
table(sal_test$Salary)

str(sal_train)
str(sal_test)

library(ggplot2)
ggplot(data=sal_train,aes(x=Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=sal_train,aes(x=Salary, y = educationno, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=sal_train,aes(x=Salary, y = hoursperweek, fill = Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

install.packages("e1071")
library(e1071)

#model building
model <- naiveBayes(sal_train$Salary~.,data = sal_train)
model$levels
model_pred <- predict(model,sal_test)
model_pred[1:400]

#confusion matrix
confusionMatrix(model_pred,sal_test$Salary)

#accuracy
mean(model_pred==sal_test$Salary) #0.81% accuracy

library(gmodels)
#creating cross tabulation of predicted vs actual
CrossTable(model_pred,sal_test$Salary,
           prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
           dnn = c('predicted','actual'))
