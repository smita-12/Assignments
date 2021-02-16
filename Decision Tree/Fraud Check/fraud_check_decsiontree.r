install.packages("caret")
install.packages("C50")
install.packages("gmodels")

library(caret)
library(C50)
library(gmodels)

head(Fraud_check)

str(Fraud_check)

colSums(is.na(Fraud_check))

summary(Fraud_check)

hist(Fraud_check$Taxable.Income)

#Converting Taxable Income into categorical as <= 30000, risky else good
tax_inc <- ifelse(Fraud_check$Taxable.Income <= 30000, "risky", "good")
mydata <- data.frame(tax_inc, Fraud_check[,-3])
View(mydata)

set.seed(100)
#training <- createDataPartition(mydata$tax_inc, P = 0.75, list = F)
#train_tax <- mydata[training,]
#test_tax <- mydata[-training]

#Spliting data into train and test
train_tax <- mydata[1:450,]
test_tax <- mydata[451:600,]

str(train_tax)
str(test_tax)

#checking count for y variable
table(train_tax$tax_inc)
table(test_tax$tax_inc)

#checking count with accuracy for y variable
prop.table(table(train_tax$tax_inc))
prop.table(table(test_tax$tax_inc))

#model using C50
model <- C5.0(train_tax$tax_inc~., data = train_tax, trails=100)
summary(model)

#testing accuracy
pred <- predict(model, test_tax)
table(pred, test_tax$tax_inc)
mean(pred==test_tax$tax_inc)          ##.85 accuracy
CrossTable(pred,test_tax$tax_inc)

plot(model)
