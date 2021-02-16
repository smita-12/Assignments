install.packages("kernlab")
library(kernlab)

install.packages("psych")
library(psych)

install.packages("caret")
library(caret)

summary(forestfires)

str(forestfires)

is.na(forestfires)
anyNA(forestfires)

skew(forestfires)

forestfires <- forestfires[,-c(1,2)]

#checking the distribution
table(forestfires$size_category)

#normalization
forestfires_norm <- scale(forestfires[,-29]) #excluding categorical columns
forest_norm <- cbind(forestfires_norm, forestfires$size_category)
colnames(forest_norm)[29] <- "size_category"
forest_norm <- as.data.frame(forest_norm)

#converting categorical data into numeric
forest_norm$size_category[forest_norm$size_category==2] <- 0
forest_norm$size_category[forest_norm$size_category==1] <- 1
forest_norm$size_category <- as.factor(forest_norm$size_category)

#spliting dataset into test and train
train <- forest_norm[1:350,]
test <- forest_norm[351:517,]

#checking the accuracy
prop.table(table(train$size_category))
prop.table(table(test$size_category))

#Building the model on train data using vanilladot
model_van<-ksvm(size_category~.,data = train,kernel = "vanilladot")
predict_van<-predict(model_van,newdata = test)
mean(predict_van==test$size_category) #0.84

##Building the model on train data using rbfdot
model_rbf<-ksvm(size_category~.,data = train,kernel = "rbfdot")
predict_rbf<-predict(model_rbf,newdata = test)
mean(predict_rbf==test$size_category) #0.70

#Building the model on train data using polydot
model_poly<-ksvm(size_category~.,data = train,kernel = "polydot")
predict_poly<-predict(model_poly,newdata = test)
mean(predict_poly==test$size_category) #0.84


