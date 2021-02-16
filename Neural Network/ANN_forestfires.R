install.packages("neuralnet")
library(neuralnet)
library(caret) #for confusion matrix
library(gmodels) #for cross table

summary(forestfires)
str(forestfires)

#checking NA values
is.na(forestfires)
anyNA(forestfires)
sum(is.na(forestfires))

#checking the distribution & accuracy
table(forestfires$size_category)
prop.table(table(forestfires$size_category))

#removing categorical column
forestfires <- forestfires[,-c(1,2)]

#Normalization of data
forest_norm <- scale(forestfires[,-29])
forest_norm <- cbind(forest_norm,forestfires$size_category)
colnames(forest_norm)[29] <- "size_category"
forest_norm <- as.data.frame(forest_norm)

#converting categorical column into numeric
forest_norm$size_category[forest_norm$size_category==2] <- 0
forest_norm$size_category[forest_norm$size_category==1] <- 1
#forest_norm$size_category <- as.factor(forest_norm$size_category)

#spliting data into train & test
forest_train <- forest_norm[1:350,]
forest_test <- forest_norm[351:517,]

#checking accuracy of train & test
prop.table(table(forest_train$size_category))
prop.table(table(forest_test$size_category))

attach(forest_norm)

#Building ANN model with single neuron
forest_model <- neuralnet(formula = size_category~.,act.fct = "logistic",
                          linear.output = FALSE,data = forest_train)

str(forest_model)

#visualize network topology
#windows()
plot(forest_model)

#Evaluating model performance
forest_result <- compute(forest_model, forest_test[,1:28])

#obtain predicted strength values
predicted_stregth <- forest_result$net.result

#examine corr b/w predicted & actual values
cor(predicted_stregth, forest_test$size_category) #throws error

#Improving model performance by adding hidden neurons
forest_model2 <- neuralnet(formula = size_category~., data = forest_train, hidden = 2)
plot(forest_model2)
forest_result2 <- compute(forest_model2, forest_test[,1:28])
predicted_stregth2 <- forest_result$net.result
cor(predicted_stregth2, forest_test$size_category)
