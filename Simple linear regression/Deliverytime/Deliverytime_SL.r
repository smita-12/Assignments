library(lattice)
library(ggplot2)
dt <- delivery_time
str(dt)

attach(dt)
#Scatter plot
plot(Delivery.Time,Sorting.Time)

#checking correleation
cor(Delivery.Time,Sorting.Time) #0.825

#checking null values
sum(is.na(dt))

#checking outliers
boxplot(dt)

summary(dt)

#model building
model <- lm(Delivery.Time~Sorting.Time)
summary(model)
predict(model)
model$residuals
confint(model,level = 0.95)
predict(model,interval = "predict")
summary(model)

#transformation of data by applying log to it
plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time) #0.83
model_log <- lm(Delivery.Time~log(Sorting.Time))
summary(model_log)
predict(model_log)
model_log$residuals
confint(model_log,level=0.95)
predict(model_log,interval = "predict")
