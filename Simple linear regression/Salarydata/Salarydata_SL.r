library(lattice)
library(ggplot2)
sd <- Salary_Data
str(sd)

attach(sd)
#Scatter plot
plot(YearsExperience,Salary)

#checking correleation
cor(YearsExperience,Salary) #0.97

#checking null values
sum(is.na(sd))

#checking outliers
boxplot(sd)

hist(sd$YearsExperience)
hist(sd$Salary)

summary(sd)

#model building
model <- lm(Salary~YearsExperience)
summary(model)
predict(model)
model$residuals
confint(model,level = 0.95)
predict(model,interval = "predict")
summary(model)

#transformation of data by applying log to it
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
model_log <- lm(Salary~log(YearsExperience))
summary(model_log)
predict(model_log)
model_log$residuals
confint(model_log,level = 0.95)
predict(model_log,interval = "predict")
