install.packages("car")
library(car)

startups <- `50_Startups`
pairs(startups)

startups$copyofstate <- NA
startups$copyofstate[startups$State=="California"]=0
startups$copyofstate[startups$State=="New York"]=1
startups$copyofstate[startups$State=="Florida"]=2

startups[,-4]

plot(startups)

install.packages("corpcor")
library(corpcor)

cor(startups[,-5]) #throws error x must be numeric
cor(startups) #throws error x must be numeric
cor2pcor(cor(startups)) #throws error x must be numeric

boxplot(startups)

#Full model
model <- lm(startups$Profit~.,data = startups)
summary(model) #R2 0.95 R:D Significant

#model admin
modeladmin <- lm(startups$Profit~startups$Administration,data = startups)
summary(modeladmin) #Not significant

#model marketing
modelmarkt <- lm(startups$Profit~startups$Marketing.Spend,data = startups)
summary(modelmarkt) #R2 0.55 significant

#model administration & marketing
modeladminmarkt <- lm(startups$Profit~startups$Administration+startups$Marketing.Spend, data = startups)
summary(modeladminmarkt) #R2 0.60 both significant

#Correlation check
cor(startups$Administration,startups$Marketing.Spend)
plot(startups$Administration,startups$Marketing.Spend)

#Correlation check with VIF
car::vif(model)
vif(model)

library(MASS)
stepAIC(model)

#final model
finalstartup <- lm(startups$Profit~startups$R.D.Spend+startups$Marketing.Spend,data = startups[-c(47),])
summary(finalstartup)

influenceIndexPlot(finalstartup)
plot(finalstartup)
influence.measures(model)

plot(startups)



