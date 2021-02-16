library(car)

Corolla <- ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")] 

pairs(Corolla)

summary(Corolla)

cor(Corolla)

attach(Corolla)

model <- lm(Corolla$Price~., data = Corolla)
summary(model)

model.cc <- lm(Price~cc, data = Corolla)
summary(model.cc)

model.doors <- lm(Price~Doors, data = Corolla)
summary(model.doors)

influenceIndexPlot(model)
influencePlot(model)

model1 <- lm(Price~., data = Corolla[-81,])
summary(model1)

vif(model1)

avPlots(model1)

model2 <- lm(Price~., data = Corolla[-222,])
summary(model2)
car::vif(model2)
