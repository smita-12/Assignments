summary(CocaCola_Sales_Rawdata)
View(CocaCola_Sales_Rawdata)
plot(CocaCola_Sales_Rawdata$Sales, type="l") #visualizing trend
boxplot(CocaCola_Sales_Rawdata$Sales) #zero outliers
cocacola <- CocaCola_Sales_Rawdata

year <- rep(1986:1996,c(rep(4,length(1986:1996))))[1:42]
Quarters<- data.frame(outer(rep(c("Q1","Q2","Q3","Q4"),length = nrow(cocacola)),c("Q1","Q2","Q3","Q4"),"==") + 0 )
colnames(Quarters) <- c("Q1","Q2","Q3","Q4")

cocacola1 <-data.frame(year,Quarters,t=1:nrow(cocacola),Sales=cocacola$Sales) 
cocacola1["t_square"]<-cocacola1["t"]*cocacola1["t"]
head(cocacola1)

#spliting the dat into train and test
train_coca <- cocacola1[1:32,]
test_coca <- cocacola1[33:42,]

########################### LINEAR MODEL #############################
attach(cocacola)
linear_model<-lm(Sales~t,data=train_coca)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_coca))
View(linear_pred)
rmse_linear<-sqrt(mean((test_coca$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear #752.923


######################### Exponential #################################

expo_model<-lm(log(Sales)~t,data=train_coca)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test_coca))
rmse_expo<-sqrt(mean((test_coca$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 590.331

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train_coca)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test_coca))
rmse_Quad<-sqrt(mean((test_coca$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 457.735


######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train_coca)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test_coca,interval='predict'))
rmse_sea_add<-sqrt(mean((test_coca$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #1850.467

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train_coca)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test_coca))
rmse_Add_sea_Linear<-sqrt(mean((test_coca$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 673.443

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train_coca)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test_coca))
rmse_Add_sea_Quad<-sqrt(mean((test_coca$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #277.351

################## Multiplicative Seasonality Linear trend ##########################

multi_linear_sea_model<-lm(log(Sales)~t+Q1+Q2+Q3+Q4,data=train_coca)
summary(multi_linear_sea_model) 
multi_linear_sea_pred<-data.frame(predict(multi_linear_sea_model,newdata=test_coca,interval='predict'))
rmse_multi_linear_sea<-sqrt(mean((test_coca$Sales-exp(multi_linear_sea_pred$fit))^2,na.rm = T))
rmse_multi_linear_sea #448.86

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_linear_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_linear_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Additive Seasonality with Quadratic has the lowest RMSE
final_coca<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cocacola1)
summary(final_coca)
final_predict <- predict(final_coca)
res_final<- cocacola1$Sales - final_predict
acf(res_final,lag.max = 10) 
