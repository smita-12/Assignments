summary(PlasticSales)
View(PlasticSales)
plot(PlasticSales$Sales, type="l")
table(PlasticSales$Month)
str(PlasticSales)
as.Date(PlasticSales$Month)
PlasticSales$Month

#Creating 11 dummy variables 
Psale<-data.frame(outer(rep(month.abb,length = 60),month.abb,"==")+ 0)
View(Psale)
colnames(Psale)<-month.abb
attach(Psale)
Psale<-cbind(PlasticSales,Psale)
Psale["t"] <- 1:60
Psale["log_sale"]<-log(Psale["Sales"])

Psale["t_square"]<-Psale["t"]*Psale["t"]
attach(Psale)

#Spliting the dataset into train and test
train_sale <- Psale[1:48,]
test_sale <- Psale[49:60,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train_sale)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test_sale))
View(linear_pred)
rmse_linear<-sqrt(mean((test_sale$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 260.937


######################### Exponential #################################

expo_model<-lm(log_sale~t,data=train_sale)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test_sale))
rmse_expo<-sqrt(mean((test_sale$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 268.693

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train_sale)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test_sale))
rmse_Quad<-sqrt(mean((test_sale$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad #297.406 

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_sale)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test_sale,interval='predict'))
rmse_sea_add<-sqrt(mean((test_sale$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #235.602

######################## Additive Seasonality with Linear #################

Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train_sale)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test_sale))
rmse_Add_sea_Linear<-sqrt(mean((test_sale$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear # 135.553

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data= train_sale)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test_sale))
rmse_Add_sea_Quad<-sqrt(mean((test_sale$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #218.193

################## Multiplicative Seasonality Linear trend ##########################

multi_linear_sea_model<-lm(log_sale~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_sale)
summary(multi_linear_sea_model) 
multi_linear_sea_pred<-data.frame(predict(multi_linear_sea_model,newdata=test_sale,interval='predict'))
rmse_multi_linear_sea<-sqrt(mean((test_sale$Sales-exp(multi_linear_sea_pred$fit))^2,na.rm = T))
rmse_multi_linear_sea # 160.683

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_Add_sea_Linear","rmse_multi_linear_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_Linear,rmse_multi_linear_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Additive Seasonality with Linear has the lowest RMSE##
final_sale<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=Psale)
summary(final_sale)
final_predict <- predict(final_sale)
res_final<-Psale$Sales- final_predict
acf(res_final,lag.max = 10)
