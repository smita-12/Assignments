library(dplyr)
library(ggplot2)

summary(bank_data)

#checking the no.of columns and rows
ncol(bank_data)
nrow(bank_data)

#overview of dataset head, str, summary)
head(bank_data)
str(bank_data)
summary(bank_data)

#distribution of y variable
summary(bank_data$y)

#checking missing values in dataset
colSums(is.na(bank_data))

colSums(bank_data == "")

#checking for extra white space and removing the same for character features
bank_data[, sapply( bank_data, is.character )] <- sapply( bank_data[, sapply( bank_data, is.character )], trimws)

glimpse(bank_data)

####creating dummy variables by combining similar categories for variable job (char type)

t <- table(bank_data$job)
sort(t)

final <- round(prop.table(table(bank_data$job, bank_data$y),1)*100,1)
View(final)
final

s<-addmargins(final,2)
sort(s[,1])

View(s)
##############################################################################

bank_data=bank_data %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)

glimpse(bank_data)
##############################################################################

t <- table(bank_data$marital)
sort(t)

bank_data <- bank_data %>%
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))
         )%>%
  select(-marital)

glimpse(bank_data)
##############################################################################

t <- table(bank_data$education)
sort(t)

bank_data <- bank_data %>%
  mutate(primary=as.numeric(education %in% c("primary")),
         tertiary=as.numeric(education %in% c("tertiary")),
         secondary=as.numeric(education %in% c("secondary"))
         )%>%
  select(-education)

glimpse(bank_data)
##############################################################################

t <- table(bank_data$contact)
sort(t)

bank_data <- bank_data%>%
  mutate(cellular=as.numeric(contact %in% c("cellular")),
         telephone=as.numeric(contact %in% c("telephone"))
         )%>%
  select(-contact)

glimpse(bank_data)
##############################################################################

t=table(bank_data$poutcome)
sort(t)

bank_data=bank_data %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)
glimpse(bank_data)
##############################################################################

table(bank_data$loan)

bank_data$loan<-as.numeric(bank_data$loan=="yes")
glimpse(bank_data)
##############################################################################

table(bank_data$housing)

bank_data$housing<-as.numeric(bank_data$housing=="yes")
glimpse(bank_data)
##############################################################################

table(bank_data$default)

bank_data$default<-as.numeric(bank_data$default=="yes")
glimpse(bank_data)
##############################################################################

table(bank_data$month)

finalmnth=round(prop.table(table(bank_data$month,bank_data$y),1)*100,1)
sss=addmargins(finalmnth,2) #adding margin across Y
sort(sss[,1])

bank_data=bank_data %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)
glimpse(bank_data)
##############################################################################

table(bank_data$y)

bank_data$y=as.numeric(bank_data$y=="yes")
glimpse(bank_data)
##############################################################################

attach(bank_data)

#Liner Regression model
fit <- lm(bank_data$y~., data = bank_data)
summary(fit)

#Residual Plot, QQ Plot, Std.Residual vs Fitted, Cooks Distance
plot(fit)

#finding outliers
boxplot(bank_data$age)
boxplot(bank_data$balance)
boxplot(bank_data$duration)


install.packages("aod")
library("aod")

windows()

install.packages("corpcor")
library(corpcor)
cor(bank_data)
cor2pcor(cor(bank_data))

install.packages("car")
library(car)

viforder <- vif(fit)
sort(viforder,decreasing = T) [1:5]

#Logistic Regression Model
logit <- glm(bank_data$y~., family = "binomial", data = bank_data)
summary(logit)

exp(coef(logit))

#Confusion MAtrix Table
prob <- predict(logit, type = c("response"), bank_data)
prob
confusion <- table(prob>0.5, bank_data$y)
confusion

#Model Accuracy
Accuracy <- sum(diag(confusion))/sum(confusion)
Accuracy

#ROC Curve
install.packages("pROC")
library(pROC)

roccurve <- roc(bank_data$y~prob)
plot(roccurve)
