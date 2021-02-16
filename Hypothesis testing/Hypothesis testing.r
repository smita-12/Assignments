summary(BuyerRatio)
#All variables are discrete & more than 2 variables
#H0 : Proportion of male and female ratio are same (Null Hypothesis)
#Ha : proportion of male and female ratio are not same (Alternative Hypothesis)

chisq.test(BuyerRatio[,-1]) #p-value 0.66

#p<0.05 so we accept null hypothesis
#########################################################################################

summary(`Costomer+OrderForm`)
install.packages("tidyr")
library(tidyr)

customer <- `Costomer+OrderForm`

#All variables are discrete & more than 2 varibales
#H0 : No training is required defective % are equal in all 4 centers (Null Hypothesis)
#Ha : Training is required defective % are not equal (Alternative Hypothesis)

#stacking data
Data<-gather(customer,country,Error,c(1:4)) 

chisq <- chisq.test(Data$country, Data$Error)
chisq #p-value 0.277

#p<0.277 so we accept null hypothesis
#########################################################################################

summary(Faltoons)
#All variables are discrete & only 2 variables so will go for 2 proportion test

attach(Faltoons)
table <- table(Weekdays, Weekend)
table

#H0 : proportion of female <= male visitor ( Null Hypothesis)
#Ha : Proportion of female > male visitor (Alternative Hypothesis)
#H0 : proportion of female=male visitor( Null Hypothesis)
#Ha : Proportion of female!=male visitor(Alternative Hypothesis)
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95,alternative = "two.sided")#p-value 1


#H0 : proportion of female< male visitor( Null Hypothesis)
#Ha : Proportion of female>male visitor(Alternative Hypothesis)
prop.test(x=c(66,47),n=c(167,120),conf.level = 0.95, correct = "TRUE", alternative = "less")#p-value 0.5

#p>0.05 so we accept null hypothesis
##########################################################################################
summary(Cutlets)
#Normality test
#H0 : No Difference in diameter between two units no action( Null Hypothesis)
#Ha : Difference in diameter between two units take action(Alternative Hypothesis)
install.packages("nortest")
library(nortest)
ad.test(Cutlets$Unit.A)#p-value 0.286 high > than p value 0.05
ad.test(Cutlets$Unit.B)#p-value 0.686 high > than p value 0.05

#Variance test
#H0: Variance are equal( Null Hypothesis)
#Ha: Variance are not equal(Alternative Hypothesis)
var.test(Cutlets$Unit.A,Cutlets$Unit.B)#p-value 0.313 high > than p value 0.05
#fail to reject null hypothesis

#Two sample t test 
#H0:do not take action when diameter of Y1=diameter of Y2( Null Hypothesis)
#Ha: Take action when diameter of Y1!=diameter of Y2 (Alternative Hypothesis)
attach(Cutlets)
t.test(Unit.A,Unit.B,alternative ="two.sided",conf.level = 0.95,var.equal = T)#p-value 0.472 high > than p value 0.05
#fail to reject null hypothesis
###########################################################################################
#LABTAT
#More than two continious varibales hence, we proceed with ANOVA

#Normality test
#H0 : No action, if sample 1,2,3,4 are normal ( Null Hypothesis)
#Ha: Take action , if sample 1 or 2 or 3 or 4 are not normal ( Alternative Hypothesis)
stacked_data<-stack(LabTAT) 
attach(stacked_data)
ad.test(stacked_data$values) #p-value 0.0507 high > than p value 0.05

#Variance test
#H0 : Variances are equal 
#Ha : Variances are not equal 
var.test(values,ind)
install.packages("car")
library(car)
leveneTest(stacked_data$values~stacked_data$ind,data = stacked_data) #p-value 0.05161 variance are equal

#one way ANOVA test
#H0 :  average Turn Around Time (TAT) for reports of 4 laboratories are same 
#Ha :  average  Turn Around Time (TAT) for reports of any one  laboratories are not same
Anova_data<-aov(values~ind,data = stacked_data) #p-value <2e-16 reject null hypothesis
summary(Anova_data)


#There is a difference in Average (TAT) between different laboratoryy


