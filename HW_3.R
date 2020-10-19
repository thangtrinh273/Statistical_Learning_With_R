
bank<-bankmarketing[1:1000,]

#1
model1<- glm(y~1, family=binomial, data=bank)
AIC(model1) #Akaike Information Criterion
BIC(model1) #Bayesian Information Criterion

summary(model1)
table(bank$y)
log[p/(1-p)] #log odds/ if brackets do not work, compute in single steps
#p = yes divided by yes+no

#for the current example:
p<-114/(886+114)

#2
model2<-glm(y~duration, family=binomial, data=bank)
AIC(model2)
summary(model2)

#in d) you need to transform the log odds to probability - remember to look at the model equation!

exp(-3.095) = Odds
p=Odds/(1+Odds)
p=exp(-3.095) / (1 + exp(-3.095))

#3
0.5 = Odds/(1+Odds)
#solve for Odds, then proceed to log(Odds)

model2$coefficients
intercept + b1 * duration = log(Odds)
#solve for b1

b1 * p(1-p) #tangent, p is already given!

#4
model3<-glm(y~age+marital+duration, family=binomial, data=bank)
summary(model3)

data3<-data.frame(age=mean(bank$age, na.rm=TRUE), marital="married", 
                  duration=mean(bank$duration, na.rm=TRUE))                                                                                                                   
predict(model3, data3, type="response")

data4<-data.frame(age=mean(bank$age, na.rm=TRUE), marital="married", 
                  duration=300) 
predict(model3, data4, type="response")

data5<-data.frame(age=mean(bank$age, na.rm=TRUE)+sd(bank$age), marital="married", 
                  duration=mean(bank$duration, na.rm=TRUE)) 
predict(model3, data5, type="response")

#5
model4<-glm(y~duration*campaign, family=binomial, data=bank)
Anova(model4)
summary(model4)
install.packages("effects")
library(effects)
plot(allEffects(model4))

#6
bank$durCentered<-bank$duration-mean(bank$duration, na.rm=TRUE)
bank$campCentered<-bank$campaign-mean(bank$campaign, na.rm=TRUE)
model5<-glm(y~durCentered*campCentered, family=binomial, data=bank)

Anova(model5)
summary(model5)
plot(allEffects(model5))

#7
model6<-glm(y~. -day -month -durCentered -campCentered, family=binomial, data=bank)
Anova(model6)
summary(model6)

#8
step.model<-step(model6, direction="both")
summary(step.model)
#9
boxplot(step.model$residuals)

bank$residuals<-step.model$residuals

datasub<-subset(bank, residuals > -1000)
boxplot(datasub$residuals)

model7<-glm(y ~ housing + loan + contact + duration + previous +
              poutcome, family=binomial, data=datasub)
boxplot(model7$residuals)
summary(model7)