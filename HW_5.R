library(datasets)
library(vcd)
library(RcmdrMisc)

admission <- data.frame(UCBAdmissions)
subsetAdmit <- subset(admission, Dept=="A" | Dept=="B" | Dept=="C") #subsetting only for the tutorial
subsetAdmit$Dept <- droplevels(subsetAdmit$Dept) #only for the tutorial

#1
table1 <- xtabs(subsetAdmit$Freq~subsetAdmit$Gender+subsetAdmit$Admit)
table1

(table1[1,1]/table1[2,1]) / (table1[1,2]/table1[2,2])

OR

loddsratio(table1, log=F) 
#2
mosaicplot(table1)

#3 - saturated model for a two-way table - A * B
model1 <- glm(Freq ~ Admit * Gender, family = poisson(log), data=subsetAdmit)
model1

subsetAdmit$predicted <- model1$fitted.values

table2 <- xtabs(subsetAdmit$predicted~subsetAdmit$Gender+subsetAdmit$Admit)
table2
loddsratio(table2, log=F) 

#4 - independence model for a three-way table - A + B + C
model2 <- glm(Freq ~ Admit + Gender + Dept, family = poisson(log), data=subsetAdmit)
model2
summary(model2)

subsetAdmit$predictedInd <- model2$fitted.values

table3 <- xtabs(subsetAdmit$Freq ~ subsetAdmit$Gender + subsetAdmit$Admit + subsetAdmit$Dept)
structable(table3)


table4 <- xtabs(subsetAdmit$predictedInd ~ subsetAdmit$Gender + subsetAdmit$Admit + subsetAdmit$Dept)
round(structable(table4), 0)

(table4[1,1,1]/table4[1,2,1]) / (table4[2,1,1]/table4[2,2,1]) # for Dept A
(table4[1,1,2]/table4[1,2,2]) / (table4[2,1,2]/table4[2,2,2]) # for Dept B
(table4[1,1,3]/table4[1,2,3]) / (table4[2,1,3]/table4[2,2,3]) # for Dept C

OR 

loddsratio(table4, log=F)

#5
mosaicplot(table3[1:2,1:2,1]) # for Dept A
mosaicplot(table3[1:2,1:2,2]) # for Dept B
mosaicplot(table3[1:2,1:2,3]) # for Dept C

#6 - homogenous association model
model3 <- glm(Freq ~ Admit*Gender + Admit*Dept + Gender*Dept, family = poisson(log), data=subsetAdmit)
model3
summary(model3)

subsetAdmit$predicted6 <- model3$fitted.values

table5 <- xtabs(subsetAdmit$predicted6 ~ subsetAdmit$Gender + subsetAdmit$Admit + subsetAdmit$Dept)
round(structable(table5), 0)

structable(table3)

round(structable(table4), 0)

#estimated odds ratio to be calculated from table5 - check code for Q4

#7
model4 <- stepwise(model3, "backward/forward", criterion="BIC")
summary(model4)

#8
model5 <- glm(Freq ~ Admit*Gender*Dept, family = poisson(log), data=subsetAdmit)
summary(model5)

AIC(model4, model5)
BIC(model4, model5)
anova(model4, model5, test="Chisq")

#9
attach(subsetAdmit)
UCBAdmit <- cbind(subsetAdmit[Admit=="Rejected",-1],
                  A=subsetAdmit[Admit=="Admitted", "Freq"])
names(UCBAdmit)[3] <- "R"
detach(subsetAdmit)

model6 <- glm(cbind(UCBAdmit$R, UCBAdmit$A)~Dept*Gender, family=binomial(link="logit"), data=UCBAdmit)
summary(model6)
Anova(model6)
