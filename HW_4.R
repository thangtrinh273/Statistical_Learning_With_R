
install.packages("foreign")
library(foreign) #necessary to be able to read .dta files

#reading the .dta file into R
program <-read.dta("C:/Users/Kitteh/Dropbox/R/hsbdemo (1).dta")
 #OR
program <-read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

#sampling the file for tutorial purposes
subsample <- program[50:150, ]

#1
install.packages("RcmdrMisc")
library(RcmdrMisc) #needed for colPercents/rowPercents function

table1 <- table(subsample$ses, subsample$prog)
table1

rowPercents(table1)
colPercents(table1)

#2
mosaicplot(table1, main="Program choice vs. socio-economic status")

subsample$academic <- NA
levels(subsample$academic)<- c("non-academic","academic")
subsample$academic[subsample$prog == "academic"] <- "academic"
subsample$academic[subsample$prog == "general"] <- "non-academic"
subsample$academic[subsample$prog == "vocation"] <- "non-academic"
subsample$academic <- ordered(subsample$academic, levels= c("non-academic","academic"))
table2 <- table(subsample$ses, subsample$academic)
table2

library(vcd) #needed for loddsratio function
loddsratio(table2, log=FALSE) #for odds ratio
loddsratio(table2) #for log odds ratio

#3
chisq.test(table1)

chisq.test(table1)$expected

table1 - chisq.test(table1)$expected

#4
Fsample <- subset(subsample, female=="female")
Msample <- subset(subsample, female=="male")

tableF <- table(Fsample$ses, Fsample$prog)
chisq.test(tableF)
chisq.test(tableF)$expected
tableF - chisq.test(tableF)$expected

tableM <- table(Msample$ses, Msample$prog)
chisq.test(tableM)
chisq.test(tableM)$expected
tableM - chisq.test(tableM)$expected

mosaicplot(tableF)

mosaicplot(tableM)

#5
install.packages("nnet")
library(nnet) #needed for multinom function

model1<- multinom(prog~female+ses+schtyp+read+write+math+science
                  +honors+awards,data=subsample, trace=FALSE)
summary(model1)

Wald <- summary(model1, cor=FALSE, Wald=TRUE)$Wald.ratios
p <- (1 - pnorm(abs(Wald), 0, 1))*2
p
p < 0.05

#6
library(RcmdrMisc)

model2<-stepwise(model1, "backward", criterion="AIC")

summary(model2)

BIC(model2)

logLik(model2)


#7
#the following dataframe is only an example for a situation in which the final model from Q6 had three predictors: ses, schtyp and math
#always double-check the dataframe to make sure all the rows are unique!

dframe <- data.frame(ses=rep(c("low", "middle", "high"),each = 2),
                     math = rep(mean(program$math), 6),
                     schtyp=rep(c("public", "private"), each=1))
                   
predict(model2, newdata=dframe, "probs")

#8
#the following dataframe is only an example for a situation in which the final model from Q6 had three predictors: ses, schtyp and math
#always double-check the dataframe to make sure all the rows are unique!

dframe2 <- data.frame(ses = rep(c("low", "middle", "high"),each=51),
                      math = rep(c(30:80),6),
                      schtyp=rep(c("public", "private"),each=51))

dframe3 <-predict(model2, dframe2, "probs")

dframe4 <- cbind(dframe2, dframe3) #we have to calculate means for every level of SES, that is why we bind those two dataframes

by(dframe4[,4:6], dframe4$ses, colMeans)


