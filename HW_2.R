load("C:/Users/Arijit Ghosh/Desktop/OregonHomes.Rdata")
ds1<-homes
ds1$index<-c(1:77)
ds1<-subset(ds1, ds1$index<=50)

#Answer 1 
ds1$ncat[ds1$Gar<=1]<-"t1"
ds1$ncat[ds1$Gar>1 ]<-"t2"

boxplot(ds1$Price~ds1$ncat)

t.test(ds1$Price~ds1$ncat,var.equal=TRUE)

var.test(ds1$Price~ds1$ncat)

#Answer 2
a1<-aov(ds1$Price~ds1$ncat)
summary(a1)

temp1<-lm(Price~ncat, data=ds1)
summary(temp1)

#Answer 3
a2<-aov(ds1$Price~as.factor(ds1$Gar))
summary(a2)

TukeyHSD(a2,ordered=TRUE,conf.level = 0.95)


#Answer 4
temp2 <- lm(Price ~ .-ncat-index, data = ds1)
summary(temp2)

#Answer 7 
temp3<-lm(Price~.-ncat-index-Year, data=ds1)
temp4 <- step(temp3, direction="both")
summary(temp4)

#Answer 8 
library(car)
crPlots(temp4)
temp5<-lm(Price~ID+Floor+Age+Status+School, data=ds1)
summary(temp5)
