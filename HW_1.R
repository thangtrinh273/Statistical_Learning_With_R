library(ggplot2)
install.packages('dplyr')
library(dplyr)
m1<-lm(formula = SALNOW ~ 1, data = bank)
summary(m1)
residual = bank$SALNOW - fitted(m1)
residual_sq = sum(residual^2)
residual_sq
#rss <- deviance(model1)
#rss
residual_std_error = sqrt(residual_sq / 473)
residual_std_error
m2<-lm(SALNOW ~EDLEVEL, data=bank)
summary(m2)
residual1 = bank$SALNOW - fitted(m2)
residual_sq_1 = sum(residual1^2)
residual_sq_1
rss <- deviance(m2)
rss
residual_std_error1 = sqrt(residual_sq_1 / 472)
residual_std_error1
m3<-lm(SALNOW ~EDLEVEL + SEX, data=bank) 
summary(m3)
residual2 = bank$SALNOW - fitted(m3)
residual_sq_2 = sum(residual2^2)
residual_sq_2
rss <- deviance(m3)
rss
residual_std_error2 = sqrt(residual_sq_2 / 471)
residual_std_error2
hist(residual2)
boxplot(residual2)
# Compute the density data
dens <- density(residual2)
# plot density
plot(dens, frame = FALSE, col = "steelblue", 
     main = "Density plot of residuals") 
qqnorm(residual2)
qqline(residual2)
ks.test(residual2,"pnorm",mean(residual2),sd(residual2))
model3 = fitted(m3)
plot(model3,bank$SALNOW,col = c("red", "blue"))
cor.test(model3,bank$SALNOW,method="pearson")
plot(bank$SALNOW,residual2,col = c("red", "blue"))
cor.test(residual2,bank$SALNOW,method="pearson")
plot(model3,residual2,col = c("red", "blue"))
cor.test(model3,residual2,method="pearson")

m4<-lm(formula = SALNOW ~ SALBEG + SEX + TIME + AGE + EDLEVEL + WORK + JOBCAT + MINORITY  , data = bank)
summary(m4)
m5<-lm(formula = SALNOW ~ SALBEG + SEX + TIME + EDLEVEL + WORK + JOBCAT + MINORITY  , data = bank)
summary(m5)
anova(m1,m2,m3,m4,m5)
