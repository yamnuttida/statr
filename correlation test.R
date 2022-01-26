pulse <- c(72,75,76,74,64,72,82,66,78,81,68,64,88,72,68)
pressure <- c(114,135,125,132,115,122,140,110,122,130,117,115,155,121,122)
plot(pulse,pressure,col="blue",pch = 20,cex=1.5)
reg <- lm(pressure~pulse)
summary(reg)

data.pre<-data.frame(pulse=c(80))
predict(reg,newdata = data.pre,interval = "prediction",level = 0.95)
          
anova(reg)

cor.test(pulse,pressure,alternative = "two.sided")