# chi-square test for independence

data <- c(476,543,211,90,31,44,22,8,31,28,9,7)
tbl <- matrix(data,nrow=3,ncol=4,byrow=TRUE)
chisq.test(tbl)

##
library(MASS)
head(survey)
Freq1 <- table(survey$Exer)
Freq2 <- table(survey$Exer)
Sum.r <- Freq1
tbl2 <- table(survey$Exer,survey$Smoke)
tbl2
colSums(tbl2)
rowSums(tbl2)
chisq.test(tbl2)

#####  Calculate Chi-square test
tbl2.re <- cbind(tbl2[,1],tbl2[,4],tbl2[,3],tbl2[,2])
tbl2.re <- rbind(tbl2.re[1,],tbl2.re[3,],tbl2.re[2,])
Sum.r <- rowSums(tbl2.re)
Sum.c <- colSums(tbl2.re)
E <- matrix(0,3,4)
for (i in 1:length(Sum.r)) 
  for (j in 1:length(Sum.c))
       E[i,j] <- Sum.r[i]*Sum.c[j]/sum(Sum.r)
E <- round(E,2)
chi <- round(((tbl2.re-E)^2)/E,4)
chisq.stat <- sum(chi)
chisq.crit <- qchisq(0.95,6)

####
tbl3 <- cbind(tbl2[,"Freq"],tbl2[,"None"]+tbl2[,"Some"])
tbl3
chisq.test(tbl3)

#-------------------------------------------
freq1 <- table(survey$Exer)
tb12<-table(survey$Smoke,survey$Exer)
tb2<-table(survey$Exer,survey$Smoke)
colSums(tb2)
rowSums(tb2)
chisq.test(tb12)

