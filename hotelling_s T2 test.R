str(iris)
head(iris)
View(iris)
tail(iris)

attach(iris)
x1 <- iris[Species=="setosa", 1:4]      #dataframe of iris "setosa"
x2 <- iris[Species=="versicolor", 1:4]  #dataframe of iris "versicolor"
x3 <- iris[Species=="virginica", 1:4]   #dataframe of iris "virginica"

#split dataframe
split.iris <- split(iris[,-5],Species)  #=split.iris <- split(iris[,1:4],Species)
x1 <- split.iris[[1]]
x2 <- split.iris[[2]]
x3 <- split.iris[[3]]

# mean and standard deviation
## x1 
#xbar11 <- mean(x1$Sepal.Length) ;sd11 <- sd(x1$Sepal.Length)  ;v11 <- var(x1$Sepal.Length)
#xbar12 <- mean(x1$Sepal.Width)  ;sd12 <- sd(x1$Sepal.Width)   ;v12 <- var(x1$Sepal.Width) 
#xbar13 <- mean(x1$Petal.Length) ;sd13 <- sd(x1$Petal.Length)  ;v13 <- var(x1$Petal.Length) 
#xbar14 <- mean(x1$Petal.Width)  ;sd14 <- sd(x1$Petal.Width)   ;v14 <- var(x1$Petal.Width) 
xbar1 <- colMeans(x1) #mean of x 
S1 <- cov(x1) ; #S1 <- round(cov(x1),3)

## x2 
#xbar21 <- mean(x2$Sepal.Length) ;sd21 <- sd(x2$Sepal.Length)  ;v21 <- var(x2$Sepal.Length)
#xbar22 <- mean(x2$Sepal.Width)  ;sd22 <- sd(x2$Sepal.Width)   ;v22 <- var(x2$Sepal.Width) 
#xbar23 <- mean(x2$Petal.Length) ;sd23 <- sd(x2$Petal.Length)  ;v23 <- var(x2$Petal.Length) 
#xbar24 <- mean(x2$Petal.Width)  ;sd24 <- sd(x2$Petal.Width)   ;v24 <- var(x2$Petal.Width) 
xbar2 <- colMeans(x2)
S2 <- cov(x2) ; #S2 <- round(cov(x2),3)

## x3
#xbar31 <- mean(x3$Sepal.Length) ;sd31 <- sd(x3$Sepal.Length)  ;v31 <- var(x3$Sepal.Length)
#xbar32 <- mean(x3$Sepal.Width)  ;sd32 <- sd(x3$Sepal.Width)   ;v32 <- var(x3$Sepal.Width) 
#xbar33 <- mean(x3$Petal.Length) ;sd33 <- sd(x3$Petal.Length)  ;v33 <- var(x3$Petal.Length) 
#xbar34 <- mean(x3$Petal.Width)  ;sd34 <- sd(x3$Petal.Width)   ;v34 <- var(x3$Petal.Width) 
xbar3 <- colMeans(x3)
S3 <- cov(x3) ; #S3 <- round(cov(x3),3)

#-------------------------------------------------
## Hotelling's T2 test [one - sample]
mu1 <- c(5,3.45,1.5,0.25)
n <- dim(x1)[1] #n <- 50 
p <- 4

s1.inv <- solve(S1) #inverse matrix of S1

#t2.stat
T2.stat <- n*t(xbar1-mu1) %*% s1.inv %*% (xbar1-mu1)
T2.stat <- as.numeric(T2.stat)

#F.stat
F.stat <- (n-p)/((n-1)*p) * T2.stat 

#critical value 
F.crit <- qf(1-0.05,p,n-p) 

#p - value 
P.value <- 1-pf(F.stat,p,n-p) 

### Hotelling's T2 test  using package  
#install.packages("ICSNP")
#install.packages("mvtnorm")
library(ICSNP)
library(mvtnorm)

HotellingsT2(x1, Y=NULL ,mu =mu1,test = "f")  # T.2 == table.F
#HotellingsT2(x1, Y=NULL ,mu =mu1,test = "chi")  # T.2 == table.chisq

#-------------------------------------------------
### Hotelling's T2 test  using package  [two - sample]
#mu=c(0,0,0,0)
n1 <- dim(x1)[1]
n2 <- dim(x2)[1]
p <- 4

#pooled covariace matrix
sp <- (((n1-1)*S1) + ((n2-1)*S2)) / (n1+n2-2)

sp.inv <- solve( ((1/n1)+(1/n2))*sp )

#t2.stat
T2.stat <- t(xbar1-xbar2) %*% sp.inv  %*% (xbar1-xbar2)
T2.stat <- as.numeric(T2.stat)

#f.stat
F.stat <- (n1+n2-p-1)/((n1+n2-2)*p)  * T2.stat 

#critical value 
qf(1-0.05,p,n1+n2-p-1)

#p - value 
1-pf(F.stat,p,n1+n2-p-1)  

### Hotelling's T2 test  using package  
HotellingsT2(x1,x2 ,mu =NULL,test = "f")  # T.2 == table.F
