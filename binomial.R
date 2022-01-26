# create vector X 
x <- seq(0,10,1) # or x <- 0:10:1
# binomial distribution of x
px <- choose(10,x)*0.4^x*0.6^(10-x)
px <- round(px,4) # round up to 4 decimal number
# built-in function 
px <- dbinom(x,10,0.4)
px <- round(px,4)
Fx <- round(pbinom (x,10,0.4),digits=4)
Fx # = P(x<=x)
x*px # = xP(X=x)
mu <- sum(x*px) # = mu # expected value E(X)
vx <- sum((x-mu)^2*px) # variance of x or V(X)
10*0.4 #mu
10*0.4*0.6 #vx
dbinom(3,10,0.4) # P(X=3)
sum(dbinom(2:4,10,0.4)) # P(2<=X<=4)
pbinom(4,10,0.4)-pbinom(1,10,0.4) # P(X<=4)-P(X<=1)
