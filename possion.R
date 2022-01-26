# create vector X 
x <- seq(0,20) # or x <- 0:20
px <- dpois(x,3)
px <- round(px,4)
Fx <- ppois(x,3)
Fx <- round(Fx,4)
sum(px)
sum(x*px)
Ex <- sum(x*px) # Ex = mu # expected value E(X)
vx <- sum((x-Ex)^2*px) # variance of x or V(X)
