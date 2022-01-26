##---------------------------------------------------- 
## joint distribution of x and y : f(x,y) = 4xy ;  0<x<1, 0<y<1
## marginal distribution of x    : f(x) = 2x  ; 0<x<1
## marginal distribution of y    : f(y) = 2y  ; 0<y<1

### mean of X
fx <- function(x) 2*x
x.fx <- function(x) 2*x^2
EX <- integrate(x.fx, lower = 0, upper = 1)
###  variance of X
x2.fx <- function(x) 2*x^3
EX2 <- integrate(x2.fx, lower = 0, upper = 1)
VX <- EX2$value - EX$value^2

### mean of Y
fy <- function(y) 2*y
y.fy <- function(y) 2*y^2
EY <- integrate(y.fy, lower = 0, upper = 1)
### variance of Y
y2.fy <- function(y) 2*y^3
EY2 <- integrate(y2.fy, lower = 0, upper = 1)
VY <- EY2$value - EY$value^2

#install.packages("pracma")
library(pracma)

### covariance of X and Y  
xy.fxy <- function(x,y) 4*x^2*y^2
EXY <- integral2(xy.fxy,xmin=0,xmax=1,ymin=0,ymax=1)
EXY
cov.XY <- EXY$Q-EX$value*EY$value
cov.XY

### Correlation coefficient of X and Y
corr.XY <- cov.XY/(sqrt(VX*VY))       
