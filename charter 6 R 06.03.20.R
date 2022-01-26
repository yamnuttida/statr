library(prob)

s <- rolldie(2,makespace = T) #prob is makespace=T
addrv(s, FUN = max, invars = c("X1","X2"), name = "U") #add column name'U' that 'U' is max of X1,X2
addrv(s, FUN = sum, invars = c("X1","X2"), name = "V") #add column name'V' that 'V' is sum of X1,X2
#addrv is Adding Random Variables to a Probability Space [Adds a column to a data frame probability space]

#store "U", "V" in s
s <- addrv(s, FUN = max, invars = c("X1","X2"), name = "U")   #dataframe
s <- addrv(s, FUN = sum, invars = c("X1","X2"), name = "V")   #dataframe

#marginal is find prob in edge #joint distibution #funtion sum prob #table6.3 in document
UV <- marginal(s, vars = c("U", "V"))                         #dataframe
PU <- marginal(s, vars = "U") #sum of column in table6.3      #dataframe
PV <- marginal(s, vars = "V") #sum of row in table6.3         #dataframe

#same PU,PV in format table
xtabs(formula = probs~U+V, data = UV)
xtabs(round(probs,3)~U+V, data = UV) #round is cut fraction

EU <- sum(PU$U * PU$probs) # expectrd of U #E(U) = U*probsU
EV <- sum(PV$V * PV$probs) # expectrd of V #E(V) = V*probsV
EUV <- sum(UV$U * UV$V * UV$probs) # expectrd of UV #E(UV) = U*V*probsUV

COV.UV <- EUV-(EU*EV) # corrvariance of UV
Var.U <- sum((PU$U-EU)^2 * PU$probs) #variance of U
Var.V <- sum((PV$V-EV)^2 * PV$probs) #variance of V

#corrlation coefficient of U and V
rho <- COV.UV / sqrt (Var.U * Var.V) #rho=COV.xy


####-----------------------------------------------------------------------------####
#install.packages("pracma")
library(pracma)
# Example 6.3 & 6.5
x.fx <- function(x) {6/5 * (x^2+1 /3*x)} #build funtion x
EX <- integrate(x.fx, lower = 0, upper = 1)   

y.fy <- function(y) {6/5 * (1/2*y + y^3)} #build funtion y
EY <- integrate(y.fy, lower = 0, upper = 1)

xy.fxy <- function(x,y) {6/5 * (x^2*y + x*y^3)}
EXY <- integral2(xy.fxy,xmin = 0, xmax = 1, ymin = 0, ymax = 1) #integral2 is 2 variable

COV.xy <- EXY$Q - EX$value * EY$value
