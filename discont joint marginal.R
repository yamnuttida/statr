library(prob)
ball <- c("R1","R2","W1","W2","W3")
S <- urnsamples(ball,size=5,replace=F,order=T)
S <- probspace(S)
nS <- dim(S)[1]
###
###  create random variable X
X <- rep(NA,nS)     # create vector x 
Y <- rep(NA,nS)   
for (i in 1:120) {
if ((S$X1[i]=="R1") | (S$X1[i]=="R2")) {
    X[i] = 0
} else {
    if ((S$X2[i]=="R1")| (S$X2[i]=="R2")) {
      X[i] = 1
    } else {
              if ((S$X3[i]=="R1")| (S$X3[i]=="R2")) {
                X[i] = 2
              } else {
                   X[i] = 3
              }
            }
   }
}
##
###  create random variable Y
Y <- rep(NA,120)
for (i in 1:120) {
  if ((S$X1[i]=="W1") | (S$X1[i]=="W2")| (S$X1[i]=="W3")) {
    Y[i] = 0
  } else {
    if ((S$X2[i]=="W1")| (S$X2[i]=="W2")|(S$X2[i]=="W3")) {
      Y[i] = 1
    } else {
       Y[i] = 2
      }
    }
}

## Probabilty space S with X and Y random variables 
S <- data.frame(S,X,Y)                

PX <- marginal(S,vars = "X")          # marginal distribution of X
table(S$X)                            # frequency table of X

PY <- marginal(S,vars = "Y")          # marginal distribution of X
table(S$Y)                            # frequency table of X
 
PXY <- marginal(S,vars = c("X","Y"))  # Joint distribution of X and Y 
xtabs(probs~X+Y,data = PXY)           # crosstab table

EX <- sum(PX$X*PX$probs)              # Expected value of X  or E(X)
VX <- sum(PX$X^2*PX$probs) - EX^2     # variance of X  or V(X)

EY <- sum(PY$Y*PY$probs)              # Expected value of Y  or E(Y)
VY <- sum(PY$Y^2*PY$probs) - EY^2     # variance of Y or V(Y)

EXY <- sum(PXY$X*PXY$Y*PXY$probs)  
cov.xy <- EXY - EX * EY2              # covariance of X and Y
corr.XY <- cov.XY/(sqrt(VX*VY))       # correlation coefficient of X and y