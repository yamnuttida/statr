weight <- read.csv("C:/Users/user/Documents/R/weightpopulation.csv",header = T)
pop1.w <- weight$pop1
n <- 20
set.seed(405)
rsample1 <- sample(pop1.w,n,F)
xbar <- mean(rsample1);s <- sd(rsample1)
LCI <- xbar - qt(0.975,19)*s/sqrt(n)
UCI <- xbar + qt(0.975,19)*s/sqrt(n)
t.test (rsample1,altervative="two.sided",mu=160)
#-----------------------------------------------------
n <- 20
set.seed(405)
xbar <- c() ; s <- c()
LCI <- c() ; UCI <- c()
for (i in 1:1000) {
  rsample2 <- sample(pop1.w,n,F)
  xbar <- mean(rsample2);s <- sd(rsample2)
  LCI <- xbar - qt(0.975,19)*s/sqrt(n)
  UCI <- xbar + qt(0.975,19)*s/sqrt(n)
}
t.test (rsample2,altervative="two.sided",mu=160)
