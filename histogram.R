weight <- read.csv("C:/Users/user/Documents/R/WeightPopulation.csv",header = T)
head(weight)

#------------------------POP1----------------------#

pop1.w <- weight$pop1
mean(pop1.w); sd(pop1.w)
hist(pop1.w, freq = F, col ="pink")

rsample1 <- sample(pop1.w,47,F)
mean(rsample1);sd(rsample1)
hist(rsample1, freq = F, col ="yellow")
rsample2 <- sample(pop1.w,47,F)
mean(rsample2);sd(rsample2)
hist(rsample2, freq = F, col ="green")

xbar <- c()
s <- c()
for (i in 1:8) {
  rsample <- sample(pop1.w,47,F)
  xbar[i] <- mean(rsample)
  s[i] <- sd(rsample)
}
#xbar
#s

set.seed(1)
xbar.1000 <- c()
s.1000 <- c()
for (i in 1:1000) {
  rsample <- sample(pop1.w,47,F)
  xbar.1000[i] <- mean(rsample)
  s.1000[i] <- sd(rsample)
}
#xbar.1000
#s.1000
mean(xbar.1000);sd(xbar.1000);var(xbar.1000)
hist(xbar.1000, freq = F, col ="Blue")

#xbar.1000>159.57
sum(xbar.1000>159.57)/1000
mean(xbar.1000>159.57)

#------------------------POP2----------------------#

pop2.w <- weight$pop2
mean(pop2.w) ; sd(pop2.w)
hist(pop2.w, freq = F, col ="brown")

set.seed(789)
xbar2.1000 <- c()
s2.1000 <- c()
for (i in 1:1000) {
  rsample <- sample(pop2.w,47,F)
  xbar2.1000[i] <- mean(rsample)
  s2.1000[i] <- sd(rsample)
}
#xbar2.1000
#s2.1000
mean(xbar2.1000);sd(xbar2.1000);var(xbar2.1000)
hist(xbar2.1000, freq = F, col ="orange")

sum(xbar2.1000>159.574)/1000
mean(xbar2.1000>159.574)

#------------------------POP3----------------------#

pop3.w <- weight$pop3
mean(pop3.w) ; sd(pop3.w)
hist(pop3.w, freq = F, col ="purple")

set.seed(902)
xbar3.1000 <- c()
s3.1000 <- c()
for (i in 1:1000) {
  rsample <- sample(pop3.w,47,F)
  xbar3.1000[i] <- mean(rsample)
  s3.1000[i] <- sd(rsample)
}
#xbar3.1000
#s3.1000
mean(xbar3.1000);sd(xbar3.1000);var(xbar3.1000)
hist(xbar3.1000, freq = F, col ="gold")

summary(xbar3.1000)

#----------------------POP3.1----------------------#

set.seed(902)
xbar3.1.1000 <- c()
s3.1.1000 <- c()
for (i in 1:1000) {
  rsample <- sample(pop3.w,188,F)
  xbar3.1.1000[i] <- mean(rsample)
  s3.1.1000[i] <- sd(rsample)
}
#xbar3.1.1000
#s3.1.1000
mean(xbar3.1.1000);sd(xbar3.1.1000);var(xbar3.1.1000)
hist(xbar3.1.1000, freq = F, col ="red")

summary(xbar3.1.1000)



