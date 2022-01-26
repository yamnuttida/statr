# EX 8.12, 8.14

score <- c(75,90,85,110,115,95,132,34,82,104,88,124,110,46,98)
install.packages("BSDA")
library(BSDA)
SIGN.test(score,md=100,alternative = "two.sided")    # EX 8.12
wilcox.test(score,mu=100,alternative = "two.sided")  # EX 8.14


# EX 8.19, 8.21
leg1 <- c(142,140,144,144,142,146,149,150,142,148)
leg2 <- c(138,136,147,139,143,141,143,145,136,146)
t.test(leg1,leg2,mu=0,alternative = "two.sided",paired = TRUE)
SIGN.test(leg1,leg2,md=0,alternative = "two.sided")   # EX 8.19
wilcox.test(leg1,leg2,mu=0,alternative = "two.sided",paired = TRUE)  # EX 8.21