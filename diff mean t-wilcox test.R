pre <- c(70,76,92,68,76,80,76,64,65,82,76,78,74,84,72)
post <- c(68,70,60,72,78,72,58,72,70,64,62,80,75,75,79)
n <- 15
diff<- pre - post
dbar <- mean(diff) ; sd <- sd(diff)
LCI <- dbar - qt(0.975,n-1)*sd/sqrt(n)
UCI <- dbar + qt(0.975,n-1)*sd/sqrt(n)

t.crit <- qt(1-0.05/2,n-1)
t.stat <- (dbar)/(sd/sqrt(n))

t.test(pre,post,alternative = "two.sided",paired = TRUE)

#-------------------------------------------

wilcox.test(pre,post,alternative = "greater",paired = TRUE)
wilcox.test(pre,post,alternative = "two.sided",paired = TRUE)

eu <- c(37.8,42.7,46.9,39.7,40.7,42.7,46.1,45.0,47.5,50.0,48.8,51.2,46.5,48.2,39.7)
asia <- c(52.0,48.4,56.2,62.0,47.5,41.8,50.2,55.4,51.7,49.2,46.9,52.7,54.5,57.0,49.8)
wilcox.test(asia,eu,alternative = "greater",paired = FALSE)
t.test(asia,eu,alternative = "greater",paired = TRUE)

