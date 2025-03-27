
n <- 1000

## original SCM
SM <- 0.3 + rnorm(n, sd = 0.1)
TS <- 25 + rnorm(n, sd = 5)
TT <- (1 + sign(1.2 + 0.08 * SM - 0.05 * TS + rnorm(n, sd = 0.1))) / 2
Y <- 2.5 + 1.5 * SM - 0.07 * TS + 2 * TT + rnorm(n, sd = 1)
Y[Y < 0] <- 0


## intervened SCM do(TT = 1)
SM1 <- 0.3 + rnorm(n, sd = 0.1)
TS1 <- 25 + rnorm(n, sd = 5)
TT1 <- rep(1, n)
Y1 <- 2.5 + 1.5 * SM1 - 0.07 * TS1 + 2 * TT1 + rnorm(n, sd = 1)
Y1[Y1 < 0] <- 0

## do(Y = rexp(rate = 5))
SM2 <- 0.3 + rnorm(n, sd = 0.1)
TS2 <- 25 + rnorm(n, sd = 5)
TT2 <- (1 + sign(1.2 + 0.08 * SM2 - 0.05 * TS2 + rnorm(n, sd = 0.1))) / 2
Y2 <- rexp(n, rate = 5)


SM3 <- 0.3 + rnorm(n, sd = 0.1)
TS3 <- 25 + rnorm(n, sd = 5)
TT3 <- sample(c(0,1), size = n , replace = TRUE)
Y3 <- 2.5 + 1.5 * SM3 - 0.07 * TS3 + 2 * TT3 + rnorm(n, sd = 1)
Y3[Y3< 0] <- 0

plot(TT3, Y3)

library(ggplot2)
ggplot(data = data.frame(Y = Y, Y1 = Y1, Y2 = Y2)) + geom_density(aes(x = Y), col = "blue")+ 
  geom_density(aes(x = Y1), col = "red") + geom_density(aes(x = Y2), col = "green")


ggplot(data = data.frame(Y = SM, Y1 = SM1, Y2 = SM2)) + geom_density(aes(x = Y), col = "blue")+ 
  geom_density(aes(x = Y1), col = "red") + geom_density(aes(x = Y2), col = "green")


ggplot(data = data.frame(Y = TT, Y1 = TT1, Y2 = TT2)) + geom_histogram(aes(x = Y), col = "blue")+ 
  geom_histogram(aes(x = Y1), col = "red") + geom_histogram(aes(x = Y2), col = "green")


hist(TT)

hist(TT1)

hist(TT2)
