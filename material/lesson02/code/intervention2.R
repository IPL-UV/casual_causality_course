
####                A
####               / \
####              V   V
####             B     C

n <- 1000
A <- rnorm(n)
B <- 1 + 0.5*A + rnorm(n, sd = 0.1)
C <- -2 + 0.7*A + rnorm(n, sd = 0.2)


plot(B,C)
cor(B,C)

####   intervene do(B = rnorm(n))
A1 <- rnorm(n)
B1 <- rnorm(n)
C1 <- -2 + 0.7*A + rnorm(n, sd = 0.2)

plot(B1,C1)
cor(B1, C1)
