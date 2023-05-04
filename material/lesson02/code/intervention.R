

## simple causal model 

n <- 1000
A <- rnorm(n)
B <- 5*A + rnorm(n, sd = 1)

plot(A, B)

### P(A | B) ??
summary(lm(A ~ B))
### P(A | B) = N(mean = 0.2 * B, sd = 0.2)


### P(A | do(B = 1))
### let's sample from the intervened model
A1 <- rep(NA, n)
B1 <- rep(NA, n)
for (i in 1:n){
  A1[i] <- rnorm(1)
  B1[i] <- 1
  }
plot(A, B)
hist(A)
