
### we sample from a BN/DAG 
### with structure   A -> B  <- C 
###                  |          |
###                  v          v
###                  D  ----->  E

### a possible topological order is A, C, B, D, E


### a possible distribution over the variables is obtained by

n <- 100
sample <- matrix(nrow = n, ncol = 5, 
                 dimnames = list(NULL, c("A", "B", "C", "D", "E")))

for (i in 1:n){ ##for each sample
  sample[i, "A"] <- rnorm(1, sd = 0.1)
  sample[i, "C"] <- rexp(1, rate = 3)
  sample[i, "B"] <- rnorm(1, mean = sample[i, "A"] + sample[i, "C"], sd = 0.3)
  sample[i, "D"] <- rexp(1, rate = abs(0.2*sample[i, "A"]))
  sample[i, "E"] <- sample(c(rnorm(1, mean = sample[i, "D"]), 
                             rexp(1, rate = sample[i, "C"])), size  = 1)
  
}
sample <- as.data.frame(sample)
plot(sample)

plot(sample$E, sample$D)
