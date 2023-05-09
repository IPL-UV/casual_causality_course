############################
##   generate data from 
##     X1       X2
##      \       /
##       v     v 
##          X3
##
############################

B <- matrix(nrow = 3, ncol = 3, 
            data = c(0, 0, 0, 0, 0, 0, -1, 2, 0))

n <- 100
p <- 3

X <- matrix(nrow = n, ncol = p, rnorm(n*p))
for (j in 1:p){
  for (i in 1:n){
    X[i, j] = X[i, j] + sum(X[i,]*B[,j]) 
  }
}
X <- as.data.frame(X)

plot(X)

library("pcalg")
library("bnlearn")

#### bnlearn pc.stable
res_pcstable <- pc.stable(X, alpha = 0.01, debug = TRUE)
graphviz.plot(res_pcstable, main = "result from bnlearn::pcstable")
#### pcalg::pc
pc.fit <- pc(suffStat = list(C = cor(X), n = nrow(X)), 
                    indepTest = gaussCItest,
                    alpha = 0.01, labels = colnames(X), verbose = TRUE)

plot(pc.fit, main = "result from pcalg::pc")

#### bnlearn::tabu 
res_tabu <- tabu(X)
plot(res_tabu, main = "result from bnlearn::tabu")

#### pcalg::ges 
score <- new("GaussL0penObsScore",  X)
ges.fit <- ges(score)
plot(ges.fit$essgraph, main = "result from pcalg::ges")


####################### example without faithfulness ###################
noisy_xor <- function (x, eps = 0) {
  return(sign(prod(x) + runif(n = 1, min = -eps, max = eps)))
}

N <- 1000
n <- 2
DD <- data.frame(observation = 1:N)
for (i in 1:n) {
  DD[[paste("X", i, sep = "")]] <- sample(
    c(-1, +1),
    #prob = runif(2),
    prob = c(0.5, 0.5),
    size = N,
    replace = TRUE
  )
}
DD$C <- apply(
  DD,
  MARGIN = 1,
  FUN = function(x) {
    return(noisy_xor(x = x[-1], eps = 0))
  }
)
DD <- DD[, -1]
for (i in 1:(n + 1)) {
  DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
}

### bnlearn pc.stable
plot(pc.stable(DD, debug = TRUE, alpha = 0.05))

### bnlearn hc
plot(hc(DD))

### pcalg 
DD1 <- (data.frame(t(apply(DD,1, function(x) as.numeric(x)))) + 1) / 2

pc.B <- pc(suffStat = list(dm = DD1, adaptDF = FALSE), maj.rule = TRUE,
           indepTest = binCItest, alpha = 0.05, labels = colnames(DD), verbose = TRUE)
pc.B
plot(pc.B, main = "pcalg pc")

