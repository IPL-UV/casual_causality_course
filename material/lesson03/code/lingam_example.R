library("pcalg")
library("bnlearn")

n <- 500
eps1 <- sign(rnorm(n)) * sqrt(abs(rnorm(n)))
eps2 <- runif(n) - 0.5
eps3 <- sign(rnorm(n)) * abs(rnorm(n))^(1/3)
eps4 <- rnorm(n)^2

x2 <-                eps2
x1 <-   0.9*x2     + eps1
x3 <-   0.8*x2     + eps3
x4 <- -x1  -0.9*x3 + eps4

X <- cbind(x1,x2,x3,x4)

trueDAG <- cbind(x1 = c(0,1,0,0),
                 x2 = c(0,0,0,0),
                 x3 = c(0,1,0,0),
                 x4 = c(1,0,1,0))


res1 <- lingam(X, verbose = TRUE)# details on LINGAM
trueDAG
as(res1, "amat")

plot(graphAM(trueDAG, edgemode = "directed"))

### pcstable 
res_pc <- pc.stable(data.frame(X))
plot(res_pc, main = "pc.stable result")
