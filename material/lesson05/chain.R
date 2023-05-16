library("pcalg")
library("bnlearn")
library("Rgraphviz")

n <- 100
X <- rnorm(n)
Y <- 0.5*X + rnorm(n, sd = 0.1)
Z <- -0.2*Y + 1 + rnorm(n, sd = 0.1)

### true graph is X -> Y -> Z

data <- data.frame(X = X, Y = Y, Z = Z)
plot(data)

pc_res <- pc.stable(data)
plot(pc_res, main = "pc.stable")


tabu_res1 <- tabu(data)
plot(tabu_res1, main = "tabu")

## but
tabu_res2 <- tabu(data[,3:1])
plot(tabu_res2, main = "tabu")

plot(cpdag(tabu_res2))

### lingam 
res1 <- lingam(as.matrix(data), verbose = TRUE)# details on LINGAM
amat <- as(res1, "amat")
plot(graphAM(matrix(amat, nrow = 3, ncol = 3, dimnames = list(c("X", "Y", "Z"), 
                                                              c("X", "Y", "Z")), byrow = TRUE), 
             edgemode = "directed"))

###########################  lingam 

n <- 1000
X <- rexp(n)
Y <- 0.5*X + rnorm(n, sd = 0.1)^2
Z <- -0.2*Y + 1 + rnorm(n, sd = 0.1)^2

data <- data.frame(X = X, Y = Y, Z = Z)

X <- as.matrix(data)

res2 <- lingam(X, verbose = TRUE)# details on LINGAM
as(res2, "amat")
amat <- as(res2, "amat")
plot(graphAM(matrix(amat, nrow = 3, ncol = 3, dimnames = list(c("X", "Y", "Z"), 
                                                              c("X", "Y", "Z")), byrow = TRUE), 
             edgemode = "directed"))

plot(pc.stable(data))

