library("lmtest")
source("functions.R")

Y <- readRDS("data/enso_Y.rds")
X <- readRDS("data/ndvi_x.rds")

dim(Y)
dim(X)

maxlag <- 6
rank <- 6


PCA <- prcomp(X , scale. = FALSE, center = TRUE, rank = rank)

VM <- list(rotation = varimax(PCA$rotation)$loadings)
VM$x <- scale(X, scale = FALSE) %*% VM$rotation

GPCA <- gpca(PCA, Y, maxlag = maxlag, scale. = FALSE, center = FALSE, 
             intercept = TRUE)


ALLMTHDS <- list(
  "PCA" = PCA,
  "PCA-VM" = VM,
  "GPCA" = GPCA
)


pvals <- data.frame(lapply(ALLMTHDS, function(MTH){
  apply(MTH$x[,1:min(rank,maxlag)], MARGIN = 2, function(xx) granger_test(Y, xx, PCA$x, 6)$`Pr(>F)`[2])
  #apply(MTH$x[,1:min(rank,maxlag)], MARGIN = 2, function(xx) lmtest::grangertest(Y, xx,maxlag)$`Pr(>F)`[2])
})) 
pvals


plot(scale(PCA$x[,6]), type = "l")
lines(Y, col = "red")

plot(scale(VM$x[,6]), type = "l")
lines(Y, col = "red")

plot(scale(GPCA$x[,1]), type = "l")
lines(Y, col = "red")
