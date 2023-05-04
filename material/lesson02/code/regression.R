
x <- seq(from = -3, to = 3, by =0.001)

f <- function(x)  3*sin(10*x/pi) + 0.5*x^2 - 0.1*x^3

eps <- rnorm(x)

y <- f(x) + eps 


d <- data.frame(X = x, Y = y, f = f(x))

library(ggplot2)

PLT <- ggplot(d)  + 
  geom_point(aes(x = X, y = Y)) + geom_line(aes(x = X, y= f), color = "red") +
  theme_bw() 

PLT

ggsave("../images/regression.pdf", PLT, width = 6, height = 6, units = "cm")

