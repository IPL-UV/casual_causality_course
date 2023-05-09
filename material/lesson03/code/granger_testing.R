library(lmtest)
## Which came first: the chicken or the egg?
data(ChickEgg)

plot(ChickEgg)


grangertest(egg ~ chicken, order = 3, data = ChickEgg)
grangertest(chicken ~ egg, order = 3, data = ChickEgg)
