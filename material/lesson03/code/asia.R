library("bnlearn")

data("asia")

colnames(asia)

dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

plot(dag)
par(mfrow=c(1,2))
graphviz.plot(dag)
graphviz.plot(cpdag(dag))

#### pc-stable
pdag1 <- pc.stable(asia, debug = TRUE)
graphviz.plot(cpdag(dag), main = "true CPDAG")
graphviz.plot(pdag1, main = "PC-stable PDAG")

### tabu search
#dag2 <- tabu(asia[,sample(ncol(asia))])
dag2 <- tabu(asia)
graphviz.plot(dag2)

## mmhc
dag3 <- mmhc(asia)
graphviz.plot(dag3)


