library(bnlearn)
library(pcalg)
#library(gclm)

filenames <- c(
  "cd3cd28",
  "cd3cd28icam2",
  "cd3cd28+aktinhib",
  "cd3cd28+g0076",
  "cd3cd28+psitect",
  "cd3cd28+u0126",
  "cd3cd28+ly",
  "pma",
  "b2camp"
)
extension <- ".csv"
basepath <- "data/Sachs/Data Files/"
completepaths <- paste0(basepath, 1:9, ". ", filenames, extension)

########################## LOADING DATA 

conditions <- 1:9
datalist <- lapply(
  conditions,
  FUN = function(cond) {
    fn <- completepaths[cond]
    tmp <- read.csv(fn)
    colnames(tmp) <- tolower(colnames(tmp))
    return(tmp)
  }
)

p <- ncol(datalist[[1]])

all <- datalist[[1]]
if (length(datalist) > 1){
for (cond in 2:length(datalist)){
  all <- rbind(all, datalist[[cond]])
}
}

message("data loaded correctly ", dim(all))

#############################################

plot(all[,1:5])


#### pc.stable from bnlearn 

pc1 <- pc.stable(all)
graphviz.plot(pc1, main = "bnlearn::pc.stable result")
#dag_pc1 <- bnlearn::as.graphNEL(dag_pc1)

#### pc from pcalg
pc2 <- pc(suffStat = list(C = cor(all), n = nrow(all)), 
              indepTest = gaussCItest,
              alpha = 0.01, labels = colnames(all), verbose = FALSE)
plot(pc2, main = "pcalg::pc result")
#dag_pc2 <- dag_pc2@graph

#### ges from pcalg
score <- new("GaussL0penObsScore",  all)
ges.fit <- ges(score)
plot(ges.fit$essgraph, main = "pcalg::ges result")



