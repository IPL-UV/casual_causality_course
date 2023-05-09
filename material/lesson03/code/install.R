if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Rgraphviz")
BiocManager::install("RBGL")

install.packages("bnlearn")
install.packages("pcalg")

install.packages("gclm")

install.packages("lmtest")
