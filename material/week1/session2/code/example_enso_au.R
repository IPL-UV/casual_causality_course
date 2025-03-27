library("ncdf4")
library("pracma")
library("stagedtrees")

### replicate the example from:
### Kretschmer, M., Adams, S. V., Arribas, A., Prudden, R., Robinson, 
### N., Saggioro, E., & Shepherd, T. G. (2021). 
### Quantifying Causal Pathways of Teleconnections, 
### Bulletin of the American Meteorological Society, 102(12), E2247-E2263. 
### Retrieved Jul 14, 2022, from 
### https://journals.ametsoc.org/view/journals/bams/102/12/BAMS-D-20-0117.1.xml


### data obtained from https://github.com/informatics-lab/causality
### download data enso_son.nc precip_au_son.nc iod_son.nc and place them in the data folder

### load the data
enso <- ncvar_get(nc_open('data/enso_son.nc'))
au <- ncvar_get(nc_open('data/precip_au_son.nc'), varid = 'precip')
iod <- ncvar_get(nc_open('data/iod_son.nc'))

### plot data
plot(enso, type = "l")
plot(au, type = "l")
plot(iod, type = "l")


data <- data.frame(enso = enso, iod = iod, au = au)

## standardize
data <- scale(data)

## detrend 
data <- data.frame(detrend(data))

### make categorical data
data.categ = data.frame(
  au = cut(data$au, breaks = quantile(data$au, c(0,0.5,1)), 
           labels = c('low', 'high'), include.lowest = TRUE),
  enso = cut(data$enso, breaks = quantile(data$enso, seq(0,1,length.out = 4)), 
             labels = c('Nina', 'neut', 'Nino'), include.lowest = TRUE),
  iod = cut(data$iod, breaks = quantile(data$iod, seq(0,1,length.out = 4)), 
            labels = c('neg', 'zero', 'pos'), include.lowest = TRUE))

### print contingency table
table(data.categ)
### ok! it agrees with the table obtained by Kretschmer et al.
### https://github.com/informatics-lab/causality/blob/master/causality_paper/notebooks/example5_nonlinear.ipynb


### estimate first model with causal order enso -> iod -> au
mod1 <- full(data.categ, order = c('enso', 'iod', 'au'), lambda = 1e-6) |> 
  stages_bhc(score = function(x) -AIC(x))

### plot staged tree and conditional probabilitites of au
par(mfrow=c(1,2))
plot(mod1)
barplot(mod1, var = "au")

plot(stndnaming(mod1, TRUE), col = "stages")


write_tikz(mod1, file = "enso.tex", node_label = \(node) "",
           edge_label_options = \(edge){
             if (edge$label == "low") return(c("swap", "sloped"))
             return("sloped")
           },scale = 10)



boot <- pbreplicate(100, {
  data <- data.categ[sample(nrow(data.categ), replace = TRUE),]
  model <- full(data, order = c('enso', 'iod', 'au'), lambda = 1e-6) |> 
    stages_bhc(score = function(x) -AIC(x))
  

  res <- ce_randomized(model, outcome = "au", treatment = "iod")
  c(rr = res[1,2] - res[3,2])
}, cl = 5)
