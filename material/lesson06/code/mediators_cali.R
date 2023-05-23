library("ncdf4")

### replicate the example 2 from:
### Kretschmer, M., Adams, S. V., Arribas, A., Prudden, R., Robinson, 
### N., Saggioro, E., & Shepherd, T. G. (2021). 
### Quantifying Causal Pathways of Teleconnections, 
### Bulletin of the American Meteorological Society, 102(12), E2247-E2263. 
### Retrieved Jul 14, 2022, from 
### https://journals.ametsoc.org/view/journals/bams/102/12/BAMS-D-20-0117.1.xml

### data obtained from https://github.com/informatics-lab/causality


### load the data
years <- 1950:2019

jet <- ncvar_get(nc_open('data/cali_jet_mean.nc'))
