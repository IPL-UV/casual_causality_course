library("ncdf4")
library("pracma")
library("raster")
library("sf")
library(ggplot2)
library(ggspatial)

### replicate the example 1 from:
### Kretschmer, M., Adams, S. V., Arribas, A., Prudden, R., Robinson, 
### N., Saggioro, E., & Shepherd, T. G. (2021). 
### Quantifying Causal Pathways of Teleconnections, 
### Bulletin of the American Meteorological Society, 102(12), E2247-E2263. 
### Retrieved Jul 14, 2022, from 
### https://journals.ametsoc.org/view/journals/bams/102/12/BAMS-D-20-0117.1.xml

### data obtained from https://github.com/informatics-lab/causality

### load the data
years <- 1950:2019
precip <- stack('data/precip_jja.nc')
nao <- ncvar_get(nc_open('data/nao_jja.nc'))

cl <-
  sf::read_sf("data/gshhg-shp-2.3.7/GSHHS_shp/c/GSHHS_c_L1.dbf")

### define regions

## med
med <- extent(c(10, 30, 36, 41))
med_precip <- crop(precip, med)


ggplot() + layer_spatial(med_precip[[1]]) + 
  scale_fill_continuous(type = "viridis") + 
  annotation_spatial(cl, color = "black", fill = NA) + 
  ggtitle("Precipitation (med)")

## denmark 

dk <- extent(c(2, 15, 50, 60))
dk_precip <- crop(precip, dk)

ggplot() + layer_spatial(dk_precip[[1]]) + 
  scale_fill_continuous(type = "viridis") + 
  annotation_spatial(cl, color = "black", fill = NA) + 
  ggtitle("Precipitation (dk)")

### compute averages
med_avg <- sapply(1:nlayers(med_precip), function(ii) mean(as.array(med_precip[[ii]])))
dk_avg <- sapply(1:nlayers(dk_precip), function(ii) mean(as.array(dk_precip[[ii]])))



### plot data
plot(x = years, y = nao, type = "l", main = "nao")
plot(x = years, y = med_avg, type = "l", main = "med precip")
plot(x =  years, y = dk_avg, type = "l", main = "dk precip")



##stnd data
data <- data.frame(scale(data.frame(NAO = nao, MED = med_avg, DK = dk_avg)))

### correlations

cor(med_avg, dk_avg)

cor.test(med_avg, dk_avg)
cor.test(data$MED, data$DK)

summary(lm(MED ~ DK, data))

### conditionign on NAO

summary(lm(MED ~ DK + NAO, data))
coefficients(lm(MED ~ DK + NAO, data))

### NAO --> DK effect 
mod1 <- lm(DK ~ NAO, data)
summary(mod1)
coef(mod1)[2]


## NAO --> MED 
mod2 <- lm(MED ~ NAO, data)
summary(mod2)
coef(mod2)[2]


## path rule 
coef(mod1)[2] * coef(mod2)[2]


