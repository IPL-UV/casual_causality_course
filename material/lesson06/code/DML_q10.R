library("DoubleML")
library("mlr3")
library("mlr3learners")
library("ranger")
#library("pbapply")
library("paradox")

## make functions quiet
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#################### synthetic data ###################
# real Q10 
Q10 <- 1.5
## inputs used 
inputs <- c("SW_POT_sm", "SW_POT_sm_diff")
## read data from csv
data <- read.csv(file = "Synthetic4BookChap.csv")
years <- 2003:2007
## subset data
ix <- data$year %in% years
Dt <- data[ix, c(inputs, "TA")]
Dt$Y <- log(data[ix, "RECO_syn"])

##################### night NEE = RECO obs #############

# real Q10 
Q10 <- 1.5
## inputs used (we can try change that)
inputs <- c("SW_POT_sm", "SW_POT_sm_diff", 
             "SWC_1", "SWC_2", "VPD")
## read data from csv
data <- read.csv(file = "Synthetic4BookChap.csv")
years <- 2003:2007
## subset data
ix <- data$year %in% years & (data$SW_IN_POT == 0) & (data$NEE > 0) & (data$NEE_QC == 0)
Dt <- data[ix, c(inputs, "TA")]
Dt$Y <- log(data[ix, "NEE"])

##################### night NEE = RECO obs #############

# real Q10 
Q10 <- 1.5
## inputs used (we can try change that)
inputs <- c("SW_POT_sm", "SW_POT_sm_diff", 
            "SWC_1", "SWC_2", "VPD")
## read data from csv
data <- read.csv(file = "Synthetic4BookChap.csv")
years <- 2003:2007
## subset data
ix <- data$year %in% years 
Dt <- data[ix, c(inputs, "TA")]
Dt$Y <- log(data[ix, "RECO_DT"])

########################################################################

Dt_double <- double_ml_data_from_data_frame(Dt, y_col = 'Y', d_cols = "TA")

ml_g = lrn("regr.ranger", num.trees = 100, max.depth = 3)
ml_m = ml_g$clone()

model <- DoubleMLPLR$new(
  Dt_double,
  ml_g,
  ml_m,
  n_folds = 10,
  n_rep = 1,
  score = "partialling out",
  dml_procedure = "dml2",
  draw_sample_splitting = TRUE,
  apply_cross_fitting = TRUE, 
)

model$tune(
  list("ml_m" = ParamSet$new(list(ParamInt$new("max.depth", lower = 1, upper = 10))),
       "ml_l" = ParamSet$new(list(ParamInt$new("max.depth", lower = 1, upper = 10)))),
  tune_settings = list(n_folds_tune = 5, rsmp_tune = mlr3::rsmp("cv", folds = 5), 
                       measure = NULL, 
                       terminator = mlr3tuning::trm("evals", n_evals = 20), 
                       algorithm = mlr3tuning::tnr("grid_search"), resolution = 5),
  tune_on_folds = FALSE
)

model$fit(store_predictions = TRUE, store_models = TRUE)

# estimated coef
model$coef

# estimated Q10
exp(model$coef * 10)


########## no DML option 1
f <- function(q10, data, tref=15, tnorm=10, ...){
  tt <- (data[["TA"]] - tref) / tnorm 
  res <- data$Y - log(q10)*tt
  x <- data[, !(colnames(data) %in% c("Y", "TA"))]
  mod <-ranger::ranger(x = x, y = res, max.depth = 3, num.trees = 100, ...)
  sum((mod$predictions - res)^2)
}
res <- optimise(f, c(0,5), data = Dt)
res$min

########## no DML option 2

x <- Dt[, !(colnames(Dt) %in% c("Y", "TA"))]
y <- Dt$Y
mod <-ranger::ranger(x = x, y = y, num.trees = 100, max.depth = 3)
resid <- y - mod$predictions 
tt <- (Dt[["TA"]] - 15) / 10
#q10naive <- exp(coefficients(lm(resid ~ Dt[["TA"]] ))[2] * 10)
q10naive <- exp(coefficients(lm(resid ~ tt ))[2])
q10naive
