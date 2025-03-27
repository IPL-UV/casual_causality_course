remotes::install_github("gherardovarando/stagedtrees", ref = "potout")
library(stagedtrees)

tree <- list(weather = c("dry", "normal", "wet"), 
             risk = c("high", "low"),
             treatment = c("treated", "untreated"),
             outcome = c("burned", "not-burned"))
model <-  sevt(tree, full = TRUE)

stages(model)["treatment", risk = "low"] <- "low-risk"
stages(model)["treatment", risk = "high"] <- "high-risk"

stages(model)["outcome", risk = "high", treatment = "treated"] <- "high-treated"
stages(model)["outcome", risk = "high", treatment = "treated", weather = "dry"] <- "high-treated-dry"

stages(model)["outcome", risk = "high", treatment = "untreated"] <- "high-untreated"
stages(model)["outcome", risk = "high", treatment = "untreated", weather = "dry"] <- "high-untreated-dry"
stages(model)["outcome", risk = "high", treatment = "untreated", weather = "wet"] <- "high-untreated-wet"

stages(model)["outcome", risk = "low", weather = "dry"] <- "low-dry"
stages(model)["outcome", risk = "low", weather = "wet"] <- "low-wet"
stages(model)["outcome", risk = "low", weather = "normal"] <- "low-normal"


set.seed(2025)
model <- random_sevt(model, q = 0)


model$prob$weather$`1`["dry"] <- 0.2
model$prob$weather$`1`["normal"] <- 0.5
model$prob$weather$`1`["wet"] <- 0.3

model$prob$risk$`1`["high"] <- 0.4
model$prob$risk$`1`["low"] <- 0.6


model$prob$risk$`2`["high"] <- 0.3
model$prob$risk$`2`["low"] <- 0.7


model$prob$risk$`3`["high"] <- 0.1
model$prob$risk$`3`["low"] <- 0.9


model$prob$treatment$`high-risk`["treated"]  <- 0.8
model$prob$treatment$`high-risk`["untreated"]  <- 0.2

model$prob$treatment$`low-risk`["treated"]  <- 0.4
model$prob$treatment$`low-risk`["untreated"]  <- 0.6


model$prob$outcome$`low-wet`["burned"] <- 0.005
model$prob$outcome$`low-wet`["not-burned"] <- 0.995

model$prob$outcome$`low-normal`["burned"] <- 0.01
model$prob$outcome$`low-normal`["not-burned"] <- 0.99

model$prob$outcome$`low-high`["burned"] <- 0.05
model$prob$outcome$`low-high`["not-burned"] <- 0.95

model$prob$outcome$`high-treated-dry`["burned"] <- 0.2
model$prob$outcome$`high-treated-dry`["not-burned"] <- 0.8

model$prob$outcome$`high-untreated-dry`["burned"] <- 0.3
model$prob$outcome$`high-untreated-dry`["not-burned"] <- 0.7

model$prob$outcome$`high-treated`["burned"] <- 0.1
model$prob$outcome$`high-treated`["not-burned"] <- 0.9

model$prob$outcome$`high-untreated`["burned"] <- 0.2
model$prob$outcome$`high-untreated`["not-burned"] <- 0.8

model$prob$outcome$`high-untreated-wet`["burned"] <- 0.1
model$prob$outcome$`high-untreated-wet`["not-burned"] <- 0.9

plot(model)
data <- sample_from(model, size = 10000, seed = 2025)

dir.create("data", showWarnings = FALSE)
write.csv(data, file = "data/fire_risk_randomized.csv")


potential_outcomes(model, outcome =  "outcome", 
                          treatment = "treatment")

### fit a stagedtrees model to data
fitted <- full(data, order = c("risk", "treatment", "outcome")) |> stages_bhc() 
potential_outcomes(fitted, "outcome", "treatment")


