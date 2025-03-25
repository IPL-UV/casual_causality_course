library(stagedtrees)

tree <- list(risk = c("high", "low"), treatment = c("treated", "untreated"),
             weather = c("dry", "normal", "wet"), outcome = c("burned", "not-burned"))
model <-  sevt(tree, full = TRUE)
stages(model)[[c(risk = "low")]] <- "low-risk"
stages(model)[[c(risk = "high")]] <- "high-risk"


model <- random_sevt(model, q = 0)

model$prob$treatment$`high-risk`[1] <- 1
model$prob$treatment$`high-risk`[1] <- 0


