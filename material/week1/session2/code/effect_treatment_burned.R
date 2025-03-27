
## conditional (on risk) randomized study 
data <- read.csv("data/fire_risk_randomized.csv")[,-1]


tt <- table(data$risk, data$treatment) 
tt / rowSums(tt)


to <- table(data$treatment, data$outcome)
to / rowSums(to)


### estimate the causal effect

data_low <- data[data$risk == "low", ]

tlow <- table(data_low$treatment, data_low$outcome)
plow <- tlow / rowSums(tlow)
chisq.test(tlow)

data_high <- data[data$risk == "high", ]
thigh <- table(data_high$treatment, data_high$outcome)
phigh <- thigh / rowSums(thigh)
chisq.test(thigh)

phigh[1,1] - phigh[2,1]

phigh[1,1]/ phigh[2,1]

(phigh[1,1]/phigh[1,2]) / (phigh[2,1]/phigh[2,2])

############## 

prisk <- table(data$risk)
prisk <- prisk / sum(prisk)

P <- plow * prisk["low"] + phigh * prisk["high"]

P[1,1] - P[2,1]

P[1,1] / P[2,1]












