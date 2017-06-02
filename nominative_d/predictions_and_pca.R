### Predictive part

setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/data/")
data = readRDS("./student-mat_fact.rds")
data1 = readRDS("./student-por_fact.rds")

# check whether we can train our model on portuguese class set and validate on math class
# it is not very logical, we could also stratify our samples, but It#d be interesting for the future
# the assumption would be that the distr are comparable - we will check whether both distributions are
# significantly different from each other

funcs = list.files("../codes_in_process/")
source("../codes_in_process/smart_anova.R")
to_comp = rbind.data.frame(cbind(data1$G3, "por"), cbind(data$G3, "mat"))
names(to_comp) = c("grade", "class")
to_comp$grade = as.numeric(to_comp$grade)
smart_anova(to_comp$grade,to_comp$class)

library(caret)
library(randomForest)
fit_rf = randomForest(G3~., data1, importance=TRUE)
prog_rf = predict(fit_rf, newdata = data)
qqnorm(fit$residuals)


control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(G3~., data1, method="rf", trControl=control, tuneLength=5)
# summarize the model
print(model)
plot(model)

varImpPlot(fit_rf)
library(Metrics)
mae(data$G3,prog_rf)
mae(data$G3, prog)
