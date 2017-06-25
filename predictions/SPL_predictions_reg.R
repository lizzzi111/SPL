source(file = "./predictions/SPL_predictions.R")
source(file = "./predictions/metrics_for_evaluation.R")

model_setup = list("NN" = list(tuneGrid = expand.grid(decay = 10^seq(-4, 0, 0.5), size = seq(3, 13, 2)), maxit = 100, method = "nnet"), 
                   "RF" = list(tuneGrid = expand.grid(mtry = c(5, 8, 10, 12, 15, 20)), ntree = 100, method = "rf"), 
                   "rpart" = list(tuneGrid = expand.grid(cp = seq(0.001, 0.1, 0.01)), method = "rpart"), 
                   "SVMradial" = list(tuneGrid = expand.grid(sigma = 2^seq(-12, -1), C = 2^seq(-12, 12)), method = "svmRadial"), 
                   "SVMlinear" = list(tuneGrid = expand.grid(C = 2^seq(-12, 12)), method = "svmLinear"))

modellist = c("SVMradial", "NN", "RF", "rpart")

data_reg_mat = readRDS("./data/student-mat_reg.rds")

# A, all the independent variables
mat_reg_fitA = modelsLib(data = data_reg_mat, 
                                 formula = G3~., 
                                 modellist = modellist,
                                 metric = "RMSE",
                                 model_setup = model_setup )

min(mat_reg_fitA$SVMradial$results$RMSE)
min(mat_reg_fitA$NN$results$RMSE)
min(mat_reg_fitA$RF$results$RMSE)
min(mat_reg_fitA$rpart$results$RMSE)

saveRDS(mat_reg_fitA, "./predictions/models/mat_reg_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
rmse(data_reg_mat$G2,data_reg_mat$G3)

# B: A without G2
mat_reg_fitB = modelsLib(data = data_reg_mat[, !(colnames(data_reg_mat)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "RMSE",
                                  model_setup = model_setup )

min(mat_reg_fitB$SVMradial$results$RMSE)
min(mat_reg_fitB$NN$results$RMSE)
min(mat_reg_fitB$RF$results$RMSE)
min(mat_reg_fitB$rpart$results$RMSE)

saveRDS(mat_reg_fitB, "./predictions/models/mat_reg_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
rmse(data_reg_mat$G1,data_reg_mat$G3)

# C: A without G2 & G1
mat_reg_fitC = modelsLib(data = data_reg_mat[, !(colnames(data_reg_mat)%in%c("G2","G1"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "RMSE",
                                  model_setup = model_setup )

min(mat_reg_fitC$SVMradial$results$RMSE)
min(mat_reg_fitC$NN$results$RMSE)
min(mat_reg_fitC$RF$results$RMSE)
min(mat_reg_fitC$rpart$results$RMSE)

saveRDS(mat_reg_fitC, "./predictions/models/mat_reg_fitC.RDS")
# for the Naive Bayes in the paper the average output value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
rmse(rep(mean(data_reg_mat$G3), 395),data_reg_mat$G3)


# PORTUGUESE CLASS
data_reg_por = readRDS("./data/student-por_reg.rds")

# A, all the independent variables
por_reg_fitA = modelsLib(data = data_reg_por, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "RMSE",
                                  model_setup = model_setup )

min(por_reg_fitA$SVMradial$results$RMSE)
min(por_reg_fitA$NN$results$RMSE)
min(por_reg_fitA$RF$results$RMSE)
min(por_reg_fitA$rpart$results$RMSE)

saveRDS(por_reg_fitA, "./predictions/models/por_reg_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
rmse(data_reg_por$G2,data_reg_por$G3)

# B: A without G2
por_reg_fitB = modelsLib(data = data_reg_por[, !(colnames(data_reg_por)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "RMSE",
                                  model_setup = model_setup )

min(por_reg_fitB$SVMradial$results$RMSE)
min(por_reg_fitB$NN$results$RMSE)
min(por_reg_fitB$RF$results$RMSE)
min(por_reg_fitB$rpart$results$RMSE)

saveRDS(por_reg_fitB, "./predictions/models/por_reg_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
rmse(data_reg_por$G1,data_reg_por$G3)

# C: A without G2 & G1
por_reg_fitC = modelsLib(data = data_reg_por[, !(colnames(data_reg_por)%in%c("G2","G1"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "RMSE",
                                  model_setup = model_setup )

min(por_reg_fitC$SVMradial$results$RMSE)
min(por_reg_fitC$NN$results$RMSE)
min(por_reg_fitC$RF$results$RMSE)
min(por_reg_fitC$rpart$results$RMSE)

saveRDS(por_reg_fitC, "./predictions/models/por_reg_fitC.RDS")
# for the Naive Bayes in the paper the average output value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
rmse(rep(mean(data_reg_por$G3), 649),data_reg_por$G3)

