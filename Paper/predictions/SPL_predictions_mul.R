source(file = "./predictions/SPL_predictions.R")
source(file = "./metrics_for_evaluation.R")

model_setup = list("NN" = list(tuneGrid = expand.grid(decay = 10^seq(-4, 0, 0.5), size = seq(3, 13, 2)), maxit = 100, method = "nnet"), 
                   "RF" = list(tuneGrid = expand.grid(mtry = c(5, 8, 10, 12, 15, 20)), ntree = 100, method = "rf"), 
                   "rpart" = list(tuneGrid = expand.grid(cp = seq(0.001, 0.1, 0.01)), method = "rpart"), 
                   "SVMradial" = list(tuneGrid = expand.grid(sigma = 2^seq(-12, -1), C = 2^seq(-12, 12)), method = "svmRadial"), 
                   "SVMlinear" = list(tuneGrid = expand.grid(C = 2^seq(-12, 12)), method = "svmLinear"))

modellist = c("SVMradial", "NN", "RF", "rpart")

data_mul_mat = readRDS("./data/student-mat_multiclass.rds")

# A, all the independent variables
mat_mul_fitA = createModelLibrary(data = data_mul_mat, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_mul_fitA$SVMradial$results$Accuracy)
max(mat_mul_fitA$NN$results$Accuracy, na.rm = TRUE)
max(mat_mul_fitA$RF$results$Accuracy)
max(mat_mul_fitA$rpart$results$Accuracy)

saveRDS(mat_mul_fitA, "./predictions/models/mat_mul_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
pcc(data_mul_mat$G2,data_mul_mat$G3)

# B: A without G2
mat_mul_fitB = createModelLibrary(data = data_mul_mat[, !(colnames(data_mul_mat)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_mul_fitB$SVMradial$results$Accuracy)
max(mat_mul_fitB$NN$results$Accuracy, na.rm = TRUE)
max(mat_mul_fitB$RF$results$Accuracy)
max(mat_mul_fitB$rpart$results$Accuracy)

saveRDS(mat_mul_fitB, "./predictions/models/mat_mul_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
pcc(data_mul_mat$G1,data_mul_mat$G3)

# C: A without G2 & G1
mat_mul_fitC = createModelLibrary(data = data_mul_mat[, !(colnames(data_mul_mat)%in%c("G1","G2"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_mul_fitC$SVMradial$results$Accuracy)
max(mat_mul_fitC$NN$results$Accuracy)
max(mat_mul_fitC$RF$results$Accuracy)
max(mat_mul_fitC$rpart$results$Accuracy)

saveRDS(mat_mul_fitC, "./predictions/models/mat_mul_fitC.RDS")
# for the Naive Bayes in the paper the most frequent value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
m=names(which.max(summary(data_mul_mat$G3)))
pcc(factor(rep(m, 395), levels = levels(data_mul_mat$G3)),data_mul_mat$G3)


# PORTUGUESE CLASS
data_mul_por = readRDS("./data/student-por_multiclass.rds")

# A, all the independent variables
por_mul_fitA = createModelLibrary(data = data_mul_por, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_mul_fitA$SVMradial$results$Accuracy)
max(por_mul_fitA$NN$results$Accuracy, na.rm = TRUE)
max(por_mul_fitA$RF$results$Accuracy)
max(por_mul_fitA$rpart$results$Accuracy)

saveRDS(por_mul_fitA, "./predictions/models/por_mul_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
pcc(data_mul_por$G2,data_mul_por$G3)

# B: A without G2
por_mul_fitB = createModelLibrary(data = data_mul_por[, !(colnames(data_mul_por)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_mul_fitB$SVMradial$results$Accuracy)
max(por_mul_fitB$NN$results$Accuracy, na.rm = TRUE)
max(por_mul_fitB$RF$results$Accuracy)
max(por_mul_fitB$rpart$results$Accuracy)

saveRDS(por_mul_fitB, "./predictions/models/por_mul_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
pcc(data_mul_por$G1,data_mul_por$G3)

# C: A without G2 & G1
por_mul_fitC = createModelLibrary(data = data_mul_por[, !(colnames(data_mul_por)%in%c("G2","G1"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_mul_fitC$SVMradial$results$Accuracy)
max(por_mul_fitC$NN$results$Accuracy)
max(por_mul_fitC$RF$results$Accuracy)
max(por_mul_fitC$rpart$results$Accuracy)

saveRDS(por_mul_fitC, "./predictions/models/por_mul_fitC.RDS")
# for the Naive Bayes in the paper the average output value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
p=names(which.max(summary(data_mul_por$G3)))
pcc(factor(rep(p, 649), levels = levels(data_mul_por$G3)),data_mul_por$G3)
