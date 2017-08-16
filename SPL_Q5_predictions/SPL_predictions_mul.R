source(file = "./SPL_Q5_predictions/SPL_predictions.R")
source(file = "./SPL_Q5_predictions/metrics_for_evaluation.R")
source(file = "./SPL_Q5_predictions/model_setup.R")

data_mul_mat = readRDS("./data/student-mat_multiclass.rds")

# A, all the independent variables
mat_mul_fitA = modelsLib(data = data_mul_mat, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_mul_fitA$SVM$results$Accuracy)
max(mat_mul_fitA$NN$results$Accuracy, na.rm = TRUE)
max(mat_mul_fitA$RF$results$Accuracy)
max(mat_mul_fitA$DT$results$Accuracy)

saveRDS(mat_mul_fitA, "./SPL_Q5_predictions/models/mat_mul_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
pcc(data_mul_mat$G2,data_mul_mat$G3)

# B: A without G2
mat_mul_fitB = modelsLib(data = data_mul_mat[, !(colnames(data_mul_mat)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_mul_fitB$SVM$results$Accuracy)
max(mat_mul_fitB$NN$results$Accuracy, na.rm = TRUE)
max(mat_mul_fitB$RF$results$Accuracy)
max(mat_mul_fitB$DT$results$Accuracy)

saveRDS(mat_mul_fitB, "./SPL_Q5_predictions/models/mat_mul_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
pcc(data_mul_mat$G1,data_mul_mat$G3)

# C: A without G2 & G1
mat_mul_fitC = modelsLib(data = data_mul_mat[, !(colnames(data_mul_mat)%in%c("G1","G2"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_mul_fitC$SVM$results$Accuracy)
max(mat_mul_fitC$NN$results$Accuracy)
max(mat_mul_fitC$RF$results$Accuracy)
max(mat_mul_fitC$DT$results$Accuracy)

saveRDS(mat_mul_fitC, "./SPL_Q5_predictions/models/mat_mul_fitC.RDS")
# for the Naive Bayes in the paper the most frequent value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
m=names(which.max(summary(data_mul_mat$G3)))
pcc(factor(rep(m, 395), levels = levels(data_mul_mat$G3)),data_mul_mat$G3)


# PORTUGUESE CLASS
data_mul_por = readRDS("./data/student-por_multiclass.rds")

# A, all the independent variables
por_mul_fitA = modelsLib(data = data_mul_por, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_mul_fitA$SVM$results$Accuracy)
max(por_mul_fitA$NN$results$Accuracy, na.rm = TRUE)
max(por_mul_fitA$RF$results$Accuracy)
max(por_mul_fitA$DT$results$Accuracy)

saveRDS(por_mul_fitA, "./SPL_Q5_predictions/models/por_mul_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
pcc(data_mul_por$G2,data_mul_por$G3)

# B: A without G2
por_mul_fitB = modelsLib(data = data_mul_por[, !(colnames(data_mul_por)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_mul_fitB$SVM$results$Accuracy)
max(por_mul_fitB$NN$results$Accuracy, na.rm = TRUE)
max(por_mul_fitB$RF$results$Accuracy)
max(por_mul_fitB$DT$results$Accuracy)

saveRDS(por_mul_fitB, "./SPL_Q5_predictions/models/por_mul_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
pcc(data_mul_por$G1,data_mul_por$G3)

# C: A without G2 & G1
por_mul_fitC = modelsLib(data = data_mul_por[, !(colnames(data_mul_por)%in%c("G2","G1"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_mul_fitC$SVM$results$Accuracy)
max(por_mul_fitC$NN$results$Accuracy)
max(por_mul_fitC$RF$results$Accuracy)
max(por_mul_fitC$DT$results$Accuracy)

saveRDS(por_mul_fitC, "./SPL_Q5_predictions/models/por_mul_fitC.RDS")
# for the Naive Bayes in the paper the average output value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
p=names(which.max(summary(data_mul_por$G3)))
pcc(factor(rep(p, 649), levels = levels(data_mul_por$G3)),data_mul_por$G3)
