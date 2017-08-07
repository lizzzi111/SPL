source(file = "./predictions/SPL_predictions.R")
source(file = "./predictions/metrics_for_evaluation.R")
source(file = "./predictions/model_setup.R")

data_bin_mat = readRDS("./data/student-mat_binary.rds")

# A, all the independent variables
mat_bin_fitA = modelsLib(data = data_bin_mat, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_bin_fitA$SVM$results$Accuracy)
max(mat_bin_fitA$NN$results$Accuracy)
max(mat_bin_fitA$RF$results$Accuracy)
max(mat_bin_fitA$DT$results$Accuracy)

saveRDS(mat_bin_fitA, "./predictions/models/mat_bin_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
pcc(data_bin_mat$G2,data_bin_mat$G3)

# B: A without G2
mat_bin_fitB = modelsLib(data = data_bin_mat[, !(colnames(data_bin_mat)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_bin_fitB$SVM$results$Accuracy)
max(mat_bin_fitB$NN$results$Accuracy)
max(mat_bin_fitB$RF$results$Accuracy)
max(mat_bin_fitB$DT$results$Accuracy)

saveRDS(mat_bin_fitB, "./predictions/models/mat_bin_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
pcc(data_bin_mat$G1,data_bin_mat$G3)

# C: A without G2 & G1
mat_bin_fitC = modelsLib(data = data_bin_mat[, !(colnames(data_bin_mat)%in%c("G1","G2"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(mat_bin_fitC$SVM$results$Accuracy)
max(mat_bin_fitC$NN$results$Accuracy)
max(mat_bin_fitC$RF$results$Accuracy)
max(mat_bin_fitC$DT$results$Accuracy)

saveRDS(mat_bin_fitC, "./predictions/models/mat_bin_fitC.RDS")
# for the Naive Bayes in the paper the most frequent value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
m=names(which.max(summary(data_bin_mat$G3)))
pcc(factor(rep(m, 395), levels = c("fail", "pass")),data_bin_mat$G3)


# PORTUGUESE CLASS
data_bin_por = readRDS("./data/student-por_binary.rds")

# A, all the independent variables
por_bin_fitA = modelsLib(data = data_bin_por, 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_bin_fitA$SVM$results$Accuracy)
max(por_bin_fitA$NN$results$Accuracy)
max(por_bin_fitA$RF$results$Accuracy)
max(por_bin_fitA$DT$results$Accuracy)

saveRDS(por_bin_fitA, "./predictions/models/por_bin_fitA.RDS")

# for the Naive Bayes in the paper G2 was used in the setting A
# we check the error with our custom function
pcc(data_bin_por$G2,data_bin_por$G3)

# B: A without G2
por_bin_fitB = modelsLib(data = data_bin_por[, !(colnames(data_bin_por)%in%"G2")], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_bin_fitB$SVM$results$Accuracy)
max(por_bin_fitB$NN$results$Accuracy)
max(por_bin_fitB$RF$results$Accuracy)
max(por_bin_fitB$DT$results$Accuracy)

saveRDS(por_bin_fitB, "./predictions/models/por_bin_fitB.RDS")
# for the Naive Bayes in the paper G1 was used in the setting B
# we check the error with our custom function
pcc(data_bin_por$G1,data_bin_por$G3)

# C: A without G2 & G1
por_bin_fitC = modelsLib(data = data_bin_por[, !(colnames(data_bin_por)%in%c("G2","G1"))], 
                                  formula = G3~., 
                                  modellist = modellist,
                                  metric = "Accuracy",
                                  model_setup = model_setup )

max(por_bin_fitC$SVM$results$Accuracy)
max(por_bin_fitC$NN$results$Accuracy)
max(por_bin_fitC$RF$results$Accuracy)
max(por_bin_fitC$DT$results$Accuracy)

saveRDS(por_bin_fitC, "./predictions/models/por_bin_fitC.RDS")
# for the Naive Bayes in the paper the average output value was used in the setting C
# we check the error with our custom function
# we won't do it using CV, we will come to the same value
p=names(which.max(summary(data_bin_por$G3)))
pcc(factor(rep(p, 649), levels = c("fail", "pass")),data_bin_por$G3)
