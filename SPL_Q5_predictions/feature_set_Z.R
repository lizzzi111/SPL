# custom feature importance Z
source(file = "./SPL_Q5_predictions/SPL_predictions.R")
source(file = "./SPL_Q5_predictions/metrics_for_evaluation.R")
source(file = "./SPL_Q5_predictions/model_setup.R")

data_reg_mat = readRDS("./data/student-mat_reg.rds")
data_reg_por = readRDS("./data/student-por_reg.rds")
data_bin_mat = readRDS("./data/student-mat_binary.rds")
data_bin_por = readRDS("./data/student-por_binary.rds")
data_mul_mat = readRDS("./data/student-mat_multiclass.rds")
data_mul_por = readRDS("./data/student-por_multiclass.rds")

dataz = function(df){
  df[, !(colnames(df)%in%c("Pstatus", "reason", "freetime", "nursery", "Fjob"))]
  return(df)
}


fitZ = modelsLib(data = dataz(data_reg_por), 
                         formula = G3~., 
                         modellist = modellist,
                         metric = "RMSE",
                         model_setup = model_setup_reg )

min(fitZ$SVM$results$RMSE)
min(fitZ$NN$results$RMSE)
min(fitZ$RF$results$RMSE)
min(fitZ$DT$results$RMSE)

rmse(rowMeans(cbind(dataz(data_reg_por)$G1, dataz(data_reg_por)$G2)),dataz(data_reg_por)$G3)

fitZ = modelsLib(data = dataz(data_mul_mat), 
                 formula = G3~., 
                 modellist = modellist,
                 metric = "Accuracy",
                 model_setup = model_setup )

max(fitZ$SVM$results$Accuracy)
max(fitZ$NN$results$Accuracy, na.rm = TRUE)
max(fitZ$RF$results$Accuracy)
max(fitZ$DT$results$Accuracy)


pcc(dataz(data_mul_por)$G2,dataz(data_mul_por)$G3)
