# feature importance main
source("./feature_importance/feature_importance.R")

data_bin_mat = readRDS("./data/student-mat_binary.rds")
data_reg_mat = readRDS("./data/student-mat_reg.rds")
data_mul_mat = readRDS("./data/student-mat_multiclass.rds")

data_bin_por = readRDS("./data/student-por_binary.rds")
data_reg_por = readRDS("./data/student-por_reg.rds")
data_mul_por = readRDS("./data/student-por_multiclass.rds")

data_list = list(data_bin_mat, data_reg_mat, data_mul_mat, data_bin_por, data_reg_por, data_mul_por)
plots_path = "./feature_importance/Importancy_plots/"

mat = feature_imp(G3~., data_frame = data_bin_mat, path = plots_path)
por = feature_imp(G3~., data_frame = data_bin_por, path = plots_path)

#feature_imp(G3~.-1, data_frame = data_bin_mat, path = plots_path, type = "glm")
#feature_imp(G3~.-1, data_frame = data_bin_por, path = plots_path, type = "glm")

variables_mat_to_drop = c("Pstatus", "reason", "freetime", "nursery", "Fjob")

source("./predictions/SPL_predictions.R")
model_setup = list("NN" = list(tuneGrid = expand.grid(decay = 10^seq(-4, 0, 0.5), size = seq(3, 13, 2)), maxit = 100, method = "nnet"), 
                   "RF" = list(tuneGrid = expand.grid(mtry = c(5, 8, 10, 12, 15, 20)), ntree = 100, method = "rf"), 
                   "rpart" = list(tuneGrid = expand.grid(cp = seq(0.001, 0.1, 0.01)), method = "rpart"), 
                   "SVMradial" = list(tuneGrid = expand.grid(sigma = 2^seq(-12, -1), C = 2^seq(-12, 12)), method = "svmRadial"))

modellist = c("SVMradial", "NN", "RF", "rpart")


# forecast with only important features
fit_custom = modelsLib(data = data_bin_mat[,!(names(data_bin_mat)%in%variables_mat_to_drop)], 
                         formula = G3~., 
                         modellist = modellist,
                         metric = "Accuracy",
                         model_setup = model_setup )

max(fit_custom$SVMradial$results$Accuracy)
max(fit_custom$NN$results$Accuracy)
max(fit_custom$RF$results$Accuracy)
max(fit_custom$rpart$results$Accuracy)
