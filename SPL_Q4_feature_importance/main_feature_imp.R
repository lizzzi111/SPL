# feature importance main
source("./SPL_Q4_feature_importance/feature_importance.R")

data_bin_mat = readRDS("./data/student-mat_binary.rds")
data_bin_por = readRDS("./data/student-por_binary.rds")

data_reg_mat = readRDS("./data/student-mat_reg.rds")
data_reg_por = readRDS("./data/student-por_reg.rds")

plots_path = "./SPL_Q4_feature_importance/Importancy_plots/"

mat = feature_imp(G3~., data_frame = data_bin_mat, path = plots_path)
por = feature_imp(G3~., data_frame = data_bin_por, path = plots_path)

feature_imp(G3~.-1, data_frame = data_bin_mat, path = plots_path, type = "glm")
feature_imp(G3~.-1, data_frame = data_bin_por, path = plots_path, type = "glm")


feature_imp(G3~.-1, data_frame = data_reg_mat, path = plots_path, type = "mars")
feature_imp(G3~.-1, data_frame = data_reg_por, path = plots_path, type = "mars")

feature_imp(G3~.-1, data_frame = data_reg_mat, path = plots_path, type = "step_lm")
feature_imp(G3~.-1, data_frame = data_reg_por, path = plots_path, type = "step_lm")

#variables_mat_to_drop = c("Pstatus", "reason", "freetime", "nursery", "Fjob")
