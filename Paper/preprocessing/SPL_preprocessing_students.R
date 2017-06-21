# Data preprocessing (converting to factors), preprocess for the
# respective approach list all the .csv files in the folder we need
files = function(path) {
  files = list.files(path, pattern = ".csv")
  return(files)
}

data_path = "./data/"
files = files(data_path)

#### Convert to factors (can be a function(data,list of parameters to make
#### factors, which of them ordered(only of it easy to assume which
#### order)))
data_prep = function(file_name, type = "multiclass") {
  if ("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr")
  }
  library(dplyr)
  data_frame = read.csv(paste0(data_path, file_name), sep = ",", header = TRUE)
  file_name = strsplit(file_name, split = "[.]")[[1]][1]
  variables_to_factors = c("Medu", "Fedu", "famrel", "freetime", "health", 
                           "goout", "Walc", "Dalc", "traveltime", "studytime")
  
  data_frame = data_frame %>% mutate_each_(funs(factor), vars = variables_to_factors)
  
  if (type == "regression") {
    saveRDS(data_frame, file = paste0(data_path, file_name, "_reg.rds"))
    cat("Data preprocessing for", file_name, "as a", type, "problem is finished\n")
  } else if (type == "multiclass") {
    data_frame$G1 = factor(ifelse(data_frame$G1 >= 16, "excellent_vgood", 
                                  ifelse(data_frame$G1 >= 14, "good", ifelse(data_frame$G1 >= 
                                                                               12, "satisfactory", ifelse(data_frame$G1 >= 10, "sufficient", 
                                                                                                          "fail")))))
    data_frame$G2 = factor(ifelse(data_frame$G2 >= 16, "excellent_vgood", 
                                  ifelse(data_frame$G2 >= 14, "good", ifelse(data_frame$G2 >= 
                                                                               12, "satisfactory", ifelse(data_frame$G2 >= 10, "sufficient", 
                                                                                                          "fail")))))
    data_frame$G3 = factor(ifelse(data_frame$G3 >= 16, "excellent_vgood", 
                                  ifelse(data_frame$G3 >= 14, "good", ifelse(data_frame$G3 >= 
                                                                               12, "satisfactory", ifelse(data_frame$G3 >= 10, "sufficient", 
                                                                                                          "fail")))))
    
    saveRDS(data_frame, file = paste0(data_path, file_name, "_multiclass.rds"))
    cat("Data preprocessing for", file_name, "as a multiclass", type, 
        "problem is finished\n")
  } else if (type == "binary") {
    data_frame$G1 = factor(ifelse(data_frame$G1 >= 10, "pass", "fail"))
    data_frame$G2 = factor(ifelse(data_frame$G2 >= 10, "pass", "fail"))
    data_frame$G3 = factor(ifelse(data_frame$G3 >= 10, "pass", "fail"))
    saveRDS(data_frame, file = paste0(data_path, file_name, "_binary.rds"))
    cat("Data preprocessing for", file_name, "as a", type, "classification problem is finished\n")
  }
}

# use our function for both data frames
sapply(files, function(x) data_prep(x, "binary"))
sapply(files, function(x) data_prep(x, "multiclass"))
sapply(files, function(x) data_prep(x, "regression"))