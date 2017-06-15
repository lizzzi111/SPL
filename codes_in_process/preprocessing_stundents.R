# Data preprocessing (converting to factors)
setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/data")
files = list.files()

#### Convert to factors (can be a function(data,list of parameters to make factors, which of them ordered(only of it easy to assume which order)))
data_prep = function(file_name, type = "regression"){
  library(dplyr)
  data_frame = read.csv(file_name, sep = ",", header = TRUE)
  file_name = strsplit(file_name, split = "[.]")[[1]][1]
  variables_to_factors = c("Medu", "Fedu", "famrel", "freetime",
                           "health", "goout", "Walc", "Dalc", 
                           "traveltime", "studytime")
  
  data_frame = data_frame %>%
    mutate_each_(funs(factor), vars=variables_to_factors)
  
  if(type == "regression"){
    saveRDS(data_frame, file = paste0(file_name, "_reg_fact.rds"))
    cat("Data preprocessing for", file_name, " as ", type, "problem is finished\n")
  }(type == "classification"){
    data_frame$G1 = factor(ifelse(data$G1 > 17.5, "excellent", 
                                  ifelse(data$G1 > 15.5, "very_good", 
                                         ifelse(data$G1 > 13.5, "good", 
                                                ifelse(data$G1 > 9.5, "sufficient", 
                                                       ifelse(data$G1 > 3.5, "week", "poor"))))))
    data_frame$G2 = factor(ifelse(data$G2 > 17.5, "excellent", 
                                  ifelse(data$G2 > 15.5, "very_good", 
                                         ifelse(data$G2 > 13.5, "good", 
                                                ifelse(data$G2 > 9.5, "sufficient", 
                                                       ifelse(data$G2 > 3.5, "week", "poor"))))))
    data_frame$G3 = factor(ifelse(data$G3 > 17.5, "excellent", 
                                        ifelse(data$G3 > 15.5, "very_good", 
                                                ifelse(data$G3 > 13.5, "good", 
                                                       ifelse(data$G3 > 9.5, "sufficient", 
                                                              ifelse(data$G3 > 3.5, "week", "poor"))))))
                                                                                                                                                  
    saveRDS(data_frame, file = paste0(file_name, "_class_fact.rds"))
    cat("Data preprocessing for", file_name, " as ", type, "problem is finished\n")
  }
}

# use our function for both data frames
sapply(files, data_prep)
