# Data preprocessing (converting to factors)

setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/data")
files = list.files()

#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

#### Convert to factors (can be a function(data,list of parameters to make factors, which of them ordered(only of it easy to assume which order)))
data_prep = function(file_name){
  library(dplyr)
  data_frame = read.csv(file_name, sep = ",", header = TRUE)
  file_name = strsplit(file_name, split = "[.]")[[1]][1]
  variables_to_factors = c("Medu", "Fedu", "famrel", "freetime",
                           "health", "goout", "Walc", "Dalc", 
                           "traveltime", "studytime")
  
  data_frame = data_frame %>%
    mutate_each_(funs(factor), vars=variables_to_factors)
  
  saveRDS(data_frame, file = paste0(file_name, "_fact.rds"))
  cat("Data preprocessing for", file_name, "is finished\n")
}

# use our function for both data frames
sapply(files, data_prep)
