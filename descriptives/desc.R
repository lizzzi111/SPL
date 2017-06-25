
# distribution of the target variable
# summary 

data_path = "./data/"
files = list.files(data_path, pattern = ".rds" )


#plot(data_frame$G3, xlab = "Final Grades", main = file_name2)

par(mfrow=c(2,3))

des = function(data_path, file){
  df = readRDS(paste0(data_path, file))
  file_name = strsplit(file, split = "[.]")[[1]][1]
  if(is.numeric(df$G3)){
    plot = hist(df$G3, xlab = "Final Grades", main = file_name, ylab = "Absolute Frequency", col = "grey")
  } else {
    plot = plot(df$G3, xlab = "Final Grades", main = file_name, ylab = "Absolute Frequency")
  }
  return(plot)
}

sapply(files, function(x) des(data_path, x))
