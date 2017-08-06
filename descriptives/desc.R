# In this file we are presenting some descriptives. 
# The aim on working with all the files at the same time, just by calling the names of files 
# within one folder.
# It will produce the plots for all of thems on one files

data_path = "./data/"
files = list.files(data_path, pattern = ".rds" )

par(mfrow=c(2,3))

# we want to plot our dependent variable distribution for all the settings at the same time
# therefore, the following function reads data from the file and plots it to a grid of histograms

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
