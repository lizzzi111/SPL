# In this file we are presenting some descriptives. 
# The aim on working with all the files at the same time, just by calling the names of files 
# within one folder.
# It will produce the plots for all of thems on one files

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

mult_plot = function(data_path = "./"){
  graphics.off()
  files = list.files(data_path, pattern = ".rds" )
  
  if(length(files)<1){
    message('Please, provide data path containing any .rds files')
    return()
  } else if((length(files)>9)) {
    message('Please, use the des() function instead, 
            more than 9 plots on one image cannot be properly depicted.')
    return()
  } else if((length(files)>=1 & length(files)<4)) {
    par(mfrow=c(1,length(files)))
  } else if ((length(files)>=4 & length(files)<=6)){
    par(mfrow=c(2,ceiling(length(files)/2)))
  } else if ((length(files)>6 & length(files)<=9)){
    par(mfrow=c(3,ceiling(length(files)/3)))
  }
  return(sapply(files, function(x) des(data_path, x)))
}


# TESTS
# 6 files
ff = mult_plot("./data/")
# default
ll = mult_plot()
# 2 files
two = mult_plot("./dep_var_dist/test_2 files/")
# 4 files
four = mult_plot("./dep_var_dist/test_4_files/")
# 8 files
ei = mult_plot("./dep_var_dist/test_8_files/")
# more than 9 files
mnine = mult_plot("./dep_var_dist/test_more_than_9_files/")
