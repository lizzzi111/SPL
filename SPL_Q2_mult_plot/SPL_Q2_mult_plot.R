# The following functionaly aims to accelarate the plotting of
# variables' distributions.  The aim on working with all the files at
# the same time, just by calling the names of files within one folder.
# It will produce a plot consisting of all multiple plots for all of
# thems on one files.  Assuming that data files have the same
# structure, ot at least the same variable

# We want to plot our dependent variable distribution for all the
# settings (bin, mult, regression) at once.  Therefore, the following
# functions read data from the file and plots it to a grid of
# histograms.  The layout is adjusted accordingly to the number of .rds
# files to plot.  Conclusively saving them into a plots folder Since a
# multiple plot having more than 9 plots is not very readable, our
# function does not allow to plot more than 9.

# it is required to run the function with at least 1 second of time
# pause

des = function(data_path, file, var = df$G3, xlab = "Final Grades", ylab = "Absolute Frequency") {
  df = readRDS(paste0(data_path, file))
  file_name = strsplit(file, split = "[.]")[[1]][1]
  if (is.numeric(var)) {
    plot = hist(var, xlab = xlab, ylab = ylab, main = file_name, col = "grey")
  } else {
    plot = plot(var, xlab = xlab, ylab = ylab, main = file_name)
  }
  return(plot)
}

# the following function is the wrapper for des() to produce a series
# of subplots
mult_plot = function(data_path = "./", savePlot = TRUE) {
  graphics.off()
  files = list.files(data_path, pattern = ".rds")
  if (length(files) < 1) {
    message("Please, provide data path containing any .rds files")
    savePlot = FALSE
    return()
  } else if ((length(files) > 9)) {
    message("Please, use the des() function instead, 
            more than 9 plots on one image cannot be properly depicted.")
    savePlot = FALSE
    return()
  } else if ((length(files) >= 1 & length(files) < 4)) {
    
    par(mfrow = c(1, length(files)))
  } else if ((length(files) >= 4 & length(files) <= 6)) {
    
    par(mfrow = c(2, ceiling(length(files)/2)))
  } else if ((length(files) > 6 & length(files) <= 9)) {
    par(mfrow = c(3, ceiling(length(files)/3)))
  }
  if (savePlot == TRUE) {
    sapply(files, function(x) des(data_path, x))
    savePlot(filename = paste0("./SPL_Q2_mult_plot/plots/plot_", Sys.Date(), 
                               format(Sys.time(), "%H_%M_%S"), ".jpeg"), type = "jpeg")
  }
  return(sapply(files, function(x) des(data_path, x)))
}


# TESTS, EXAMPLES 6 files
ff = mult_plot("./data/")
# default
ll = mult_plot()
# 2 files
two = mult_plot("./SPL_Q2_mult_plot/test_2 files/")
# 4 files
four = mult_plot("./SPL_Q2_mult_plot/test_4_files/")
# 8 files
ei = mult_plot("./SPL_Q2_mult_plot/test_8_files/")
# more than 9 files
mnine = mult_plot("./SPL_Q2_mult_plot/test_more_than_9_files/")

