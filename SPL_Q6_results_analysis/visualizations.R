binary = read.csv2("./SPL_Q6_results_analysis/binary_fin.csv")
regression = read.csv2("./SPL_Q6_results_analysis/regression_fin.csv")
multiclass = read.csv2("./SPL_Q6_results_analysis/multiclass_fin.csv")
# here the functionality for result plots will be proposed

result_plots = function(df, title = NULL) {
  
  list.of.packages = c("ggplot2")
  new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  library(ggplot2)
  
  regr_title = "Regression Results"
  class_title = "Classification Performance Results"
  
  
  if (any(names(df) %in% "rmse")) {
    title = ifelse(is.null(title), regr_title, title)
    ax_title_y = "Root Mean Squarred Error"
  } else {
    df$rmse = df$pcc
    title = ifelse(is.null(title), class_title, title)
    ax_title_y = "Percentage of Correctly Classified values"
  }
  
  
  boxplot = ggplot(aes(y = rmse, x = model), data = df) + 
            geom_boxplot() + 
            theme_bw() + 
            labs(y = ax_title_y) + 
            ggtitle(title)
  
  line_plot = ggplot(data = df, aes(x = model, y = rmse, group = interaction(Input_type,Class), shape = Input_type)) +
              geom_line(aes(linetype = Input_type, color = Input_type), size = 2) + 
              geom_point(aes(color = Input_type), size = 4) + 
              facet_grid(Class~.)+
              theme(text = element_text(size = 15), 
              axis.text.x = element_text(angle = 90, hjust = 1), 
              plot.title = element_text(size = 15,lineheight = 0.8, face = "bold")) + 
              ggtitle(title) + 
              theme_bw() + 
              labs(y = ax_title_y)
  
  output = list(boxplot = boxplot, lines = line_plot)
  return(output)
}

bin = result_plots(binary, "Binary Classification Performance")
bin$boxplot
bin$lines
mul = result_plots(multiclass, "Multiclass Classification Performance")
mul$boxplot
mul$lines
reg = result_plots(regression)
reg$boxplot
reg$lines
