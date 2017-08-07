binary = read.csv2("./results_analysis/binary.csv")
regression = read.csv2("./results_analysis/regression.csv")
multiclass = read.csv2("./results_analysis/multiclass.csv")
# here the functionality for result plots will be proposed

result_plots = function(df, goal_var){
  if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  regr_title = "Regression Performance Results"
  bin_title = "Binary Performance Results"
  mult_title = "Multiclass Performance Results"


  if(is.numeric(df$goal_var)){
    accuracy = rmse
    title = regr_title
  } else {
    if(length(levels(df$goal_var))>2){
      accuracy = pcc
      title = mult_title
    } else {
      accuracy = df$pcc
      title = bin_title
    }
  }
  
  boxplot = plot(accuracy~model, df, main = title)
  line_plot = ggplot(df, aes(x=model, y=accuracy, colour = Input_type, shape=Input_type, group= Input_type))+
    geom_line(size=2) +
    geom_point(size=4) +
    facet_grid(df$Class~.) +
    theme(text = element_text(size=15),
          axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 15,lineheight=.8, face="bold")) + 
    ggtitle(title)+theme_bw()
  output = list(boxplot, line_plot)
  return(output)
}

ff = result_plots(binary, "G3")
result_plots(multiclass, "G3")
result_plots(regression, "G3")


# all
qplot(data=binary,
      y=pcc,
      x=model,
      group=Inut_type,
      geom=c("point", "line"),
      color=Inut_type,
      shape=Inut_type,
      width=0.25) + theme_bw() #+ facet_grid(binary$Class~.)

# portugal class & math class
qplot(data=binary,
      y=pcc,
      x=model,
      group=Inut_type,
      geom=c("point", "line"),
      color=Inut_type,
      shape=Inut_type,
      size=I(2)) + theme_bw() + facet_grid(binary$Class~.)

# boxplots
qplot(data=binary,
      y=pcc,
      x=model,
      group=Inut_type,
      geom=c(""),
      color=Inut_type,
      shape=Inut_type,
      width=0.25) + theme_bw() + facet_grid(binary$Class~.)


ggplot(binary, aes(x=model, y=pcc, colour = Inut_type, shape=Inut_type, group= Inut_type))+
   geom_line(size=2) +
   geom_point(size=4) +
   facet_grid(binary$Class~.) +
   theme(text = element_text(size=15),
  axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 15,lineheight=.8, face="bold")) + 
  ggtitle("Binary Performance Results")+theme_bw()
