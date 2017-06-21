binary = read.csv2("./results_analysis/binary.csv")
library(ggplot2)
plot(pcc~model, binary)
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
      width=0.25) + theme_bw() + facet_grid(binary$Class~.)

# boxplots
qplot(data=binary,
      y=pcc,
      x=model,
      group=Inut_type,
      geom=c(""),
      color=Inut_type,
      shape=Inut_type,
      width=0.25) + theme_bw() + facet_grid(binary$Class~.)
