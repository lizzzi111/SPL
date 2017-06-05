# nominative descriptives
setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/")

# this example of descriptive data will be done on the set with mathematics
# however exactly the same code can be used for portuguese or their mix
data = readRDS("./data/student-mat_fact.rds")
#data = read.table("student-por.csv",sep=",",header=TRUE)

# let's look at all the distributions
summary(data)

# simple barplot 
custom_graph = function(variable, type="barplot", prop = FALSE){
  if(prop==TRUE){
    if(type=="barplot"){
      plot = barplot(prop.table(table(variable)), main=paste0("Relative frequency of the variable ", deparse(substitute(variable))))
    }
    if(type=="pie"){
      plot = pie(prop.table(table(variable)), main=paste0("Relative frequency of the variable ", deparse(substitute(variable))))
    }
    if(type=="mosaicplot"){
      plot = mosaicplot(prop.table(table(variable)), main=paste0("Relative frequency of the variable ", deparse(substitute(variable))))
    }
    if(type=="spineplot"){
      plot = spineplot(prop.table(table(variable)), main=paste0("Relative frequency of the variable ", deparse(substitute(variable))))
    }
  } else {
  if(type=="barplot"){
    plot = barplot(table(variable), main=paste0("Absolute frequency of the variable ", deparse(substitute(variable))))
  }
  if(type=="pie"){
    plot = pie(table(variable), main=paste0("Absolute frequency of the variable ", deparse(substitute(variable))))
  }
  if(type=="mosaicplot"){
    plot = mosaicplot(table(variable), main=paste0("Absolute frequency of the variable ", deparse(substitute(variable))))
  }
  if(type=="spineplot"){
    plot = spineplot(table(variable), main=paste0("Absolute frequency of the variable ", deparse(substitute(variable))))
  }
  }
}

#### Visualizations using the custom graph funtion
# some basic data visualizations
custom_graph(data$sex, "pie")
custom_graph(data$age)
custom_graph(data$Walc)
custom_graph(data$Dalc, prop=TRUE)
custom_graph(data$school, "pie")
custom_graph(data$famsize, "pie")
custom_graph(data[, c("famsize", "Medu" )], "mosaicplot")
custom_graph(data[, c("famsize", "Fedu" )], "mosaicplot")
custom_graph(data[, c("freetime", "goout" )], "mosaicplot")

# Through these basic visualizations we can see that it is quite difficult to generalize 
# plot for every type of variable. It is quite informative, however, it can be improved.
# So we'd present other options which are adjusted for each variable or problem

# week
plot_ly(data, x = ~Walc, type = "histogram", 
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(title = "Alcohol Consumption during the Week")
print("Percent of students in each drinking level:")
table(data$Walc) / nrow(data)

# weekend
plot_ly(data, x = ~Dalc, type = "histogram", 
        marker = list(line = list(color = 'white', width = 1))) %>%
  layout(title = "Alcohol Consumption during the Weekend")
print("Percent of students in each drinking level:")
table(data$Dalc) / nrow(data)

plot_ly(data, x = ~Dalc, y = ~G3, type="box")
plot_ly(data, x = ~Walc, y = ~G3, type="box")

plot_ly(data, x = ~Dalc, y = ~G2, type="box")
plot_ly(data, x = ~Walc, y = ~G2, type="box")

plot_ly(data, x = ~Dalc, y = ~G1, type="box")
plot_ly(data, x = ~Walc, y = ~G1, type="box")

#we can see that means within each alcohol consumption level is quite similar
source("../SPL/codes_in_process/smart_anova.R")
grades_vars = c(paste0("data$", c("G1","G2","G3")))
group_vars = c(paste0("data$", c("Walc","Dalc","school", "Fedu", "Medu", "Pstatus", "Mjob", "Fjob")))

sapply(data[,names(data)%in%grades_vars], function(dep) sapply(data[,names(data)%in%group_vars], function(indep) smart_anova(dep,indep)))

smart_anova(data$G3, data$Dalc)
smart_anova(data$G3, data$Walc)

smart_anova(data$G2, data$Dalc)
smart_anova(data$G2, data$Walc)

smart_anova(data$G1, data$Dalc)
smart_anova(data$G1, data$Walc)

# therefore we can see that there is no significance diffirence or influence of the Alcohol on the grades
