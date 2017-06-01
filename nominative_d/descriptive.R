# nominative descriptives
setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/")

# this example of descriptive data will be done on the set with mathematics
# however exactly the same code can be used for portuguese or their mix
data = readRDS("student-mat_fact.rds")
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
custom_graph(data$Walc)
custom_graph(data$Dalc, prop=TRUE)
custom_graph(data$school)
custom_graph(data$famsize)
custom_graph(data[, c("famsize", "Medu" )], "mosaicplot")
custom_graph(data[, c("famsize", "Fedu" )], "mosaicplot")

custom_graph(data[, c("freetime", "goout" )], "mosaicplot")
