#the function receives a matrix or df with numerical data
# it checks whether there are linear combination of variables
# in R 1 is not always equal to 1, therefore we needed to convert our correlations into characters
# output: variables' names which are linear combinations of each other
is_multicol <- function(d){
  f = cor(d)
  diag(f) = 0
  f = as.data.frame(f)
  vars = sapply(f,function(x) max(abs(x)))
  vars = vars[as.character(vars)=="1"]
  ifelse(length(vars)>0, return(names(vars)), return("There is no linear combinations in the data"))
}