# function performing PCA and adding either only first two components or all to the initial df or matrix
# input: data frame with arbitrary number of numeric variables (only numerics), or matrix
# output: df with new columns

get_pc = function(d){
  if(all(sapply(d, is.numeric))){
    d = cbind(d, as.data.frame(prcomp(d)$x)[,c("PC1", "PC2")])
    return(d)
  } else {
    cat("Input has to be a data frame of numeric data or a matrix.")
  }
}

get_pc_all = function(d){
  if(all(sapply(d, is.numeric))){
    d = cbind(d, as.data.frame(prcomp(d)$x))
    return(d)
  } else {
    cat("Input has to be a data frame of numeric data or a matrix.")
  }
}

get_pca_imp <- function(d){
  if(all(sapply(d, is.numeric))){
    fit <- prcomp(d)    
    cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
    d <- cbind(d, fit$x[,1:min(which(cum_prop>0.9))])    
    return(d) 
  } else {
    cat("Input has to be a data frame of numeric data or a matrix.")
  }
}

