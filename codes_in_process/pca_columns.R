# function performing PCA and adding either only first two components or all to the initial df or matrix
# input: data frame with arbitrary number of numeric variables (only numerics), or matrix
# output: df with new columns

pca_2 = function(d){
  if(all(sapply(d, is.numeric))){
    d = cbind(d, as.data.frame(prcomp(d)$x)[,c("PC1", "PC2")])
    return(d)
  } else {
    cat("Input has to be a data frame of numeric data or a matrix.")
  }
}

pca_all = function(d){
  if(all(sapply(d, is.numeric))){
    d = cbind(d, as.data.frame(prcomp(d)$x))
    return(d)
  } else {
    cat("Input has to be a data frame of numeric data or a matrix.")
  }
}

pca_imp = function(d, threshold=0.9){
  if(all(sapply(d, is.numeric))){
    fit = prcomp(d)    
    cum_prop = summary(fit)$importance['Cumulative Proportion',]    
    d = cbind(d, fit$x[,1:min(which(cum_prop>threshold))])    
    return(d) 
  } else {
    cat("Input has to be a data frame of numeric data or a matrix.")
  }
}

