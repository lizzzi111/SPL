# here we are going to implement hierarchical clastering
# input, data frame with arbitrary number of numeric variables, 
# and wished number of clusters 

# moreover the next function identifies on which variables our clusters significantly differ
# assumption: criteria for anova are fullfilled

nhcl = function(test_data, n_cluster){
  dist_matrix = dist(test_data)
  fit = hclust(dist_matrix)
  test_data$cluster = factor(cutree(fit, n_cluster)) 
  return(test_data)
}

get_difference = function(test_data, n_cluster=NULL){
  if(!is.null(n_cluster) & !("cluster"%in%names(test_data))){
    test_data = nhcl(test_data, n_cluster)
  }
  anova = sapply(test_data[,!(names(test_data)%in%"cluster")], 
                 function(var) summary(aov(var~cluster, test_data))[[1]]$`Pr(>F)`[1])  
  vars = names(anova)[anova<0.05]
  return(vars)
}