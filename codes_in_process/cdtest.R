# we test whether two categorical variables are independent or not
# Input either x,y are the vectors of the same length, or y is not given and x contain a data frame of two categorical variables
# the default significance leve is 0.05

cdtest =  function(x, y=NULL, alpha=0.05){
  if(is.null(x)){
    l = table(x) 
  } else {
    l = table(x,y)
  }
  
  if(any(l<5)) {
    res = fisher.test(l)
    cat("Data have less than 5 observations in at least one of the sells in the contingency table,
          Chi-Square test cannot be applied, accortingly, the Fisher exact test is used.\n")
    if(res$p.value<alpha){
      cat("Used level of significance is", paste0(alpha,"."), "The Zero hypothesis is rejected: variables are independent.\n")
    } else {
      cat("Used level of significance is", paste0(alpha,"."),"The Zero hypothesis is not rejected: variables are dependent.\n")
    }
    cat("The according P-value is:", round(res$p.value, digits = 2, sep = " "))
    return(c(res$p.value))
  }else{
    res = chisq.test(l)
    cat("The Chi-Square test is applied.\n")
    
    if(res$p.value<alpha){
      cat("Used level of significance is", paste0(alpha,"."),"The Zero hypothesis is rejected: variables are independent.\n")
      }
    else{
      cat("Used level of significance is", paste0(alpha,"."),"The Zero hypothesis is not rejected: variables are dependent.\n")
    }
    
    cat("The according test statistics, number of degrees of freedom and P-value are:\n",
        round(res$statistic, digits = 2),  round(res$parameter, digits = 2), round(res$p.value, digits = 2), sep = " ")
    return(c(res$statistic, res$parameter, res$p.value))
}
}