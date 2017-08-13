# This is a helper function to identify variables having only one
# constant value
check_unique = function(df) {
  out = lapply(df, function(x) length(unique(x)))
  want = which(!out > 1)
  unlist(want)
}

variance_check = function(df, goal_var) {
  # First we drop the dependent variable, since we check only predictors
  df1 = df[, !names(df) %in% goal_var]
  
  # we divide into factors and non factors since they will be handled
  # differently
  factors_ind = sapply(df1, is.factor)
  non_factors_ind = sapply(df1, is.numeric)
  
  factors_df = df1[, factors_ind]
  non_factors_df = df1[, non_factors_ind]
  
  # factors: to calculate variance for a factor variable would be wrong
  # therefore we would aim to look for constants. If some variable has
  # only one unique value, it won't give us any additional predictive
  # power.  It would only take computational power into account.
  # Especially in the cases where we have some similarity matrices.
  
  const_fac = check_unique(factors_df)
  
  # for numerics we simply check the variance we took 0.09 as a
  # threshold,
  n = sapply(non_factors_df, var) < 0.08
  
  # In the last step we produce textual output and return the names of
  # the variables having few variability as a vector
  
  if (length(const_fac) == 0 & length(n[n == TRUE]) == 0) {
    cat("There are no suscpicious variables with little variance or having only one unique value.\n")
  } else if (length(const_fac) == 1 & length(n[n == TRUE]) == 0) {
    cat("The variable ", names(const_fac), " has only one unique value.\n")
    return(names(const_fac))
  } else if (length(const_fac) > 1 & length(n[n == TRUE]) == 0) {
    cat("The variables ", names(const_fac), " have only one unique value.\n")
    return(names(const_fac))
  } else if (length(const_fac) == 0 & length(n[n == TRUE]) == 1) {
    cat("The variable ", names(n[n == TRUE]), " has only one unique value.\n")
    return(names(n[n == TRUE]))
  } else if (length(const_fac) == 0 & length(n[n == TRUE]) > 1) {
    cat("The variables ", names(n[n == TRUE]), " have only one unique value.\n")
    return(names(n[n == TRUE]))
  } else if (length(const_fac) > 0 & length(n[n == TRUE]) > 1) {
    cat("The variables ", names(n[n == TRUE]), "&", names(const_fac), 
        " has only one unique value.\n")
    return(c(names(n[n == TRUE]), names(const_fac)))
  }
}