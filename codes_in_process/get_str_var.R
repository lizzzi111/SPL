# function to get a variable having non-linear dependency to the others,
# detect violation of LR requirements

get_strange_var <- function(d){
  res_fit = sapply(d, function(ind) sapply(d, function(dep) shapiro.test(lm(dep~ind)$residuals/sd(lm(dep~ind)$residuals))$p.value ))
  df = as.data.frame(res_fit)
  diag(df) = 1
  ind = as.data.frame(df < 0.05)
  ind_sum = sapply(ind, function(x) sum(x[x==TRUE]) )
  max_sum = max(ind_sum)
  ifelse(ind_sum>1, return(names(ind_sum[ind_sum==max_sum])), return("There is no strange variable in the data"))
}