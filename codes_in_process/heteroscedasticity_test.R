# Test on the heteroscedasticity
# Input: 2 parameters vector of dependent variable (regressand) and the matrix or data frame of 
# independent variables indep (regressors)

# needs revision (better functionality and parameter definition)
hetero_test <-  function(dep, indep){
  library(lmtest)
  fit = lm(dep~.,indep)
  res_fit =  bptest(fit) #summary(lm(fit$residuals^2~.,indep))
  ifelse(res_fit$p.value>0.05, 
         return("Assumption of homoscedasticity holds"), 
         return("Assumption of homoscedasticity is rejected"))
}