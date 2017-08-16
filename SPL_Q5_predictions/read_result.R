read_the_results = function(obj, model, type = 'class'){
  if(type =='class'){
    result =  max(obj$model$results$Accuracy)
  } else if(type =='reg'){
    result = min(obj$model$results$RMSE) 
  } else {
    return('The type provided is unknown. Classification "class" and Regression "reg" are only available. ')
  }
   return(result) 
}