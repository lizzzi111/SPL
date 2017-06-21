# metrics

pcc = function(y_pred, y_true){
  if (length(y_pred)!=length(y_true)){
    return("The vectors have to be of the same length")
  } else if (!is.factor(y_pred)|!is.factor(y_pred)){
    return("Percentage of Corrrect Classifications can be calculated only on the factors")
  } else {
    pcc =   sum(y_pred==y_true)/length(y_pred)
    return(pcc)    
  }
}

# root mean squarred
rmse  = function(y_pred, y_true){
  if (length(y_pred)!=length(y_true)){
    return("The vectors have to be of the same length")
  } else if (!is.numeric(y_pred)|!is.numeric(y_pred)){
    return("Root mean sqared can be calculated only on numeric data")
  } else {
    rmse =   sqrt(mean((y_true - y_pred)^2))
    return(rmse)    
  }
}
