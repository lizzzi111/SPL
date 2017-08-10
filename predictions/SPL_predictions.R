# This function will produce a list of trained models for a given data set,
# for a diverse number of algorithms provided by varet package.
# The idea of this functionality was to wrap up model building int o one library, where you can
# specify only how you want to train the model and with which algorithms.
# Moreover it allows you to parallize the process if multiple CPU cores are available.


modelsLib = function(formula, data, modellist = c("RF"), model_setup, 
                              metric , model_control = NULL, preProcess = NULL) {
  list.of.packages <- c("doParallel","foreach", "caret")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  library(caret); library(foreach); library(doParallel); library(e1071); library(randomForest)
  
  # Parallelization
  runParallel = detectCores() -1
  # Setup up parallel backend
  # Detect number of cores and register all of them minus 1 as our working cluster
  cl = makeCluster(min(detectCores()-1, runParallel))
  registerDoParallel(cl)
  cat(paste(getDoParWorkers(),"cores are registered to a cluster.\n"))
  
  start.time = Sys.time()
  
  # in order to identify what kind of DM problem we are dealing with
  y = c(data[, names(data) %in% as.character(formula)[2]])
  # Specify model estimation control settings, as given in the paper
  if (is.null(model_control)) {
    model_control = trainControl(method = "cv", number = 10, repeats = 200, 
                                 savePredictions = FALSE, 
                                 classProbs = ifelse(is.factor(y), TRUE, FALSE),
                                 allowParallel = TRUE,
                                 returnData = FALSE)
  }
  
  
  # Training use caret package
  cat("Start model training \n")
  modelLibrary = list()
  for (model in modellist) {
    cat(paste("Start training model", model, "at", Sys.time(), "\n"))
    modelObject = try(do.call(
      caret::train, c(list(form = formula, 
                           data = data, 
                           trControl = model_control, 
                           preProcess = preProcess,
                           metric = metric), 
                      
                      model_setup[[model]])))
    
    if ("train" %in% class(modelObject)) {
      modelLibrary[[model]] = modelObject
    } else {
      warning("Failed to train model ", model, ". ", modelObject)
    }
    rm(modelObject)
  }
  end.time = Sys.time()  # Print time
  cat(paste("Training time:", round(end.time - start.time,2), "\n"))
  
  stopCluster(cl)
  cat("Cluster closed")
  
  return(modelLibrary)
}

