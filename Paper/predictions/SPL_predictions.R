createModelLibrary = function(formula, data, modellist = c("RF"), model_setup, 
                              metric , model_control = NULL, preProcess = NULL) {
  list.of.packages <- c("doParallel","foreach", "caret")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(caret); library(foreach); library(doParallel)
  
  # Parallelization
  runParallel = detectCores() -1
  # Setup up parallel backend
  ##### Setting up parallelization
  cl <- makeCluster(min(detectCores()-1, runParallel))
  #, outfile="") # This redirects the output to the R master console but not in RStudio
  # on.exit(stopCluster(cl))
  registerDoParallel(cl)
  message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))
  
  start.time = Sys.time()
  
  # in order to identify what kind of DM problem we are dealing with
  y = c(data[, names(data) %in% as.character(formula)[2]])
  # Specify model estimation control settings, as given in the paper
  if (is.null(model_control)) {
    model_control = trainControl(method = "cv", number = 10, repeats = 200, 
                                 savePredictions = FALSE, classProbs = TRUE, returnData = FALSE)
  }
  
  
  # Training use caret package
  message("Start model training")
  modelLibrary = list()
  for (model in modellist) {
    message(paste("Start training model", model, "at", Sys.time()))
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
  message(paste("Training time:", end.time - start.time))
  
  stopCluster(cl)
  
  return(modelLibrary)
}

