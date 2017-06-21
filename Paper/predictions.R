createModelLibrary = function(formula, data, modellist = c("rf"), model_setup, metric = "ROC", model_control = NULL, preProcess = NULL){
  start.time = Sys.time()
  
  # in order to identify what kind of DM problem we are dealing with
  y = c(data[,names(data)%in%as.character(formula)[2]])
  # Specify model estimation control settings, as given in the paper
  if(is.null(model_control)){
    model_control = trainControl(method = "cv",
                                 number = 10,
                                 repeats = 200,
                                 savePredictions = FALSE,
                                 classProbs = TRUE,
                                 returnData = FALSE
    )
}
  
  
  # Training
  # use caret package
  message("Start model training")
  modelLibrary = list()
  for(model in modellist){
    message(paste("Start training model", model, "at", Sys.time()))
    modelObject = try(do.call(caret::train, c(
      list(
        form = formula,
        data = data,
        trControl = model_control,
        preProcess = preProcess,
        metric = metric 
      ),
      model_setup[[model]]
      ))
    ) 
    if("train" %in% class(modelObject)) {modelLibrary[[model]] = modelObject
    } else{warning("Failed to train model ", model, ". ", modelObject)}
    rm(modelObject)
  }
  end.time = Sys.time() # Print time
  message(paste("Training time:",end.time - start.time))
  
  # Save trained model library
  #saveRDS(modelLibrary, sprintf("modelLibrary %s target_%s balanced_%s varSelection_%s.rds", analysisName, targetVariable, balanceDataset, variableSelectionMethod)) 
  
  return(modelLibrary)
}

model_setup = list("NN" =  list("tuneGrid" = expand.grid(decay = 10^seq(-4, 0, 0.5)
                                                               ,size = seq(3,13,2))
                                      ,"maxit" = 100
                                      , "method" = "nnet")
                       , "RF" =  list("tuneGrid" = expand.grid(mtry= c(5,8,10,12,15,20))
                                      , "ntree"= 100,
                                       "method" = "rf")
                       , "rpart" =  list("tuneGrid" = expand.grid(cp = seq(0.001,0.1,0.01))
                                         , "method" = "rpart")
                       , "SVMradial" =  list("tuneGrid" = expand.grid(sigma= 2^seq(-12,-1),
                                                                      C = 2^seq(-12,12))
                                             ,"method" = "svmRadial")
                       , "SVMlinear" =  list("tuneGrid" = expand.grid(C = 2^seq(-12,12))
                                             ,"method" = "svmLinear")
)

