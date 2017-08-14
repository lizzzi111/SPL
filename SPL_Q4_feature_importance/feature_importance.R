# feature importance
feature_imp = function(formula, data_frame, path = "./", type = "rf") {
  if (type == "rf") {
    if ("randomForest" %in% rownames(installed.packages()) == FALSE) {
      install.packages("randomForest", dependencies = TRUE)
    }
    library(randomForest)
    fit = randomForest(formula, data_frame, importance = TRUE)
    imp = importance(fit)
    
    
    png(filename = paste0(path, as.character(deparse(substitute(data_frame))), 
                          "var_plot_", type, ".png"))
    var_plot = varImpPlot(fit, main = 'Feature Importance Plot.')
    dev.off()
  } else if (type == "glm") {
    list.of.packages <- c("glmnet", "caret")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, 
                                                                                  "Package"])]
    if (length(new.packages)) 
      install.packages(new.packages, dependencies = TRUE)
    library(glmnet)
    library(caret)
    
    x = model.matrix(formula, data_frame)
    y = c(data_frame[, names(data_frame) %in% as.character(formula)[2]])
    fit = glmnet(x, y, family = ifelse(!is.factor(y), "gaussian", 
                                ifelse(length(levels(y)) > 2, "multinomial", "binomial")))
    
    imp = caret::varImp(fit, lambda = min(fit$lambda))
    imp = cbind.data.frame(colnames(x), imp[, 1])
    imp = imp[order(imp$`imp[, 1]`, decreasing = F), ]
    
    png(filename = paste0(path, as.character(deparse(substitute(data_frame))), 
                          "var_plot_", type, ".png"))
    var_plot = dotchart(imp$`imp[, 1]`[imp$`imp[, 1]` > 
                        quantile(imp$`imp[, 1]`, probs = 0.45)], 
                        labels = imp$`colnames(x)`[imp$`imp[, 1]` > quantile(imp$`imp[, 1]`, probs = 0.45)],
                        main = "Feature Importance Ranking Plot for GLM")
    dev.off()
  } else if (type == "mars") {
    if ("earth" %in% rownames(installed.packages()) == FALSE) {
      install.packages("earth", dependencies = TRUE)
    }
    library(earth)
    
    if (!is.factor(data_frame[, names(data_frame) %in% as.character(formula)[2]])) {
      fit = earth(formula, data_frame)  # build model
      imp = evimp(fit)
      
      
      png(filename = paste0(path, as.character(deparse(substitute(data_frame))), 
                            "var_plot_", type, ".png"))
      var_plot = plot(imp, main = 'Feature Importance Plot')
      dev.off()
    } else {
      return("Mars model is used for the regression, therefore, it requires a numeric dependent variable")
    }
  } else if (type == "step_lm") {
    if (!is.factor(data_frame[, names(data_frame) %in% as.character(formula)[2]])) {
      x = as.data.frame(model.matrix(formula, data_frame))
      y = c(data_frame[, names(data_frame) %in% as.character(formula)[2]])
      
      
      base.mod = lm(y ~ 1, x)  # base intercept only model
      all.mod = lm(y ~ ., x)  # full model with all predictors
      stepMod = step(base.mod, scope = list(lower = base.mod, upper = all.mod), 
                     direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
      
      
      imp = cbind.data.frame(stepMod$coefficients)
      imp$abs = abs(imp$`stepMod$coefficients`)
      imp = imp[order(imp$abs, decreasing = FALSE), ]
      imp = imp[!(row.names(imp) %in% "(Intercept)"), ]
      
      png(filename = paste0(path, as.character(deparse(substitute(data_frame))), 
                            "var_plot_", type, ".png"))
      dotchart(imp$abs, labels = row.names(imp), main = "Feature Importance Ranking Plot for Stepwise LR")
      dev.off()
    } else {
      return("Stepwise linear model (lm) is used for the regression, therefore, it requires a numeric dependent variable")
    }
  }
  return(imp)
}
