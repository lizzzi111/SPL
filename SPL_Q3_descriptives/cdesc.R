# Function checks for zero Variance Predictors, duplicates, highly
# correlated variables.  Moreover, it returnes corelation table,
# summary, pairs of highly corelated predictors for a given threshold
# and variables having variance close to zero

cdesc = function(df, goal_var, searchDuplicates = FALSE, cor_cutoff = 0.9) {
  
  # zero variance predictors: we want to identify and probably drop
  # predictors with small predictive power load customized function
  source("./SPL_Q3_descriptives/variance_check.R")
  suspicious_variables = variance_check(df, goal_var)
  
  # duplicates make sure we don't have duplicates in the data
  dup = duplicated(df)
  num_dup = dup[dup]
  if (length(num_dup) != 0 & searchDuplicates == TRUE) {
    cat("Duplicates were dropped.\n")
  } else if (length(num_dup) == 0 & searchDuplicates == TRUE) {
    cat("No duplicates in data.\n")
  }
  
  # distributions of variables
  summary = summary(df)
  
  # highly correlated predictors? correlation for a given cutoff value
  cor = cor(df[, sapply(df, is.numeric) & !(names(df) %in% goal_var)])
  highCor = abs(cor) >= cor_cutoff
  # we change diagonal elements to false, since corelation of the
  # variable to itself is always 1 and it won't give us any additional
  # information
  diag(highCor) = FALSE
  pairs = apply(highCor, 1, function(u) paste(names(which(u)), collapse = ","))
  pairs[pairs == ""] = NA
  
  
  if (all(sapply(pairs, is.na))) {
    cat("There are no highly correlated predictors.\n")
  } else {
    cat("Highly correlated variables were found.\n")
    print(pairs)
  }
  
  if (length(suspicious_variables) == 0 & all(sapply(pairs, is.na))) {
    output = list(summary = summary, correlation = cor)
  } else if ((length(suspicious_variables) != 0 & all(sapply(pairs, is.na)))) {
    output = list(nearZerovars = suspicious_variables, summary = summary, 
                  correlation = cor)
  } else if (length(suspicious_variables) == 0 & !all(sapply(pairs, is.na))) {
    output = list(summary = summary, correlation = cor, highlyCorPredictors = pairs)
  } else {
    output = list(nearZerovars = suspicious_variables, summary = summary, 
                  correlation = cor, highlyCorPredictors = pairs)
  }
  return(output)
}


# EXAMPLE ON THREE TYPES OF DATA FOR OUR BOTH DATASETS REGRESSION
math_reg = readRDS("./data/student-mat_reg.rds")
math_reg_desc = cdesc(math_reg, "G3", T)

port_reg = readRDS("./data/student-por_reg.rds")
port_reg_desc = cdesc(port_reg, "G3")

# BINARY
math_bin = readRDS("./data/student-mat_binary.rds")
math_bin_deasc = cdesc(math_bin, "G3")

port_bin = readRDS("./data/student-por_binary.rds")
port_bin_desc = cdesc(port_bin, "G3")

# MULTICLASS
math_mul = readRDS("./data/student-mat_multiclass.rds")
math_mul_desc = cdesc(math_mul, "G3")

port_mul = readRDS("./data/student-por_multiclass.rds")
port_mul_desc = cdesc(port_mul, "G3")
