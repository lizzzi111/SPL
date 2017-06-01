# load Data
setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/")
#Testing assumptions of a regression
data("swiss")

# test whether the residuals are distributed normally, it can be because of nonlinear relationship between variables
source("./codes_in_process/get_str_var.R")
source("./codes_in_process/heteroscedasticity_test.R")
get_strange_var(swiss)

library(ggplot2)
ggplot(swiss, aes(Catholic))+geom_histogram()

glm(Infant.Mortality~., )
# function for transformation in order to get rid of illinearity
cor(swiss)
#test on heteroscedasticity
hetero_test(swiss$Examination, swiss[, !(names(swiss)%in%"Examination")])

# check 
# transform the variable 
swiss$Catholic = log(swiss$Catholic)
