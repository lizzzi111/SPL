#### Regression Diagnostics and Gauss Markov Theorem, only two regressors
setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/data/")
data = readRDS("./student-mat_fact.rds")
# data1 = readRDS('./student-por_fact.rds')

# 1) linearity 2) Strict exogenity 3) Full rank (check multicollinearity) 4) Spherical errors (homoscedasticity)


# 1) linearity
pairs(data[, c("G3", "G2", "G1")])
# relationships between variables look quite linear, however we can see some outliers
data_out = data[data$G2 > 5 & data$G3 > 5 & data$G1 > 5, c("G3", "G2", "G1")]
pairs(data_out)
# looks better - we've losz 22 observations assumption holds

# 2) Strict exogenity
fit = lm(G3 ~ G2 + G1, data)
par(mfrow = c(2, 2))
plot(fit)

fit_out = lm(G3 ~ G2 + G1, data_out)
plot(fit_out)
# 62, 647, 63

shapiro.test(fit_out$residuals/sd(fit_out$residuals))
# gets better but not normal however we need that the mean would be approximately zero
mean(fit$residuals)
mean(fit_out$residuals)
# in both cases mean is approximately zero = the assumption holds

# 3) Full rank (check multicollinearity)
cor(data_out)
# we do not have 1s, assumption holds

# 4) Spherical errors (homoscedasticity)
hetero_test <- function(dep, indep) {
    library(lmtest)
    fit = lm(dep ~ ., indep)
    res_fit = bptest(fit)  #summary(lm(fit$residuals^2~.,indep))
    ifelse(res_fit$p.value > 0.05, return("Assumption of homoscedasticity holds"), return("Assumption of homoscedasticity is rejected"))
}

hetero_test(data_out$G3, as.data.frame(cbind(data_out$G1, data_out$G2)))
hetero_test(data$G3, as.data.frame(cbind(data$G1, data$G2)))
# that looks bad, assumption does not hold, however we can still use linear regression, but we have to keep in mind that
# standard errors are wrong, whereas, coefficients won't change.

summary(fit_out)
summary(fit)
