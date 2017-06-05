rm(list = ls())
graphics.off()
# Alcohol consumption and final grades
setwd("/Users/lizzzi111/Documents/Master/SoSe17/SPL/SPL/")
library(plyr)
# this example of descriptive data will be done on the set with mathematics however exactly
# the same code can be used for portuguese or their mix
data = readRDS("./data/student-mat_fact.rds")
data$Dalc <- as.factor(data$Dalc)
data$Dalc <- mapvalues(data$Dalc, from = 1:5, to = c("Very Low", "Low", "Medium", "High", "Very High"))

data$Walc <- as.factor(data$Walc)
data$Walc <- mapvalues(data$Walc, from = 1:5, to = c("Very Low", "Low", "Medium", "High", "Very High"))

# simple barplot
custom_graph = function(variable, type = "histogram", prop = FALSE) {
    if (prop == TRUE) {
        if (type == "histogram") {
            plot = hist(prop.table(table(variable)), main = paste0("Relative frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "bar") {
            plot = barplot(prop.table(table(variable)), main = paste0("Relative frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "pie") {
            plot = pie(prop.table(table(variable)), main = paste0("Relative frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "mosaicplot") {
            plot = mosaicplot(prop.table(table(variable)), main = paste0("Relative frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "spineplot") {
            plot = spineplot(prop.table(table(variable)), main = paste0("Relative frequency of the variable ", 
                deparse(substitute(variable))))
        }
    } else {
        if (type == "barplot") {
            plot = barplot(table(variable), main = paste0("Absolute frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "pie") {
            plot = pie(table(variable), main = paste0("Absolute frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "mosaicplot") {
            plot = mosaicplot(table(variable), main = paste0("Absolute frequency of the variable ", 
                deparse(substitute(variable))))
        }
        if (type == "spineplot") {
            plot = spineplot(table(variable), main = paste0("Absolute frequency of the variable ", 
                deparse(substitute(variable))))
        }
    }
}

#### Visualizations using the custom graph funtion some basic data visualizations
custom_graph(data$Walc)
custom_graph(data$Dalc, prop = TRUE)
custom_graph(data$G3, prop = TRUE)
custom_graph(data$G2, prop = TRUE)
custom_graph(data$G1, prop = TRUE)

par(mfrow = c(1, 3))
custom_graph(data$G3, prop = TRUE)
custom_graph(data$G2, prop = TRUE)
custom_graph(data$G1, prop = TRUE)
hist(data$G1)

graphics.off()
# Interactive visualizations (without custom graph) alcohol consumption week
library(plotly)
plot_ly(data, x = ~Walc, type = "histogram", marker = list(line = list(color = "white", width = 1))) %>% 
    layout(title = "Alcohol Consumption during the Week")
print("Percent of students in each drinking level:")
table(data$Walc)/nrow(data)

# weekend
plot_ly(data, x = ~Dalc, type = "histogram", marker = list(line = list(color = "white", width = 1))) %>% 
    layout(title = "Alcohol Consumption during the Weekend")
print("Percent of students in each drinking level:")
table(data$Dalc)/nrow(data)

# Visualizations alcohol and grades boxplots alcohol, grade within one term
plot_ly(data, x = ~Dalc, y = ~G3, type = "box", color = data$Dalc, colors = "Set1")
plot_ly(data, x = ~Walc, y = ~G3, type = "box", color = data$Walc, colors = "Set1")

plot_ly(data, x = ~Dalc, y = ~G2, type = "box", color = data$Dalc, colors = "Set1")
plot_ly(data, x = ~Walc, y = ~G2, type = "box", color = data$Walc, colors = "Set1")

plot_ly(data, x = ~Dalc, y = ~G1, type = "box", color = data$Dalc, colors = "Set1")
plot_ly(data, x = ~Walc, y = ~G1, type = "box", color = data$Walc, colors = "Set1")


# Three way visualization To make it better to understand we will group G3 into 'didn't pass'
# 'good' 'perfect'
data$G3_grouped = factor(ifelse(data$G3 > 17.5, "excellent", ifelse(data$G3 > 15.5, "very_good", 
    ifelse(data$G3 > 13.5, "good", ifelse(data$G3 > 9.5, "sufficient", ifelse(data$G3 > 3.5, 
        "week", "poor"))))))
library(ggplot2)
ggplot(data, aes(x = Dalc, y = Walc, col = G3_grouped, group = G3_grouped)) + stat_summary(fun.data = mean_cl_boot, 
    geom = "errorbar", width = 0.1, position = position_dodge(0.2)) + stat_summary(fun.data = mean_cl_boot, 
    geom = "point", size = 3, position = position_dodge(0.2)) + stat_summary(fun.data = mean_cl_boot, 
    geom = "line", position = position_dodge(0.2))

# we can see that means within each alcohol consumption level is quite similar smart_anova
# function smart anova, x - numeric, y - categorical variable separating data into groups if
# distributions within groups are normal, and variances are homogen - apply anova if one of
# the criteria is not fullfilled - apply kruskal wallis test Idea: check whether group
# significantly differ from each other in pattern recognition, if they do - the feature is
# good
smart_anova <- function(x, y, alpha = 0.05) {
    norm_t <- tapply(x, y, shapiro.test)
    n <- unlist(lapply(norm_t, function(x) x$p.value))
    hom <- bartlett.test(x, y)
    
    if (any(n < 0.05) | hom$p.value < alpha) {
        test <- kruskal.test(x, y)$p.value
        names(test) <- "KW"
        cat("Data does not follow normal distribution. Kruskall-Wallis Test is applied\n")
        
        if (test < alpha) {
            cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is rejected: groups are significantly different from each other.\n")
        } else {
            cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is not rejected: groups are not significantly different from each other.\n")
        }
        cat("The according P-value is:", round(test, digits = 2), sep = " ")
    } else {
        test <- aov(x ~ y)
        p_value <- summary(test)[[1]]$"Pr(>F)"[1]
        names(p_value) <- "ANOVA"
        
        cat("Data follows normal distribution. ANOVA is applied\n")
        
        if (test < alpha) {
            cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is rejected: groups are significantly different from each other.\n")
        } else {
            cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is not rejected: groups are not significantly different from each other.\n")
        }
        cat("The according P-value is:", round(p_value, digits = 2), sep = " ")
    }
}
smart_anova(data$G3, data$Dalc)
smart_anova(data$G3, data$Walc)

smart_anova(data$G2, data$Dalc)
smart_anova(data$G2, data$Walc)

smart_anova(data$G1, data$Dalc)
smart_anova(data$G1, data$Walc)

# therefore we can see that there is no significance diffirence or influence of the Alcohol
# on the grades we could also test, whether the level of alcohol drunk during the week and
# weekend are correlated which we could also assume
tab = data.frame(table(data$Dalc, data$Walc))
names(tab) = c("workday", "weekend", "count")
ggplot(tab, aes(x = workday, y = weekend)) + geom_tile(aes(fill = count))

# we test whether two categorical variables are independent or not Input either x,y are the
# vectors of the same length, or y is not given and x contain a data frame of two categorical
# variables the default significance leve is 0.05

cdtest = function(x, y = NULL, alpha = 0.05) {
    if (is.null(x)) {
        l = table(x)
    } else {
        l = table(x, y)
    }
    
    if (any(l < 5)) {
        tryCatch({
            res = fisher.test(l)
            cat("Data have less than 5 observations in at least one of the sells in the contingency table,
           Chi-Square test cannot be applied, accortingly, the Fisher exact test is used.\n")
            if (res$p.value < alpha) {
                cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is rejected: variables are dependent.\n")
            } else {
                cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is not rejected: variables are independent.\n")
            }
            cat("The according P-value is:", round(res$p.value, digits = 2), sep = " ")
            return(c(res$p.value))
        }, error = function(x) {
            res = fisher.test(l, simulate.p.value = TRUE)
            cat("Data have less than 5 observations in at least one of the sells in the contingency table,
          Chi-Square test cannot be applied. Moreover, contigency table is too large for a Fisher exact test.\n Accortingly, the Monte Carlo Simulation is used.\n")
            if (res$p.value < alpha) {
                cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is rejected: variables are dependent.\n")
            } else {
                cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is not rejected: variables are independent.\n")
            }
            cat("The according P-value is:", round(res$p.value, digits = 2), sep = " ")
            return(c(res$p.value))
        })
    } else {
        res = chisq.test(l)
        cat("The Chi-Square test is applied.\n")
        
        if (res$p.value < alpha) {
            cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is rejected: variables are independent.\n")
        } else {
            cat("Used level of significance is", paste0(alpha, "."), "The Zero hypothesis is not rejected: variables are dependent.\n")
        }
        
        cat("The according test statistics, number of degrees of freedom and P-value are:\n", 
            round(res$statistic, digits = 2), round(res$parameter, digits = 2), round(res$p.value, 
                digits = 2), sep = " ")
        return(c(res$statistic, res$parameter, res$p.value))
    }
}
cdtest(data$Walc, data$Dalc)

# as we cann see alcohol consumption during the week and during the weekend depend on each
# other

