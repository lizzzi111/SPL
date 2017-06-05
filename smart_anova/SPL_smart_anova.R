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
