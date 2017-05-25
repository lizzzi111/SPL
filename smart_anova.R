# function smart anova, x - numeric, y - categorical variable separating data into groups
# if distributions within groups are normal, and variances are homogen - apply anova
# if one of the criteria is not fullfilled - apply kruskal wallis test
# Idea: check whether group significantly differ from each other
# in pattern recognition, if they do - the feature is good
smart_anova <- function(test_data){
  norm_t <- tapply(test_data$x, test_data$y, shapiro.test)
  n <-   unlist(lapply(norm_t, function(x) x$p.value ))
  hom <- bartlett.test(test_data$x,test_data$y)
  
  if(any(n<.05)|hom$p.value<.05){
    test <- kruskal.test(test_data$x,test_data$y)$p.value
    names(test) <- "KW"
    return(test)
  } else {
    test <- aov(x~y, test_data)
    p_value <- summary(test)[[1]]$'Pr(>F)'[1]
    names(p_value) <-  "ANOVA"
    return(p_value)
  }
}