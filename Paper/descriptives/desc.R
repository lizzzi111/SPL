
# distribution of the target variable
print(summary(data))
if ("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2")
}
library(ggplot2)

ggplot(data, aes(x=G3))+geom_bar()

if ("corrplot" %in% rownames(installed.packages()) == FALSE) {
  install.packages("corrplot")
}
library(corrplot)

corrplot(data)
