

setwd("Documents/Master/SoSe17/SPL/student-alcohol-consumption/")

d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

#### Convert to factors (can be a function(data,list of parameters to make factors, which of them ordered(only of it easy to assume which order)))
d1$Medu <- factor(d1$Medu)
d2$Medu <- factor(d2$Medu)


d1$Fedu <- factor(d1$Fedu)
d2$Fedu <- factor(d2$Fedu)

d1$famrel <- factor(d1$famrel, ordered=T)
d2$famrel <- factor(d2$famrel, ordered=T)

d1$freetime <- factor(d1$freetime, ordered=T)
d2$freetime <- factor(d2$freetime, ordered=T)

d1$health <- factor(d1$health, ordered=T)
d2$health <- factor(d2$health, ordered=T)

d1$goout <- factor(d1$goout, ordered=T)
d2$goout <- factor(d2$goout, ordered=T)

d1$Dalc <- factor(d1$Dalc, ordered=T)
d2$Dalc <- factor(d2$Dalc, ordered=T)

d1$Walc <- factor(d1$Walc, ordered=T)
d2$Walc <- factor(d2$Walc, ordered=T)

d1$traveltime <- factor(d1$traveltime, ordered=T)
d2$traveltime <- factor(d2$traveltime, ordered=T)

d1$studytime <- factor(d1$studytime, ordered=T)
d2$studytime <- factor(d2$studytime, ordered=T)
#### one quantlet, encode noinal variables (different style of encodings)



### First we are going to look at the summary
summary(d1)
#Math 349 - Gabriel Pereira or 46 - Mousinho da Silveira
# R 88 U 307
# Pstatus A 41, T 354


library(dplyr)

