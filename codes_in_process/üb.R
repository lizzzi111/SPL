 set.seed(-123)
 bla <- rnorm(n = 100, mean = -4, sd = 1.37)
 bla1 <- rnorm(n = 100, mean = 4, sd = 1.37)
 blan <- rnorm(n = 100, mean = 0, sd = 1) 
 
plot(density(bla*0.2+bla1*0.2+blan*0.6))
p <- bla*0.2+bla1*0.2+blan*0.6
mean(p)
var(p)
psych::describe(p)
e1071::kurtosis(p)+3
e1071::kurtosis(blan, type = 1)
sum((p-mean(p))^3)/var(p)^(3/2)
shapiro.test(p)
ks.test(p)

vark <- sum(sapply(p, function(x_i)  (x_i - mean(p))^2))/(length(p)-1)
schiefe<- (sum(sapply(p, function(x_i)  (x_i - mean(p)))^3)/length(p))/vark^(3/2)
kurt<- (sum(sapply(p, function(x_i)  (x_i - mean(p)))^4)/length(p))/vark^(4/2)
