library(ISLR)
attach(Auto)

names(Auto)

boxplot(mpg~horsepower)
plot(mpg,horsepower)
hist(mpg~horsepower)

jpeg("mpg_horsepower.jpeg")
plot(mpg,horsepower)
dev.off()
