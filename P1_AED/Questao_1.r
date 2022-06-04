#Criando .jpeg do Gráfico
library(ISLR)
attach(College)
jpeg("boxplot.jpeg")
boxplot(S.F.Ratio)
dev.off()
?jpeg