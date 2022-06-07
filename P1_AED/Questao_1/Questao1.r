library(ISLR)
attach(College)

#Criando a imagendo do BoxPlot
jpeg("S.F.Ratio_Tr.jpeg")
boxplot(round(S.F.Ratio), main = "Número de Estudantes por Professor",
# boxplot(((S.F.Ratio^-2)-1)/(-2))
xlab = "Professor",
ylab = "Estudantes",
yaxp = c(0,40,8),
col = "#7f7fdb"
)
dev.off()

# Analisando os Dados e Calculando os Cercas e Diferença Interquartil
summary(S.F.Ratio)
quantile(S.F.Ratio)
DIQ <- 16.5-11.5
inf <- 11.5- 1.5 * DIQ
sup <- 15.5 + 1.5 * DIQ

#Filtrando o nome da Universidade com S.F.Ratio
df_College <- data.frame(name_College=rep(rownames(College)), ratio=rep(round(S.F.Ratio)))

#df_College <- data.frame(name_College=rep(rownames(College)), ratio=rep(S.F.Ratio))

filtro1 <- subset(College, round(S.F.Ratio) < inf, select=c(S.F.Ratio)) # Outlines Inferiores
filtro2 <- subset(College, round(S.F.Ratio) > sup, select=c(S.F.Ratio)) # Outlines Superiores

filtro3 <- subset(College, S.F.Ratio < 4, select=c(S.F.Ratio))
filtro4 <- subset(College, S.F.Ratio > 24, select=c(S.F.Ratio))

View(df_College)
boxplot(S.F.Ratio)
boxplot(round(S.F.Ratio))
View(filtro1)
View(filtro2)
