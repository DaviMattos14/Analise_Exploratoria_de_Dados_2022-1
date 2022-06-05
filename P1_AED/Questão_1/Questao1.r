library(ISLR)
attach(College)

names(College)
head(College)
dim(College)

#Criando a imagendo do BoxPlot
jpeg("S.F.Ratio.jpeg")
boxplot(S.F.Ratio, main = "Número de Estudantes por Professor",
xlab = "Professor",
ylab = "Estudantes",
col = "#7f7fdb"
)
dev.off()

#Transformando a variável
simetr <- function(lambda){
    tr <- ((S.F.Ratio^lambda)-1)/lambda
    q1 <- quantile(tr,0.25)
    q2 <- quantile(tr,0.5)
    q3 <- quantile(tr,0.75)
    return(abs(q2-0.5*(q1+q3))/q2)
}
aux <- c(-2,-1,-0.5,0.5,1,2)
for (num in aux) {
    print(num)
    print(simetr(num))
}
teste <- 999
lbd <- 0
for(lambda in aux){
    if (simetr(lambda) < teste) {
       teste <- simetr(lambda)
       lbd <- lambda
    }
}
print(lbd)
print(teste)
boxplot(((S.F.Ratio^lbd)-1)/lbd)
boxplot(S.F.Ratio)

# Analisando os Dados e Calculando os Cercas e Diferença Interquartil
sort(S.F.Ratio)
summary(S.F.Ratio)
quantile(S.F.Ratio)

DIQ <- 16.5-11.5
inf <- 11.5- 1.5 * DIQ
sup <- 16.5 + 1.5 * DIQ

S.F.Ratio

