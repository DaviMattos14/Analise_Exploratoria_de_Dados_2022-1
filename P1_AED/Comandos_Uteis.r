library(ISLR) #Selecionado da Biblioteca de Dados
attach(College) #Seleciona o Dataset
names(College) # Mostra os nomes das varáveis
head(College) # Mostra as Primeira linhas
dim(College) # Dimensão

stem(S.F.Ratio) #Ramo-Folha
boxplot(S.F.Ratio) #Boxplot
hist(S.F.Ratio) #Histogrma
plot(S.F.Ratio) #Scatterplot
summary(S.F.Ratio)# resumos de várias funções de ajuste de modelo
 
#Criando .jpeg do Gráfico
jpeg("S.F.Ratio.jpeg")
boxplot(S.F.Ratio)
dev.off()

#função de simetrização
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

