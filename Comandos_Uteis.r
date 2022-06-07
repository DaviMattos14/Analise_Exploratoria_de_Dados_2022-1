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
    if(lambda == 0){tr <- log(S.F.Ratio)} else{tr <- ((S.F.Ratio^lambda)-1)/lambda}
    q1 <- quantile(tr,0.25)
    q2 <- quantile(tr,0.5)
    q3 <- quantile(tr,0.75)
    return(abs(q2-0.5*(q1+q3))/q2)
}

simetr2 <- function(lambda){
    if(lambda == 0){tr <- log(S.F.Ratio)} else{tr <- ((S.F.Ratio^lambda)-1)/lambda}
    q1 <- quantile(tr,0.25)
    q2 <- quantile(tr,0.5)
    q3 <- quantile(tr,0.75)
    return(abs((q2-((q1+q3)/2))/q2))
}

lambda <- c(-2,-1,-0.5,0,0.5,1,2)

for(num in lambda){
    print(num)
    print(simetr2(num))
}

valorTr <- log(S.F.Ratio)
lbd <- 0

for(lam in lambda){
    if (simetr(lam) < valorTr) {
       valorTr <- simetr(lam)
       lbd <- lam
    }
}
lbd

q1_tr <- quantile(valorTr,0.25)
q2_tr <- quantile(valorTr,0.5)
q3_tr <- quantile(valorTr,0.75)
    
DIQ <- q3_tr - q1_tr
inf <- q1_tr - 1.5 * DIQ
sup <- q3_tr + 1.5 * DIQ
sup
inf
#----------------------------------

boxplot(College$S.F.Ratio)
fix(College)
rownames(College)
View (College)
df_College <- data.frame(name_College=rep(rownames(College)), ratio=rep(S.F.Ratio))
boxplot(df)
head(df)
filtro2 <- subset(df_College, log(ratio) > sup)
filtro1 <- subset(df_College, log(ratio) < inf)
# filtro2 <- subset(College, S.F.Ratio > 24, select=c(S.F.Ratio))

View(filtro1)


outlines <- subset(College, ((S.F.Ratio^-2)-1)/(-2) < inf, select=c(S.F.Ratio))
outlines
y <- 1/(sqrt(inf*(-2)+1))
y

jpeg("hist_S.F.Ratio.jpeg")
hist(S.F.Ratio, main="Relação Estudantes-Professor")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
hist(S.F.Ratio, add = TRUE, col = "white")
dev.off()

hist(((S.F.Ratio^1)-1)/(1))
hist(log(S.F.Ratio))
hist(S.F.Ratio)
plot(((S.F.Ratio^0.5)-1)/(0.5))
