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
    if (lambda == 0) {tr <- log(S.F.Ratio)} else {tr <- ((S.F.Ratio^lambda)-1)/lambda}
    q1 <- quantile(tr,0.025)
    q2 <- quantile(tr,0.5)
    q3 <- quantile(tr,0.975)
    return(abs(1-(q3-q2)/(q2-q1)))
}

aux <- c(-2,-1,-0.5,0, 0.5,1,2)
for (num in aux) {
    print(num)
    print(simetr(num))
}

bxplt <- function(lambda){
    if (lambda == 0) {tr = log(S.F.Ratio)}
    else { tr = (S.F.Ratio^lambda-1)/lambda }
    boxplot(tr)
}

bxplt(2)

q()

#Transformando a variável
simetr <- function(lambda){
    tr <- ((S.F.Ratio^lambda)-1)/lambda
    q1 <- quantile(tr,0.25)
    q2 <- quantile(tr,0.5)
    q3 <- quantile(tr,0.75)
    return(abs(q2-0.5*(q1+q3))/q2)
}

aux <- c(-2,-1,-0.5,0.5,1,2)

#for (num in aux) {
#    print(num)
#    print(simetr(num))
#}

valorTr <- 999
lbd <- 0
for(lambda in aux){
    if (simetr(lambda) < valorTr) {
       valorTr <- simetr(lambda)
       lbd <- lambda
    }
}
q1_tr <- quantile(valorTr,0.25)
q2_tr <- quantile(valorTr,0.5)
q3_tr <- quantile(valorTr,0.75)
    
DIQ <- q3_tr - q1_tr
inf <- q1_tr - 1.5 * DIQ
sup <- q3_tr + 1.5 * DIQ

boxplot(((S.F.Ratio^-2)-1)/(-2))
sort(((S.F.Ratio^-2)-1)/(-2))
#----------------------------------

boxplot(S.F.Ratio)
boxplot(((S.F.Ratio^-2)-1)/-2)
boxplot(((S.F.Ratio^-1)-1)/-1)
boxplot(((S.F.Ratio^-0.5)-1)/-0.5)
boxplot(((S.F.Ratio^0.5)-1)/0.5)
boxplot(((S.F.Ratio^1)-1)/1)
boxplot(((S.F.Ratio^2)-1)/2)
boxplot(log(S.F.Ratio))
boxplot(S.F.Ratio)

dataframe <- College[0][0]
head(College)
head(S.F.Ratio)
table(College$S.F.Ratio)
head(College)
help(College)
summary(College)
lm(Apps~Private+Accept,data=College)
S.F.Ratio

stuFal = read.table("College")

boxplot(College$S.F.Ratio)

read.csv("College.csv")

fix(College)
rownames(College)
View (College)
college_SFRatio <- table()
df <- data.frame(name_College=rep(rownames(College)),s_f=rep(S.F.Ratio))
boxplot(df)
head(df)
filtro2 <- subset(df_College, ratio > 24)
filtro1 <- subset(df_College, ratio < 4)
# filtro2 <- subset(College, S.F.Ratio > 24, select=c(S.F.Ratio))

outlines <- subset(College, ((S.F.Ratio^-2)-1)/(-2) < inf, select=c(S.F.Ratio))
outlines
y <- 1/(sqrt(inf*(-2)+1))
y
