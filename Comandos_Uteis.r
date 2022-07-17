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

help(sqrt) # help(comando) para saber mais sobre algum comando
shell("cls") # Comando para Limpar o cmd
q() #Comando para Sair do R

# Atribuindo Valores
x <- 5
0.54 -> y
z = 2 #não recomendável
assign("a",6)

# Vetores
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

sum(x) # Soma todos os valores do vetor
prod(x) # Multiplica todos os valores do vetor
max(x) # Pega o maior número do vetor
min(x) # Pega o menor Número do Vetor

# vetor de "a" até "z"
seq(from= a, to= z)
#vetor de "a" até "z" com passo "n"
seq(from= a, to= z, by= n )
#vetor de "a" até "z" com "n" elementos
seq(from= a, to= z, length.out= n)

# Matrizes
matriz <- matrix(valor, num_linha, num_coluna) # matrix(data = NA, nrow = x, ncol = y, byrow = TRUE | T | F | FALSE)
rbind(vetor1, vetor2, ..., vetorN)
cbind(vetor1, vetor2, ..., vetorN)
m <- matrix(x,2,5)

# Operações Matemáticas
a=2; b=3; c=5 
a*b
b-c
a+c
b^a

# Operações Lógicas
a==b
b<c
c!=a

# GRÁFICOSS

a<-c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(a)<-c("a","b","c","d","e","f")
pie(a,col = c("red","blue","green","gray", "brown", "black")) #Gráfico de Pizza

#--------------------------------------------------------------------------------------------------------------
# --------------------------------------- ESTATÍSCA BÁSICA ---------------------------------------------------- 
#--------------------------------------------------------------------------------------------------------------

x <- c(10, 14, 13, 15, 16, 18, 12)
mean(x) # Média Aritimética

median(x) # Mediana

# Moda
y <- c(7,8,9,10,10,10,11,12)
table(y)
subset(table(y),table(y)==max(table(y)))

choose(n,k) # Combinatória

factorial(x) #Fatorial

# Média Ponderada
notas <- c(4, 2, 8)
pesos <- c(1,1,2)
sum((notas*pesos))/sum(pesos)


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
