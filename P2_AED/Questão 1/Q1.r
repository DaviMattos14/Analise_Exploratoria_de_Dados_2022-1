linhas <- 4
colunas <- 5

mu <- 5

alfa <- c(-2,-1,1,2)
beta <- c(-5,-3,0,3,5)

erro <- rnorm(20,0,0.2)
e <- matrix(erro, linhas,colunas, byrow = TRUE)

y <- matrix(0,linhas,colunas)

for (i in 1:linhas) {
   for (j in 1:colunas) {
      y[i,j] <- exp((mu + alfa[i] + beta[j] + e[i,j]))
   }
}

yhat <- c()
delta <- matrix(0,4,5)
for(j in 1:5){
    delta[,j] <- y[,j] - median(y[,j]) # matriz delta
    yhat <- c(yhat, median(y[,j])) # yhat
}

centr <- median(yhat) #Valor central

rowef <- c()
for(i in 1:4){
    rowef <- c(rowef, median(delta[i,])) # efeito de linha
}

colef <- numeric(5)
for (j in 1:5) {
   colef[j] <- yhat[j] - centr # efeito de coluna
}

res <- matrix(0,4,5) #Erro
for(i in 1:4){
    res[i,] <- delta[i,] - median(delta[i,])
}

comparacao_valores <- matrix(0,4,5)
for(i in 1:4){
    for(j in 1:5){
        comparacao_valores[i,j] <- (rowef[i]*colef[j])/centr
    }
}

cv<-numeric(20)
sr<-numeric(20)
for (i in 1:4) {
   for (j in 1:5) {
      cv[(i-1)*5+j] <- comparacao_valores[i,j]
      sr[(i-1)*5+j] <- res[i,j]
   }
}

ls.fit=lm(sr~cv)
ls.fit

incl=sr/cv
incli=na.omit(incl)
median(incli)
