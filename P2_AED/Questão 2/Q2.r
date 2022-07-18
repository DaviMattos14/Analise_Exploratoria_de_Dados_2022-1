cells <- c(73.77574, 81.28325, 94.07271, 104.92151, 118.88436, 86.69515, 95.23071, 109.83399, 127.11574, 148.41316, 86.68249, 98.59934, 112.64397, 123.08970, 136.94859)

novos_valores <- c()
for(i in 1:length(cells)){
    novos_valores <- c(novos_valores, log(cells[i]))
}

rnames <- c("Ate34","35a49","50+")
cnames <- c("1","2","3","4","5")
tabela_wage <- matrix(novos_valores,3,5,byrow = TRUE,dimnames=list(rnames,cnames))
tabela_wage

d <- dim(tabela_wage)

yhat <- c()
delta <- matrix(0,3,5)
for(j in 1:5){
    delta[,j] <- tabela_wage[,j] - median(tabela_wage[,j]) # matriz delta
    yhat <- c(yhat, median(tabela_wage[,j])) # yhat
}

centr <- median(yhat) #Valor central

rowef <- c()
for(i in 1:3){
    rowef <- c(rowef, median(delta[i,])) # efeito de linha
}

colef <- numeric(5)
for (j in 1:5) {
   colef[j] <- yhat[j] - centr # efeito de coluna
}

res <- matrix(0,3,5) #Erro
for(i in 1:3){
    res[i,] <- delta[i,] - median(delta[i,])
}

comparacao_valores <- matrix(0,3,5)
for(i in 1:3){
    for(j in 1:5){
        comparacao_valores[i,j] <- (rowef[i]*colef[j])/centr
    }
}

cv<-numeric(length(rnames)*length(cnames))
sr<-numeric(length(rnames)*length(cnames))
for (i in 1:length(rnames)) {
   for (j in 1:length(cnames)) {
      cv[(i-1)*5+j] <- comparacao_valores[i,j]
      sr[(i-1)*5+j] <- res[i,j]
   }
}

ls.fit=lm(sr~cv)
ls.fit

incl=sr/cv
incli=na.omit(incl)
median(incli)

x <- c(-1,-1,1,1)
y <- c(0,0.5,0.5,1)

jpeg("grafico_wage_age.jpeg")
plot(x,y)

abline(4.778151,1)
abline(4.528275,1)
abline(4.309985,1)
abline(4.017729,1)
abline(3.823909,1)

abline(4.778151,-1)
abline(5.221849,-1)
abline(5.061061,-1)
dev.off()
