n <- 1000
estr <- numeric(n) 
e <- rnorm(n, 0, 300)
x <- numeric(n)
y <- numeric(n)

for (i in 1:n) {
    x[i] <- i/10
    estr[i] <- 500 + (-30 * x[i]) + (2 * (x[i])^2) + (-0.02 * (x[i])^3)
    y[i] <- estr[i] + e[i]
}

df <- data.frame(x,estr,e,y)
head(df)
plot(x,y)

powlims <- range(x)
pow.grid <-seq(from=powlims[1],to=powlims[2])

jpeg("a_span_05_degree_2_symmetric.jpeg")

plot(x,y,xlim=powlims,cex=.5)
curve(500 + (-30 * x) + (2 * (x)^2) + (-0.02 * (x)^3),add=T,col="blue",lwd=4)
title("Local Regression")
fit1=loess(y~x,span=.3,degree=2,family="gaussian")
lines(pow.grid,predict(fit1,data.frame(x=pow.grid)),col="red",lwd=2)
legend("bottomleft",legend=c("degree 2; symmetric; span 0.5","true curve"),col=c("red","#2600ff"),lty=1,lwd=2,cex=1.2)

dev.off()
# Criando Matriz para comparação

ncol <- c("span", "degree", "family", "error")
nlin <- seq(1, 12)
menor_erro <- matrix(0, nrow = 12, ncol = 4, byrow = TRUE, dimnames = list(nlin,ncol))
span1 <- c(0.1,0.3,0.5)
grau <- c(1,2)
familia <- c("gaussian", "symmetric")
verd <- numeric(100) 
linha <- 1

for (i in 1:length(familia)){
   for (k in 1:length(span1)) {
      for (j in 1:length(grau)){
        fit1 <- loess(y~x,span=span1[k],degree=grau[j],family=familia[i])
        prec <- 0
        for (m in 1:100) {
            verd[m] <- 500 + (-30 * m) + (2 * (m^2)) + (-0.02 * (m^3))
            prec <- prec + ((predict(fit1,m) - verd[m])^2)
        }
        menor_erro[linha,1] <- span1[k]
        menor_erro[linha,2] <- grau[j]
        menor_erro[linha,3] <- familia[i]
        menor_erro[linha,4] <- prec
        linha <- linha + 1 
      }
   }
}
menor_erro
min(menor_erro[,4]) # Vendo o menor erro

#Criando Tabela para melhor visualização
peso <- menor_erro[,1]
grau <- menor_erro[,2]
familia <- menor_erro[,3]
erro <- menor_erro[,4]
min_err <- data.frame(peso,grau,familia,erro)
min_err
fix(min_err)
