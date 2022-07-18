n <- 1400
estr <- numeric(n) 
e <- rnorm(n, 0, 0.1)
x <- numeric(n)
y <- numeric(n)

for (i in 1:n) {
    x[i] <- i/100
    estr[i] <- exp(-0.2 * x[i]) * sin(x[i])
    y[i] <- estr[i] + e[i]
}

df <- data.frame(x,estr,e,y)

plot(x,y)

powlims <- range(x)
pow.grid <-seq(from=powlims[1],to=powlims[2])

plot(x,y,xlim=powlims,cex=.5)
curve(exp(-0.2*x)*sin(x),add=T,col="blue",lwd=4)
title("Local Regression")
fit1=loess(y~x,span=.1,degree=2,family="gaussian")
lines(pow.grid,predict(fit1,data.frame(x=pow.grid)),col="red",lwd=2)
legend("bottomleft",legend=c("degree 2; symmetric; span 0.3","true curve"),col=c("red","#0400ff"),lty=1,lwd=2,cex=1.2)


# Criando Matriz para comparação

ncol <- c("span", "degree", "family", "error")
nlin <- seq(1, 12)
menor_erro <- matrix(0, nrow = 12, ncol = 4, byrow = TRUE, dimnames = list(nlin,ncol))
span1 <- c(0.1,0.3,0.5)
grau <- c(1,2)
familia <- c("gaussian", "symmetric")
verd <- numeric(100) 
linha <- 1

for (i in 1:2){
   for (k in 1:3) {
      for (j in 1:2){
        fit1 <- loess(y~x,span=span1[k],degree=grau[j],family=familia[i])
        prec <- 0
        for (m in 1:140) {
            verd[m] <- exp(-0.2*(m/10)) * sin(m/10)
            prec <- prec + ((predict(fit1,(m/10)) - verd[m])^2)
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
