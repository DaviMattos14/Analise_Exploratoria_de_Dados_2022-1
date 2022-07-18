library(ISLR)
attach(Auto)
names(Auto)

plot(acceleration, horsepower)

powlims <- range(acceleration)
pow.grid <-seq(from=powlims[1],to=powlims[2])

plot(acceleration,horsepower,xlim=powlims,cex=.5)
fit1 <- loess(horsepower~acceleration,span=.1,degree=1,family="symmetric")
lines(pow.grid,predict(fit1,data.frame(acceleration=pow.grid)),col="red",lwd=2)
fit2 <- loess(horsepower~acceleration,span=.1,degree=1,family="gaussian")
lines(pow.grid,predict(fit2,data.frame(acceleration=pow.grid)),col="#fffb00",lwd=2)

plot(acceleration,horsepower,xlim=powlims,cex=.5)
fit3 <- loess(horsepower~acceleration,span=.1,degree=2,family="symmetric")
lines(pow.grid,predict(fit3,data.frame(acceleration=pow.grid)),col="#09ff00",lwd=2)
fit4 <- loess(horsepower~acceleration,span=.1,degree=2,family="gaussian")
lines(pow.grid,predict(fit4,data.frame(acceleration=pow.grid)),col="#00ffff",lwd=2)

plot(acceleration,horsepower,xlim=powlims,cex=.5)
fit5 <- loess(horsepower~acceleration,span=.3,degree=1,family="symmetric")
lines(pow.grid,predict(fit5,data.frame(acceleration=pow.grid)),col="#001aff",lwd=2)
fit6 <- loess(horsepower~acceleration,span=.3,degree=1,family="gaussian")
lines(pow.grid,predict(fit6,data.frame(acceleration=pow.grid)),col="#ff00ff",lwd=2)

plot(acceleration,horsepower,xlim=powlims,cex=.5)
fit7 <- loess(horsepower~acceleration,span=.3,degvree=2,family="symmetric")
lines(pow.grid,predict(fit7,data.frame(acceleration=pow.grid)),col="#00ffb3",lwd=2)
fit8 <- loess(horsepower~acceleration,span=.3,degree=2,family="gaussian")
lines(pow.grid,predict(fit8,data.frame(acceleration=pow.grid)),col="#eeff00",lwd=2)

plot(acceleration,horsepower,xlim=powlims,cex=.5)
fit9 <- loess(horsepower~acceleration,span=.5,degree=1,family="symmetric")
lines(pow.grid,predict(fit9,data.frame(acceleration=pow.grid)),col="#003cff",lwd=2)
#________________________________________________________________________________________
# Visualmente a mais suave
fit10 <- loess(horsepower~acceleration,span=.5,degree=1,family="gaussian")
lines(pow.grid,predict(fit10,data.frame(acceleration=pow.grid)),col="#ff00d4",lwd=2)
#________________________________________________________________________________________
plot(acceleration,horsepower,xlim=powlims,cex=.5)
fit11 <- loess(horsepower~acceleration,span=.5,degree=2,family="symmetric")
lines(pow.grid,predict(fit11,data.frame(acceleration=pow.grid)),col="#ff00aa",lwd=2)
fit12 <- loess(horsepower~acceleration,span=.5,degree=2,family="gaussian")
lines(pow.grid,predict(fit12,data.frame(acceleration=pow.grid)),col="#ff0000",lwd=2)


jpeg("grafico_q4_05_1_g.jpeg")
plot(acceleration, horsepower)

powlims <- range(acceleration)
pow.grid <-seq(from=powlims[1],to=powlims[2])

fit10 <- loess(horsepower~acceleration,span=.5,degree=1,family="gaussian")
lines(pow.grid,predict(fit10,data.frame(acceleration=pow.grid)),col="red",lwd=2)

fit5 <- loess(horsepower~acceleration,span=.3,degree=1,family="symmetric")
lines(pow.grid,predict(fit5,data.frame(acceleration=pow.grid)),col="#001aff",lwd=2)

legend("topright",legend=c("degree 1; gaussian; span 0.5","degree 1; symmetric; span 0.3"),col=c("red","blue"),lty=1,lwd=2,cex=1.2)

dev.off()
