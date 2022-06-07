library(ISLR)
attach(Wage)
names(Wage)
dim(Wage)
head(Wage)

boxplot(wage~education)

simetr <- function(lambda){
    if(lambda == 0){tr <- log(wage)} else{tr <- ((wage^lambda)-1)/lambda}
    q1 <- quantile(tr,0.25)
    q2 <- quantile(tr,0.5)
    q3 <- quantile(tr,0.75)
    return(abs(q2-0.5*(q1+q3))/q2)
}

lambda <- c(-2,-1,-0.5,0,0.5,1,2)

valorTr <- 999
lbd <- 0

for(num in lambda){
    print(num)
    print(simetr(num))
}

lbd


boxplot(log(wage)~education)

df_Wage <- data.frame(education=rep(education), wage=rep(wage))
filtro1 <- subset(df_Wage, education == "2. HS Grad")
summary(filtro1)

DIQ <- 109.83 - 77.95
inf <- 77.95 - 1.5 * DIQ
sup <- 109.83 + 1.5 * DIQ
sup
inf

filtro2 <- subset(filtro1, wage > sup)
filtro3
filtro3 <- subset(Wage, wage < inf & education == "2. HS Grad")
filtro4 <- subset(Wage, wage > sup & education == "2. HS Grad")

View(filtro4)
View(filtro3)
jpeg("Wage_Education.jpeg")
boxplot(log(wage)~education,
col = "#7f7fdb"
)
dev.off()


