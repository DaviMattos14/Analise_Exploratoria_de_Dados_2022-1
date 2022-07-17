library(ISLR)
attach(Wage)

#---------------------------------------------
# Transformando variável para melhor trabalho
#---------------------------------------------
id <- numeric(3000)
for (i in 1:3000) { 
    if (age[i] < 35) {id[i] = 1} 
    else {
        if (age[i] < 50) { id[i] = 2 } else {id[i] = 3}
    }
}

grau_educ <- numeric(3000)
for(i in 1:3000){
    if(education[i]=="1. < HS Grad") {grau_educ[i]=1}
    if(education[i]=="2. HS Grad") {grau_educ[i]=2}
    if(education[i]=="3. Some College") {grau_educ[i]=3}
    if(education[i]=="4. College Grad") {grau_educ[i]=4}
    if(education[i]=="5. Advanced Degree") {grau_educ[i]=5}
}

bb <- 10*grau_educ + id
bb

wage2 <- data.frame(wage,bb)
xx <- split(wage2, wage2$bb)

mediana <- matrix(0, 5, 3)
for (grau in 1:5) {
   for (idade in 1:3) {
        aux <- as.character(10*grau + idade)   
        mediana[grau,idade] <- xx$aux
   }
}

for(i in 1:15){
    aux1 <- xx[i]
    aux2 <- log(median((aux1$wage)))
}


b11 <- xx$'11'
m11 <- log(median(b11$wage))

cells <- c(73.77574, 81.28325, 94.07271, 104.92151, 118.88436, 86.69515, 95.23071, 109.83399, 127.11574, 148.41316, 86.68249, 98.59934, 112.64397, 123.08970, 136.94859)
rnames <- c("Ate34","35a49","50+")
cnames <- c("1","2","3","4","5")
tabela_wage <- matrix(cells,3,5,byrow = TRUE,dimnames=list(rnames,cnames))
tabela_wage
