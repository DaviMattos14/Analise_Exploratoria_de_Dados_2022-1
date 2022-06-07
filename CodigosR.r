library(ISLR)
attach(Carseats)
names(Carseats)
head(Carseats)
dim(Carseats)
boxplot(Sales~ShelveLoc)

#----------------------------------

library(ISLR)
attach(Auto)
names(Auto)
head(Auto)
stem(mpg)
stem(mpg, scale=2)
stem(mpg, scale=0.5)
stem(horsepower)
boxplot(mpg)
boxplot(horsepower)
sort(horsepower)
quantile(horsepower)
dim(Auto)
sort(horsepower, decreasing = FALSE)
newdata <- Auto[order(horsepower,decreasing=TRUE),]
head(newdata)
newdata <- mtcars[order(mpg),]
bxpl.fn=function(xlamb)
{
  tr=(horsepower^xlamb-1)/xlamb
  boxplot(tr)
}
bxpl.fn(-2)
bxpl.fn(-1)
bxpl.fn(-0.5)
boxplot(log(horsepower))
bxpl.fn(0.5)
bxpl.fn(1)
bxpl.fn(2)

simetr.fn=function(xlamb)
{
  tr=(horsepower^xlamb-1)/xlamb
  q1=quantile(tr,0.25)
  q2=quantile(tr,0.5)
  q3=quantile(tr,0.75)
  return (abs(q2-0.5*(q1+q3))/q2)
}
simetr.fn(-2)
simetr.fn(-1)
simetr.fn(-0.5)
simetr.fn(0.5)
simetr.fn(1)
simetr.fn(2)

#--------------------------------------
attach(IRISDAT)
dim(IRISDAT)
names(IRISDAT)
head(IRISDAT)
boxplot(SEPALLEN~tipo)
boxplot(SEPALWID~tipo)
boxplot(PETALLEN~tipo)
boxplot(PETALWID~tipo)

bxplt.fn=function(xlamb)
{
  tr=(PETALWID^xlamb-1)/xlamb
  boxplot(tr~tipo)
}
bxplt.fn(-2)
bxplt.fn(-1)
bxplt.fn(-0.5)
boxplot(log(PETALWID)~tipo)
bxplt.fn(0.5)
bxplt.fn(1)
bxplt.fn(2)


X <- split(IRISDAT, X <- IRISDAT$tipo)
X


Seto = X$'1'
Virg = X$'2'
Vers = X$'3'

homog.fn=function(xlamb)
{
  if (xlamb==0) (tr1= log(Seto$PETALWID))
  else (tr1=(Seto$PETALWID^xlamb-1)/xlamb)
  q11=quantile(tr1,0.25)
  q31=quantile(tr1,0.75)
  diq1=q31-q11
  if (xlamb==0) (tr2 = log(Virg$PETALWID))
  else (tr2=(Virg$PETALWID^xlamb-1)/xlamb)
  q12=quantile(tr2,0.25)
  q32=quantile(tr2,0.75)
  diq2=q32-q12
  if (xlamb==0) (tr3 = log(Vers$PETALWID))
  else (tr3=(Vers$PETALWID^xlamb-1)/xlamb)
  q13=quantile(tr3,0.25)
  q33=quantile(tr3,0.75)
  diq3=q33-q13
  return (abs(max(diq1,diq2,diq3)-min(diq1,diq2,diq3))/(diq1+diq2+diq3))
}
i2n=homog.fn(-2)
i1n=homog.fn(-1)
imn=homog.fn(-0.5)
i0=homog.fn(0)
im=homog.fn(0.5)
i1=homog.fn(1)
i2=homog.fn(2)
cl=c(-2,-1,-0.5,0,0.5,1,2)
cind=c(i2n,i1n,imn,i0,im,i1,i2)
plot(cl,cind)

#--------------------------------------------

attach(Idosas)
dim(Idosas)
names(Idosas)
ClIMC=get('Cl IMC')
ClRCQ=get('Cl RCQ')
head(Idosas)
boxplot(IMC~CAT)
bxplt.fn=function(xlamb)
{
  tr=(IMC^xlamb-1)/xlamb
  boxplot(tr~CAT)
}
bxplt.fn(-2)
bxplt.fn(-1)
bxplt.fn(-0.5)
bxplt.fn(0.5)

IMC
sort(IMC)
IMCtrunc=IMC-20

bxplttru.fn=function(xlamb)
{
  tr=(IMCtrunc^xlamb-1)/xlamb
  boxplot(tr~CAT)
}
bxplttru.fn(-2)
bxplttru.fn(-1)
bxplttru.fn(-0.5)
boxplot(log(IMCtrunc)~CAT)
bxplttru.fn(0.5)
bxplttru.fn(1)
bxplttru.fn(2)

dadosaum <- data.frame(Idosas,IMCtrunc)
head(dadosaum)
X <- split(dadosaum, X <- dadosaum$CAT)
X

Ati = X$A
Sed = X$S

homog.fn=function(xlamb)
{
  if (xlamb==0) (tr = log(dadosaum$IMCtrunc)) 
     else (tr=(dadosaum$IMCtrunc^xlamb-1)/xlamb)
  q2=quantile(tr,0.5)
  if (xlamb==0) (trA = log(Ati$IMCtrunc))
     else (trA=(Ati$IMCtrunc^xlamb-1)/xlamb)
  q1A=quantile(trA,0.25)
  q3A=quantile(trA,0.75)
  diqA=q3A-q1A
  if (xlamb==0) (trS = log(Sed$IMCtrunc))
     else (trS=(Sed$IMCtrunc^xlamb-1)/xlamb)
  q1S=quantile(trS,0.25)
  q3S=quantile(trS,0.75)
  diqS=q3S-q1S
  return (abs(diqS-diqA)/q2)
}
homog.fn(-2)
homog.fn(-1)
homog.fn(-0.5)
homog.fn(0)
homog.fn(0.5)
homog.fn(1)
homog.fn(2)

#------------------------------------------------

n = 100
mi1 = 10
sig1 = 1
mi2 = 20
sig2 = 2
mi3 = 30
sig3 = 3
mi4 = 40
sig4 = 4
x1 = rnorm(n,mean=mi1,sd=sig1)
x2 = rnorm(n,mean=mi2,sd=sig2)
x3 = rnorm(n,mean=mi3,sd=sig3)
x4 = rnorm(n,mean=mi4,sd=sig4)


xx = c(x1,x2,x3,x4)
xx

hist(xx)

rot <- rep(c(1,2,3,4), each=n)
rot
dados <- data.frame(rot,xx)
dados
bxplt.fn=function(xlamb)
{
  tr=(xx^xlamb-1)/xlamb
  boxplot(tr~rot)
}
bxplt.fn(-2)
bxplt.fn(-1)
bxplt.fn(-0.5)
boxplot(log(xx)~rot)
bxplt.fn(0.5)
bxplt.fn(1)
bxplt.fn(2)

X <- split(dados, X <- dados$rot)
X

A1 = X$'1'
A2 = X$'2'
A3 = X$'3'
A4 = X$'4'

homog.fn=function(xlamb)
{
  if (xlamb==0) (tr = log(dados$xx)) 
  else (tr=(dados$xx^xlamb-1)/xlamb)
  q2=quantile(tr,0.5)
  if (xlamb==0) (tr1 = log(A1$xx))
  else (tr1=(A1$xx^xlamb-1)/xlamb)
  q11=quantile(tr1,0.25)
  q31=quantile(tr1,0.75)
  diq1=q31-q11
  if (xlamb==0) (tr2 = log(A2$xx))
  else (tr2=(A2$xx^xlamb-1)/xlamb)
  q12=quantile(tr2,0.25)
  q32=quantile(tr2,0.75)
  diq2=q32-q12
  if (xlamb==0) (tr3 = log(A3$xx))
  else (tr3=(A3$xx^xlamb-1)/xlamb)
  q13=quantile(tr3,0.25)
  q33=quantile(tr3,0.75)
  diq3=q33-q13
  if (xlamb==0) (tr4 = log(A4$xx))
  else (tr4=(A4$xx^xlamb-1)/xlamb)
  q14=quantile(tr4,0.25)
  q34=quantile(tr4,0.75)
  diq4=q34-q14
  return((max(diq1,diq2,diq3,diq4)-min(diq1,diq2,diq3,diq4))/(diq1+diq2+diq3+diq4))
}
i2n=homog.fn(-2)
i1n=homog.fn(-1)
imn=homog.fn(-0.5)
i0=homog.fn(0)
im=homog.fn(0.5)
i1=homog.fn(1)
i2=homog.fn(2)
cl=c(-2,-1,-0.5,0,0.5,1,2)
cind=c(i2n,i1n,imn,i0,im,i1,i2)
plot(cl,cind)

#-------------------------------------------------

n = 100
mi1 = 10
sig1 = 1
mi2 = 20
sig2 = 1.33
mi3 = 30
sig3 = 1.67
mi4 = 40
sig4 = 2
x1 = rnorm(n,mean=mi1,sd=sig1)
x2 = rnorm(n,mean=mi2,sd=sig2)
x3 = rnorm(n,mean=mi3,sd=sig3)
x4 = rnorm(n,mean=mi4,sd=sig4)


xx = c(x1,x2,x3,x4)
xx

hist(xx)

rot <- rep(c(1,2,3,4), each=n)
rot
dados <- data.frame(rot,xx)
dados
bxplt.fn=function(xlamb)
{
  tr=(xx^xlamb-1)/xlamb
  boxplot(tr~rot)
}
bxplt.fn(-2)
bxplt.fn(-1)
bxplt.fn(-0.5)
boxplot(log(xx)~rot)
bxplt.fn(0.5)
bxplt.fn(1)
bxplt.fn(2)

X <- split(dados, X <- dados$rot)
X

A1 = X$'1'
A2 = X$'2'
A3 = X$'3'
A4 = X$'4'

homog.fn=function(xlamb)
{
  if (xlamb==0) (tr = log(dados$xx)) 
  else (tr=(dados$xx^xlamb-1)/xlamb)
  q2=quantile(tr,0.5)
  if (xlamb==0) (tr1 = log(A1$xx))
  else (tr1=(A1$xx^xlamb-1)/xlamb)
  q11=quantile(tr1,0.25)
  q31=quantile(tr1,0.75)
  diq1=q31-q11
  if (xlamb==0) (tr2 = log(A2$xx))
  else (tr2=(A2$xx^xlamb-1)/xlamb)
  q12=quantile(tr2,0.25)
  q32=quantile(tr2,0.75)
  diq2=q32-q12
  if (xlamb==0) (tr3 = log(A3$xx))
  else (tr3=(A3$xx^xlamb-1)/xlamb)
  q13=quantile(tr3,0.25)
  q33=quantile(tr3,0.75)
  diq3=q33-q13
  if (xlamb==0) (tr4 = log(A4$xx))
  else (tr4=(A4$xx^xlamb-1)/xlamb)
  q14=quantile(tr4,0.25)
  q34=quantile(tr4,0.75)
  diq4=q34-q14
  return((max(diq1,diq2,diq3,diq4)-min(diq1,diq2,diq3,diq4))/(diq1+diq2+diq3+diq4))
}
i2n=homog.fn(-2)
i1n=homog.fn(-1)
imn=homog.fn(-0.5)
i0=homog.fn(0)
im=homog.fn(0.5)
i1=homog.fn(1)
i2=homog.fn(2)
cl=c(-2,-1,-0.5,0,0.5,1,2)
cind=c(i2n,i1n,imn,i0,im,i1,i2)
plot(cl,cind)

#-------------------------------------------------------------
n = 1000
mi = 10
sig = 2
xx = rnorm(n,mean=mi,sd=sig)
boxplot(xx)
yy=100*(1-1/((xx-4)^2))
yy
boxplot(yy)

bxpl.fn=function(xlamb)
 {
   #xlamb=0
   if (xlamb==0) (tr = log(yy)) else (tr=(yy^xlamb-1)/xlamb)
   boxplot(tr)
 }
bxpl.fn(-2)
bxpl.fn(-1)
bxpl.fn(-0.5)
bxpl.fn(0)
bxpl.fn(0.5)
bxpl.fn(1)
bxpl.fn(2)

simet.fn=function(xlamb)
{
  if (xlamb==0) (tr = log(yy)) else (tr=(yy^xlamb-1)/xlamb)
  q2 = quantile(tr,0.5)
  q025 = quantile(tr,0.025)
  q975 = quantile(tr,0.975)
  return(abs(1-(q975-q2)/(q2-q025)))
  #abs(1-(q975-q2)/(q2-q025))
}
i2n=simet.fn(-2)
i1n=simet.fn(-1)
imn=simet.fn(-0.5)
i0=simet.fn(0)
im=simet.fn(0.5)
i1=simet.fn(1)
i2=simet.fn(2)
cl=c(-2,-1,-0.5,0,0.5,1,2)
cind=c(i2n,i1n,imn,i0,im,i1,i2)
plot(cl,cind)
