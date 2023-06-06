#Coeficiente de determinación
#Ejemplo 1
set.seed(1234)
eps1=rnorm(50,mean=0,sd=0.1)
x=seq(0,1,len=50)
beta0=1
beta1=2
y1=beta0+beta1*x+eps1
plot(x,y1,pch=16)
mod=lm(y1~x)
summary(mod)
abline(mod,col=2,lwd=3)

eps2=rnorm(50,mean=0,sd=0.5)
y2=beta0+beta1*x+eps2
plot(x,y2,pch=16)
mod2=lm(y2~x)
summary(mod2)
abline(mod2,col=2,lwd=3)


eps3=rnorm(50,mean=0,sd=1)
y3=beta0+beta1*x+eps3
plot(x,y3,pch=16)
mod3=lm(y3~x)
summary(mod3)
abline(mod3,col=2,lwd=3)

#Datos de produccion
datos=read.table("produccion.txt",header=TRUE)
head(datos)
attach(datos)
Y=RunTime
x=RunSize
mod=lm(Y~x)
summary(mod)

#Linealidad y homocedasticidad
par(mfrow=c(1,2))
plot(x,Y,pch=16)
abline(mod,col=2,lwd=3)
plot(x,mod$residuals,pch=16,main="Residuos")
abline(h=0,lwd=3)
par(mfrow=c(1,1))
#normalidad
shapiro.test(mod$residuals)#p-valor alto, aceptamos normalidad
ks.test(mod$residuals,"pnorm",mean=0,sd=sd(mod$residuals))
#Lilliefors=Kolmogorov-smirnov para normalidad
qqnorm(mod$residuals)

install.packages("nortest")
library(nortest)
pearson.test(mod$residuals)

#Correlacion(independencia)
install.packages("lmtest")
library(lmtest)
dwtest(Y~x,alternative = "two.sided")

Box.test(mod$residuals,type="Ljung-Box",lag=1)
Box.test(mod$residuals,type="Ljung-Box",lag=2)#p-valor alto, aceptamos correlaci


#Ejercicio 2 (Tema 2)
hor=read.table("hormones.txt",header=TRUE,dec=".")
head(hor)
dim(hor)

#Variable respuesta Y: nuevo test (Test)
#Variable explicativa: tratamiento existente (Reference)
attach(hor)
plot(Reference,Test,pch=16)
plot(Test~Reference,pch=16)

mod=lm(Test~Reference)
summary(mod)

mod2=lm(Test~Reference-1)
summary(mod2)


abline(mod2,col=2,lwd=3)

#Validación del modelo
#Linealidad y homocedasticidad
par(mfrow=c(1,2))
plot(Reference,Test,pch=16)
plot(Reference,mod2$residuals,pch=16)
abline(h=0)
par(mfrow=c(1,1))
#linealidad ok, homocedasticidad dudosa
#Normalidad
shapiro.test(mod2$residuals)
#rechazamos normalidad p-valor bajo
#Independencia
Box.test(mod2$residuals,type="Ljung-Box")#P-v alto--aceptamos independencia
library(lmtest)
dwtest(mod2)


#Transformación logarítmica
xx=seq(0,10,len=1000)
yy=log10(xx)
plot(xx,yy,type="l")
abline(v=1,col=4)
abline(v=0,col=4)

lReference=log(Reference)
lTest=log(Test)
modl=lm(lTest~lReference)
plot(lReference,lTest,pch=16)
abline(modl,col=2)  

madera=read.table("madera.txt",header=TRUE)
head(madera)
dim(madera)

attach(madera)
plot(altitud,produccion,pch=16)
lp=log(produccion)
plot(altitud,lp,pch=16)
modm=lm(lp~altitud)
abline(modm,col=2,lwd=2)
summary(modm)

exp(modm$coefficients)

#raiz cuadrada
xx=seq(0,10,len=1000)
yy=log10(xx)
plot(xx,yy,type="l",ylim=c(-2,10))
yy2=sqrt(xx)
lines(xx,yy2,col=2,lwd=2)


#Transformacion Box-Cox

dat=read.table("responsetransformation.txt",header=TRUE)
head(dat)
dim(dat)
attach(dat)

plot(x,y,pch=16)
lambda=seq(-1,0.7,len=15)
lambda
i=0
rss=numeric(0)
for(l in lambda){
  yt=(y^l-1)/l
  modi=lm(yt~x)
  i=i+1
  rss[i]=sum((modi$residuals)^2) #deviance(modi)
}
lambda
rss
plot(lambda,rss,type="l")

lambda[which.min(rss)]

library(MASS)
aux=boxcox(y~x)
aux

t =aux$x[which.max(aux$y)]
ly = (y^t-1)/t
lx = (x^t-1)/t
modl=lm(ly~x)
summary(modl)
plot(x,ly)
abline(modl)
