install.packages("faraway")
library(faraway)
data(savings)
head(savings)
?savings

attach(savings)
mod=lm(sr~pop15+pop75+dpi+ddpi,data=savings)
summary(mod)
#eliminar dpi (coeficiente no significativo)
mod2=lm(sr~pop15+pop75+ddpi,data=savings)
summary(mod2)
mod3=lm(sr~pop15+ddpi,data=savings)
summary(mod3)

step(mod)

modf=mod2
summary(modf)

#Si pop15=24, pop75=2.5, ppdi=1.4
28.1247 + (-0.4518)*24 + (-1.8354)*2.5 + 0.4278*1.4
modf$coefficients

sum(coef(modf)*c(1,24,2.5,1.4))

Y=savings$sr

X=model.matrix(modf)
XtX=t(X)%*%X
XtXi=solve(XtX)
hbeta=XtXi%*%t(X)%*%Y #estimacion de los coeficientes
hbeta

#RSS
sum((modf$residuals)^2)

RSS=deviance(modf)

#estimacion de la varianza del error
n=dim(savings)[1];n
p=4
RSS/(n-p)


#Interpretacion de los parametros
datos=read.table("Greene.txt",header=TRUE,sep="",dec=".")
head(datos)
dim(datos)

real=datos
real$Año=1:15
real$PNB=(datos$PNB/datos$IPC)*100
real$IN=(datos$IN/datos$IPC)*100
real$IPC[1]=(datos$IPC[1]/79.06-1)*100 #79.06 es en IPC en 1967
real$IPC[2:15]=(datos$IPC[2:15]/datos$IPC[1:14]-1)*100

head(real)


#regresion lineal simple inversion sobre año
mods=lm(IN~Año,data=real)
summary(mods)

#regresion lineal mulitple de inversion sobre el resto de variables
modm=lm(IN~.,data=real) #lm(IN~Año+PNB+IP+TI,data=real)
summary(modm)


#regresion particionada
mod1=lm(IN~PNB+IPC+TI,data=real)
summary(mod1)
res1=residuals(mod1) #mod1$residuals
res1
mod2=lm(Año~PNB+IPC+TI,data=real)
summary(mod2)
res2=mod2$residuals
res2
m=lm(res1~res2)
summary(m)

#coeficiente lineal multiple
modm
cor(real)#matriz correlaciones(simples)
cor(real$IN,fitted(modm))#coeficiente de correlación múltiple

cor(real$IN,fitted(modm))^2 #coeficiente de determinacion de modm

#coeficiente de correlacion parcial de IN y Año dadas PNB, IPC y TI
cor(res1,res2)


#Intervalos de confianza para beta
ICbeta=confint(modm,level=0.95)
linf=ICbeta[,1]
lsup=ICbeta[,2]

beta=coef(modm)


install.packages("ellipse")
library(ellipse)
plot(ellipse(modm,c(2,3)),type="l",main="Region de confianza",
     xlim=c(-22,0),ylim=c(0,1))
points(beta[2],beta[3],col=4,pch=16,cex=1.2)
points(0,0,col=2,pch=16,cex=1.2)

plot(ellipse(modm,c(4,5)),type="l",main="")
points(beta[4],beta[5],col=4,pch=16,cex=1.3)
points(0,0,col=2,pch=16,cex=1.3)
#pintar los intervalos de confianza individuales
abline(v=linf[4],col=2,lwd=2)
abline(v=lsup[4],col=2,lwd=2)
abline(h=linf[5],col=2,lwd=2)
abline(h=lsup[5],col=2,lwd=2)


#Contraste para comparacion de modelos
mod1=lm(IN~Año+PNB,data=real)
mod2=modm
anova(mod1,mod2)

#prediccion
predict(modm) #para los individuos de la muestra
head(real)

predict(modm,interval="confidence")
predict(modm,interval="prediction")

predict(modm,data.frame(Año=12,PNB=1200,IPC=5,TI=6)) #para un nuevo valor

predict(modm,data.frame(Año=12,PNB=1200,IPC=5,TI=6),interval="confidence") #con IC media condicionada

predict(modm,data.frame(Año=12,PNB=1200,IPC=5,TI=6),interval="prediction") #con IC para una observacion cualquiera

#Ejercicio propuesto
install.packages("datarium")
library(datarium)
head(marketing)
?marketing
#modelo de regresión lineal simple de sales sobre facebook
#ajuste mod, intervalos confianza coeficientes, alguna predicción
#valideis mod

#modelo de regresion lineal multiple de sales frente a las otras 3 varibles
#chequear significacion coeficientes
#hacer algun test-F
#hacer alguna prediccion
#pintar region confianza para dos coef (los que querais) conjuntamente

