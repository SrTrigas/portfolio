datos=read.table("produccion.txt",header=TRUE)
head(datos)
attach(datos)
plot(RunTime,RunSize,pch=16,cex=1.5)
cov(RunTime,RunSize)

Y=RunTime
x=RunSize

plot(x,Y,pch=16,cex=1.5,xlab="RunSize",ylab="RunTime")

mod=lm(Y~x)
mod
summary(mod)

mod$residuals
names(mod)
mod$coefficients #beta0gorro y beta1gorro
mod$fitted.values #Y gorro
(Y-mod$fitted.values) #residuos


#estimacion de la varianza del error
n=length(Y)
res=mod$residuals
(1/(n-2))*sum(res^2)
#desviacion tipica
sqrt((1/(n-2))*sum(res^2))

#Pintar recta de regresion
plot(x,Y,pch=16,cex=1.5,xlab="RunSize",ylab="RunTime")
abline(mod,col=2,lwd=3)

#pintar los residuos
plot(x,res,xlab="RunSize",ylab="Residuos",cex=1.5,pch=16)
abline(h=0,col=3,lwd=3)


#Inferencia sobre los parámetros
#intervalos
confint(mod,level=0.93)
#contrastes
summary(mod)



#F de Snedecor
xx=seq(0,30,len=1000)
y1=df(xx,df1=1,df2=1)
y2=df(xx,df1=5,df2=7)
y3=df(xx,df1=10,df2=30)
plot(xx,y1,type="l",col=2,lwd=3)
lines(xx,y2,col=3,lwd=3)
lines(xx,y3,col=4,lwd=3)


#Descomposición de la variabilidad
anova(mod)


#Media condicionada
x0=c(60,100,150,200)
predict(mod,newdata=data.frame(x=x0),
        interval="confidence",level=0.95)
#predicción
t=predict(mod,newdata=data.frame(x=x0),
        interval="prediction",level=0.95)



matplot(t,pch=16)
#############################################
#ej1
glu=c(0.2, 0.5, 1, 2, 3)#x
reac=c(8, 10 ,18,35 ,60)#Y
plot(glu,reac,pch=16,cex=1.5)
mod2=lm(reac~glu)
summary(mod2)

n=length(reac)
res=mod2$residuals
(1/(n-2))*sum(res^2)
#desviacion tipica
sqrt((1/(n-2))*sum(res^2))
#pintar modelo
plot(glu,reac,pch=16,cex=1.5)
abline(mod2,col=2,lwd=3)

#pintar residuos
plot(glu,mod2$residuals,pch=16,cex=1.5)
abline(h=0)
#intervalos
confint(mod2,level=0.95)
anova(mod2)

#Media condicionada
x0=c(60,100,150,200)
predict(mod2,newdata=data.frame(glu=x0),
        interval="confidence",level=0.95)
#predicción
t=predict(mod2,newdata=data.frame(glu=x0),
          interval="prediction",level=0.95)

matplot(t,pch=16)
