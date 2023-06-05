##############################
# MODELO POLINÓMICO
##############################

datos1=read.table("acaros.txt", header=TRUE)
attach(datos1)
head(datos1)

# Diagrama de dispersión
windows()
plot(humedad,acaros)


# Polinomio de grao 0 : constante

modelo0=lm(acaros~1)
summary(modelo0)
abline(modelo0,lty=2,lwd=2)

# Polinomio de grado 1: recta de regresión simple


modelo1=lm(acaros~humedad)
summary(modelo1)
abline(modelo1,col="blue",lwd=2)


# Polinomio de grado 2: parábola

humedad2=humedad*humedad
modelo2=lm(acaros~humedad+humedad2)
summary(modelo2)
beta=modelo2$coefficients
curve(beta[1]+beta[2]*x+beta[3]*x^2,add=TRUE,col=2,lwd=2)

# Polinomio de grado 3

modelo3=lm(acaros~humedad+I(humedad^2)+I(humedad^3))
summary(modelo3)

beta=modelo3$coefficients
curve(beta[1]+beta[2]*x+beta[3]*x^2+beta[4]*x^3,add=TRUE,col="green",lwd=2)

##########################
# MODELO CON INTERACCIÓN
##########################

ca<-c(750, 800, 850 ,900 ,950 ,1000 ,1050, 1100 ,1150 ,1200)
d<-c(280 ,300 ,440 ,350 ,470 ,320 ,380 ,450 ,500, 360)
hueso<-c(1.06, 1.08, 1.04 ,1.04, 1.15, 1.04, 1.15 ,1.21 ,1.33, 1.21)

# Regresión lineal múltiple
# sólo con los efectos principales
# y sin la interacción de las dos variables

mod=lm(hueso~ca+d)
summary(mod)

# Ajuste del modelo con interacción

# Vector de la interacción

cad=ca*d
modi<-lm(hueso~ca+d+cad)
summary(modi)
step(modi)
# Sintaxis en R para la interacción sin crear el 
# vector de productos

modi=lm(hueso~ca+d+ca:d)
summary(modi)
step(modi)
# Versión abreviada para la interacción

modi=lm(hueso~ca*d)
summary(modi)
step(modi)

# Modelo no jerárquico

mod_nj=lm(hueso~ca+ca:d)
summary(mod_nj)

#--------------------------------------
#  Diagnosis modelo regresión múltiple
#--------------------------------------

bridge=read.table("bridge.txt",header=TRUE)
head(bridge)
attach(bridge)

#-- Ajuste del modelo de regresión múltiple

mod73=lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
summary(mod73)

#--- Validación del modelo de regresión múltiple

plot(log(Time)~fitted(mod73),xlab="Valores ajustados",ylab="Variable respuesta")

#--- Residuos estandarizados frente a todas las variables
#    explicativas

windows()
par(mfrow=c(2,3))
res_standard=rstandard(mod73)
plot(res_standard~log(DArea),ylab="Residuos estandarizados")
plot(res_standard~log(CCost),ylab="Residuos estandarizados")
plot(res_standard~log(Dwgs),ylab="Residuos estandarizados")
plot(res_standard~log(Length),ylab="Residuos estandarizados")
plot(res_standard~log(Spans),ylab="Residuos estandarizados")
plot(res_standard~fitted(mod73),ylab="Residuos estandarizados",xlab="Valores ajustados")
par(mfrow=c(1,1))

# Análisis de datos atípicos e influyentes

windows()
plot(mod73)


#  Colinealidad - Factores de inflacción

x=cbind(log(DArea),log(CCost),log(Dwgs),log(Length),log(Spans))
colnames(x)=c("log(DArea)","log(CCost)","log(Dwgs)","log(Length)","log(Spans)")
round(cor(x),2)

library(car)
#install.packages("car")
vif(mod73)

# Selección de variables
?step
step(mod73)->mod73_s
summary(mod73_s)

