#Generar datos simulados

set.seed(123456)

n=20              #Tamaño de la muestra
x=1:20;x          #variable explicativa
y=1+0.5*x+rnorm((n));y #Variable respuesta


#Representar conjunto datos originales

plot(y~x,xlab="",ylab="",asp=1)

#ajuste modelo regresion lineal si¡mple

mod=lm(y~x)
summary(mod)
abline(mod,col="blue")
points(mean(x),mean(y),col="blue",pch="X")

windows()
par(mfrow=c(2,2))

xmarco=c(1,30)
ymarco=c(0,16)
# Con un dato más en el centro
x0=mean(x)
y0p=predict(mod,data.frame(x=c(x0)))
names(y0p)=c()
y0=y0p
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_a1=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(a1)",ylab="",asp=1)
points(x,y)
points(x0,y0,col="red",pch=19)
points(mean(x_ampliado),mean(y_ampliado),col="blue",pch="X")
abline(mod_a1,col="red")
summary(mod_a1)

# Con un dato más, sin capacidad de palanca y fuera de la recta
y0=y0p-8
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_a2=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(a2)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="red",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_a2,col="red")
summary(mod_a2)

# Con un dato más, con capacidad de palanca pero dentro de la recta
x0=30
y0p=predict(mod,data.frame(x=c(x0)))
names(y0p)=c()
y0=y0p
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_b1=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(b1)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="red",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_b1,col="red")
summary(mod_b1)

# Con un dato más, con capacidad de palanca y fuera de la recta
y0=y0p-8
x_ampliado=c(x,x0)
y_ampliado=c(y,y0)
mod_b2=lm(y_ampliado~x_ampliado)
plot(xmarco,ymarco,type="n",xlab="(b2)",ylab="",asp=1)
points(x,y)
points(mean(x_ampliado),mean(y_ampliado),col="red",pch="X")
abline(mod,col="blue")
points(x0,y0,col="red",pch=19)
abline(mod_b2,col="red")
summary(mod_b2)
par(mfrow=c(1,1))


