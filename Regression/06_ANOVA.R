#Anova ej. Oxigeno del rio
#-----------------------------

#Leemos datos

rio = read.table("oxigeno-del-rio.txt",head=T) 
head(rio)

attach((rio))

# diagram de dispersion

plot(Oxigeno~Lugar)

#Diagram de cajas

plot(Oxigeno~as.factor(Lugar),col=(1:4))

#MOdelo incorrecto:
mod_mal=lm(Oxigeno~Lugar)
summary(mod_mal)

plot(Oxigeno~Lugar,MAIN ="mOD. REGRESION LINEA SIMPLE(INCORRECTO)")
abline(mod_mal)

#Mod Anova de forma correcta

Lugar = factor(Lugar)
mod_bien= lm(Oxigeno~Lugar)
summary(mod_bien)

#medias locales

mu_local = tapply(Oxigeno,Lugar,mean)
mu_local

#Parametrización ....
lm(Oxigeno~Lugar-1)

plot(Oxigeno~as.numeric(Lugar),main ="mOD. REGRESION LINEA SIMPLE(INCORRECTO)")
points(1:4,coef(lm(Oxigeno~Lugar-1)),col=3,pch=19,cex=2)
#Parametrixacion respecto media global y desviaciones

mu_global= mean(Oxigeno);mu
global
alfa= mu_local-mu_global
alfa#El signo nos dice como se comporta la media local respecto a la global

# ¿Como cambiar grupo de referencia?

Lugar_2ref=relevel(Lugar,ref="2")
Lugar_2ref

mod_2ref=lm(Oxigeno~Lugar_2ref)
summary(mod_2ref)

#-------------------------------------------------------------------------------
#Análisi anova
#-------------------------------------------------------------------------------

anova(mod_bien)#Rechazo H0: mu1=mu2=mu3=mu4

#--- Comparaciones múltiples

#Bonferroni:

pairwise.t.test(Oxigeno,Lugar,p.adjust.method = "bonferroni")

#Tukey

TukeyHSD(aov(Oxigeno~Lugar))

#NOrmalidad

shapiro.test(Oxigeno[Lugar=="1"])
shapiro.test(Oxigeno[Lugar=="2"])
shapiro.test(Oxigeno[Lugar=="3"])
shapiro.test(Oxigeno[Lugar=="4"])
#p-valores altos, no rechazamos normalidad

# Homocedasticidad(Levenne)
library(car)
leveneTest(Oxigeno,Lugar,center="mean")
#p-valor grande, por tanto no rechazo H0 igual varian, portanto asumimos igualdad varianzas
detach()
#---------------------------------------------
Ej. Ejemplo TIRE
#--------------------------------------------

library(PASWR2)
datos = TIRE
attach(datos)
head(datos)

class(tire)
plot(stopdist~tire,col=(2:5))
#moedlo Anova
mod = lm(stopdist~tire)
summary(mod)
#Anova
anova(mod)


##-----------------------
# Anova Oxígeno del río
#------------------------


# -- Lectura de los datos

rio=read.table("oxigeno-del-rio.txt",header=TRUE)
head(rio)

attach(rio)

# Diagrama de dispersión de los datos

plot(Oxigeno~Lugar)

class(Lugar)

# Diagrama de cajas

plot(Oxigeno~as.factor(Lugar),col=1:4)

#-- Modelo incorrecto

mod_mal=lm(Oxigeno~Lugar)
summary(mod_mal)

plot(Oxigeno~Lugar,main="Modelo lineal simple (incorrecto)")
abline(mod_mal,col=2)

# Modelo Anova de forma correcta
# Parametrización respecto al grupo 1 de referencia

Lugar=factor(Lugar)
mod_bien=lm(Oxigeno~Lugar)
mod_bien

# Medias locales

mu_local=tapply(Oxigeno,Lugar,mean)
mu_local

# Parametrización con las medias de todos los grupos

lm(Oxigeno~Lugar-1)


plot(Oxigeno~as.numeric(Lugar),main="Modelo lineal simple (incorrecto)")
points(1:4,coef(lm(Oxigeno~Lugar-1)),col=3,pch=19,cex=2)

# Parametrización media global y desviaciones

mu_global=mean(Oxigeno);mu_global
alfa=mu_local-mu_global;alfa

# ¿Como cambiar el grupo de referencia?

Lugar_2ref=relevel(Lugar,ref="2")
Lugar_2ref

mod_2ref=lm(Oxigeno~Lugar_2ref)
summary(mod_2ref)

#--------------------------------
# Análisis del ANOVA 
#-------------------------------

anova(mod_bien)  # Rechazo H0: mu1=mu2=mu3=m4

#--- Comparaciones múltiples

# Bonferroni

pairwise.t.test(Oxigeno,Lugar,p.adj="bonferroni")

# Tukey

TukeyHSD(aov(Oxigeno~Lugar))

# Diagnosis del modelo


#-- Normalidad

shapiro.test(Oxigeno[Lugar=="1"])
shapiro.test(Oxigeno[Lugar=="2"])
shapiro.test(Oxigeno[Lugar=="3"])
shapiro.test(Oxigeno[Lugar=="4"])

#-- Homocedasticidad | Levene

library(car)
leveneTest(Oxigeno,Lugar,center="mean")
