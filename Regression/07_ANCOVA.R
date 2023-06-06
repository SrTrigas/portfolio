# Analisis de la Covarianza
#--------------------------
library(MASS)
data("whiteside")
head(whiteside)
attach(whiteside)

# Diagram de dispersión

plot(Temp,Gas,type="n",xlan="temeperature",ylab="Consumo de gas")
ib =(Insul=="Before");ib #sin aislamiento
ia =(Insul=="After")#con aislamiento

points(Temp[ib],Gas[ib])
points(Temp[ia],Gas[ia],pch=18)

# Ajuste del modelo con efectos principales
# interacción

mod_i_t=lm(Gas~Insul+Temp)
summary(mod_i_t)

# Representar ajuste, rectas ins interacción

beta=coef(mod_i_t);beta

abline(a=beta["(Intercept)"],b=beta["Temp"],col = "blue")
abline(a=beta["(Intercept)"]+beta["InsulAfter"],b=beta["Temp"],col = "red")

#Contraste de los efectos

# Efecto variable explicativa continua:Temp

mod_i=lm(Gas~Insul)
mod_i_t=lm(Gas~Insul+Temp)
anova(mod_i,mod_i_t)

# Efecto variable explicativa factor: Insul

mod_t=lm(Gas~Temp)
anova(mod_t,mod_i_t)

#Modelo con interacción:
mod_i_t_it=lm(Gas~Insul+Temp+Insul:Temp)#mod_i_t_it=lm(Gas~Insul*Temp) Es equivalente
summary(mod_i_t_it)


beta = coef(mod_i_t_it)
abline(a=beta["(Intercept)"],b=beta["Temp"],col = "green")
abline(a=beta["(Intercept)"]+beta["InsulAfter"],b=beta["Temp"]+beta["InsulAfter:Temp"],col = "green")

# Chequear modelo con interacción
# Equiv a ajustar so rectar de reg.lineal simple
  plot(Temp,Gas,type="n",xlan="temeperature",ylab="Consumo de gas")
  ib =(Insul=="Before");ib #sin aislamiento
  ia =(Insul=="After")#con aislamiento
  
  points(Temp[ib],Gas[ib])
  points(Temp[ia],Gas[ia],pch=18)
#Grupo sin aislamiento: ib
mod_t_ib=lm(Gas[ib]~Temp[ib])
summary(mod_t_ib)
abline(mod_t_ib,col=2)

#Grupo sin aislamiento: ia
mod_t_ia=lm(Gas[ia]~Temp[ia])
summary(mod_t_ia)
abline(mod_t_ia,col=2)

#Contraste de la interacción:

anova(mod_t_ib,mod_t_ia)
# p-valor muy pequeño, rechazamos H0, por tanto pendientes son diferentes


#-----------------------------#
# Análisis de la covarianza #
#-----------------------------#
#--- Se cargan los datos

library(faraway)
data(sexab)
attach(sexab)
head(sexab)

class(csa)

#--- Diagrama de dispersi\'on

plot(cpa,ptsd,type="n",xlab="abuso físico infantil",ylab="estrés")
ib=(csa=="Abused")
ia=(csa=="NotAbused")
points(cpa[ib],ptsd[ib])
points(cpa[ia],ptsd[ia],pch=18)

#--- Modelo con efectos principales, sin interacción

mod_0=lm(ptsd~csa+cpa)
summary(mod_0)

#--- Representa las dos rectas paralelas del modelo sin interacción

beta=coef(mod_0)
abline(a=beta["(Intercept)"],b=beta["cpa"],col="blue")
abline(a=beta["(Intercept)"]+beta["csaNotAbused"],b=beta["cpa"],col="blue")
#--- Contraste de los efectos
mod_1=lm(ptsd~csa)
mod_2=lm(ptsd~cpa)

anova(mod_1,mod_0)
anova(mod_2,mod_0)

#--- Modelo con interacción
 
mod_0i=lm(ptsd~csa*cpa)
summary(mod_0i)

#--- Representa las dos rectas del modelo con interacción

plot(cpa,ptsd,type="n",xlab="abuso físico infantil",ylab="estrés")
ib=(csa=="Abused")
ia=(csa=="NotAbused")
points(cpa[ib],ptsd[ib])
points(cpa[ia],ptsd[ia],pch=18)
beta=coef(mod_0i)
abline(a=beta["(Intercept)"],b=beta["cpa"],col="pink",lwd=2)
abline(a=beta["(Intercept)"]+beta["csaNotAbused"],
b=beta["cpa"]+beta["csaNotAbused:cpa"],col="pink",lwd=2)

#--- Comprobaci\'on


abline(lm(ptsd[ib]~cpa[ib]),col="green") 
abline(lm(ptsd[ia]~cpa[ia]),col="green")

#--- Contraste de la interacción
anova(mod_0,mod_0i)
