#' ---
#' title: "Árboles de decisión"
#' output:
#'   bookdown::html_document2:
#'     pandoc_args: ["--number-offset", "1,0"]
#'     toc: yes
#' ---
#'
#'
## ----setup, echo=FALSE-----------------------------------------
accuracy <- function(pred, obs, na.rm = FALSE,
                     tol = sqrt(.Machine$double.eps)) {
  err <- obs - pred     # Errores
  if(na.rm) {
    is.a <- !is.na(err)
    err <- err[is.a]
    obs <- obs[is.a]
  }
  perr <- 100*err/pmax(obs, tol)  # Errores porcentuales
  return(c(
    me = mean(err),           # Error medio
    rmse = sqrt(mean(err^2)), # Raíz del error cuadrático medio
    mae = mean(abs(err)),     # Error absoluto medio
    mpe = mean(perr),         # Error porcentual medio
    mape = mean(abs(perr)),   # Error porcentual absoluto medio
    r.squared = 1 - sum(err^2)/sum((obs - mean(obs))^2)
  ))
}

#'
#'
#'
#'
#' # Ejercicios del Capítulo 2
#'
#' En este documento se incluyen soluciones a los ejercicios propuestos a lo
#' largo del Capítulo 2.
#'
#' Se emplean las siguientes librerías:
#'
#'
## --------------------------------------------------------------
library(rpart)
library(rpart.plot)

#'
#'
#' Se emplea el mismo conjunto de datos considerado a lo largo del Capítulo 2,
#' `winequality.RData`, que contiene información físico-química y sensorial
#' de una muestra de 1250 vinos portugueses.
#' Como respuesta se considera la variable `quality`,  mediana de las evaluaciones
#' de los catadores entre 0 (muy malo) y 10 (muy excelente).
#'
#'
## --------------------------------------------------------------
load("./data/winequality.RData")
str(winequality)
# barplot(table(winequality$quality))

#' Como base se podría considerar el siguiente código incial:
#'
## ---- eval=FALSE-----------------------------------------------
## ntest <- 10
## test <- winequality[1:ntest, ]
## df <- winequality[-(1:ntest), ]
## nobs <- nrow(df)


#' **Solución**
#'
#' Se reservan $n_{test}=10$ observaciones como muestra test.
#' Para el resto de datos ($n - n_{test}$) se selecciona al azar un 80\% para
#' la muestra de entrenamiento, se estima el árbo, se poda (con una selección
#' óptima del parámetro de complejidad) y se predice en la muestra de test.
#' Estos 3 pasos se repiten con 5 semillas diferentes en el siguiente código.
#'
## --------------------------------------------------------------
ntest <- 10
test <- winequality[1:ntest, ]
df <- winequality[-(1:ntest), ]
nobs <- nrow(df)

# Semillas y opciones
semillas <- 1:5
nsemillas <- length(semillas)
prop.train <- 0.8
cp.inicial <- 0
# Predicciones
pred <- matrix(NA, nrow = ntest, ncol = nsemillas)
colnames(pred) <- paste0("pred", 1:nsemillas)


# Bucle semillas
for (i in 1:nsemillas){
  set.seed(semillas[i])
  itrain <- sample(nobs, prop.train * nobs)
  train <- df[itrain, ]
  # Ajuste árbol completo
  tree <- rpart(quality ~ ., data = train, cp = cp.inicial)
  # Selección hiperparámetro por VC
  xerror <- tree$cptable[,"xerror"]
  imin.xerror <- which.min(xerror) # Indice valor óptimo
  # Límite superior "oneSE rule"
  upper.xerror <- xerror[imin.xerror] + tree$cptable[imin.xerror, "xstd"]
  # Complejidad mínima por debajo del límite
  icp <- min(which(xerror <= upper.xerror))
  # CP seleccionado
  cp <- tree$cptable[icp, "CP"]
  # Modelo final y predicción
  tree <- prune(tree, cp = cp)
  pred[, i] <- predict(tree, newdata = test)
}


#' Comparamos las predicciones obtenidas con las observaciones
#'
## --------------------------------------------------------------
obs <- test$quality
res <- data.frame(obs, pred)
round(res, 2)
psych::pairs.panels(res, ellipses = FALSE)
err <- apply(pred, 2, function(x) accuracy(x, obs))
round(err, 2)

#' Se observan bastantes diferencias, ¿posible valor atípico en la muestra test?
#'
## --------------------------------------------------------------
prob <- c(.7, .15, .15)
datos <- winequality
set.seed(1)
nobs <- nrow(datos)
itrain <- sample(nobs, prob[1] * nobs)
inotrain <- setdiff(seq_len(nobs), itrain)
ivalidate <- sample(inotrain, prob[2] * nobs)
itest <- setdiff(inotrain, ivalidate)
train <- datos[itrain, ]
validate <- datos[ivalidate, ]
test <- datos[itest, ]

#' Rejilla con los valores de los hiperparámetros (complejidad)
#'
## --------------------------------------------------------------
cps <- seq(0.001, 0.1, len = 10)
# Sería mejor en escala logarítmica:
cps <- 10^seq(log10(0.001), log10(0.1), len = 10)

#'
## --------------------------------------------------------------
tree <- rpart(quality ~ ., data = train, cp = 0) # cp = min(cps)
cps <- tree$cptable[, 1]

#'
#' Ojo ahora la complejidad es ascendente
#' Para buscar el óptimo la recomendación sería emplear un bucle:
#'
## --------------------------------------------------------------
ncps <- length(cps)
mse <- mse.sd <- numeric(ncps)
# Podríamos guardar resultados adicionales, p.e. los árboles
# pruned.tree <- vector(mode = "list", length = ncps)
# names(pruned.tree) <- cps

for(i in 1:ncps){ # i <- 1
  pruned.tree <- prune(tree, cp = cps[i])
  res <- validate$quality - predict(pruned.tree, newdata = validate)
  se <- res^2
  mse[i] <- mean(se)
  mse.sd[i] <- sd(se)/sqrt(length(se))
}

#' Selección del valor óptimo de cp
#'
## --------------------------------------------------------------
plot(cps, mse,  ylim = c(0.5, .9), xlab = "cp (inverso complejidad)")
# Valor óptimo
imin.mse <- which.min(mse)
cp.op <- cps[imin.mse]
points(cp.op, mse[imin.mse], pch = 16,col=2)

#' Selección del valor óptimo de cp usando el criterio de 1 error estándar
#'
## --------------------------------------------------------------
plot(cps, mse, ylim = c(0.5, .9), xlab = "cp (inverso complejidad)")
segments(cps, mse - mse.sd, cps, mse + mse.sd)
upper.mse <- mse[imin.mse] + mse.sd[imin.mse]
abline(h = upper.mse, lty = 2)
imin.1se <- min(which(mse <= upper.mse))
cp.1se <- cps[imin.1se]
points(cp.1se, mse[imin.1se], pch = 16,col=2)

#' Se ajusta de nuevo el modelo con los datos de entrenamiento y validación
#'
## --------------------------------------------------------------
tree1 <- rpart(quality ~ ., data = datos[-itest], cp = 0)
pruned.tree1 <- prune(tree1, cp = cp.op)
pruned.tree1se <- prune(tree1, cp = cp.1se)
pred1 <- predict(pruned.tree1, newdata = test)
pred1se <- predict(pruned.tree1se, newdata = test)
accuracy(test$quality, pred1)
accuracy(test$quality, pred1se)

#' Una alternativa a particionar en entrenamiento y validación sería emplear bootstrap.
#' La idea es emplear una remuestra bootstrap del conjunto de datos de entrenamiento
#' para ajustar el modelo y utilizar las observaciones no seleccionadas
#'  (se suelen denominar datos *out of bag*) como conjunto de validación.
#'
## --------------------------------------------------------------
set.seed(1)
nobs <- nrow(winequality)
itrain <- sample(nobs, 0.8 * nobs)
train <- winequality[itrain, ]
test <- winequality[-itrain, ]

# Indice muestra de entrenamiento bootstrap
set.seed(1)
ntrain <- nrow(train)
itrain.boot <- sample(ntrain, replace = TRUE)
train.boot <- train[itrain.boot, ]

#'
#' El resto sería igual que el caso anterior cambiando `train` por `train.boot`
#'  y `validate` por `oob`.
#'
#' Como comentario final, lo recomendable sería repetir el proceso un número
#' grande de veces y promediar los errores, especialmente cuando el tamaño
#' muestral es pequeño. Esto está relacionado con el método de *bagging*
#' descrito en el siguiente capítulo.
#'
#' La muestra bootstrap va a contener muchas observaciones repetidas y habrá
#' observaciones no seleccionadas.
#' La probabilidad de que una observación no sea seleccionada es
#'  $(1 - 1/n)^n \approx e^{-1} \approx 0.37$.
#'
## --------------------------------------------------------------
# Número de casos "out of bag"
ntrain - length(unique(itrain.boot))

# Muestra "out of bag"
# oob <- train[-unique(itrain.boot), ]
oob <- train[-itrain.boot, ]

#'
#
#'
#'
#' Por último, para el caso de múltiples réplicas bootstrap se podría considerar
#' repetir los pasos anteriores B veces (núm. de réplicas bootstrap) mediante un `for`.
#' Lo ideal sería estudiar la convergencia para asegurarse de que el número de
#' réplicas bootstrap son suficientes (por ejemplo se podría hace un gráfico de
#' convergencia para el cp óptimo).
#'
