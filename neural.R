# Este tutorial ha sido sacado de aquí https://datascienceplus.com/fitting-neural-network-in-r/

set.seed(500)
library(MASS)

# conseguimos los datos
# ¿Qué es Boston y qué significa esos parámetros?
#  Valores de la vivienda en los suburbios de Boston
# https://translate.google.com/translate?sl=en&tl=es&u=https%3A%2F%2Fstat.ethz.ch%2FR-manual%2FR-devel%2Flibrary%2FMASS%2Fhtml%2FBoston.html
data <- Boston

# apply(data, 2, function(x) sum(is.na(x)))
# esta función te da por cada columna los valores que NA (not available),
# valores donde no hay valor

# el símbolo ':' genera secuencias regulares
# desde:hasta, es igual que seq_len
# 1:17 -> 1 2 3 4 5 ... 17

# round(valor, aproximacion = a la unidad)
index <- sample(1:nrow(data), round(0.75 * nrow(data)))

train <- data[index,]
test <- data[-index,]

# Ajustamos un modelo de regresión lineal y lo probamos en el conjunto de prueba
# medv es una columna del dataframe Boston
lm.fit <- glm(medv~., data=train)

# función genérica usada para producir resultados resumidos de varios modelos de
# funciones de ajustes.
summary(lm.fit)

pr.lm <- predict(lm.fit, test)

# esto nos dará el error cuadrático medio
# una medida de error para saber como nuestras predicciones
# se alejan de los datos reales
MSE.lm <- sum((pr.lm - test$medv) ^ 2) / nrow(test)

# Antes de ajustar una red neuronal necesitamos cierta preparación antes.
# normalizamos y escalamos nuestros datos
# I chose to use the min-max method and scale the data in the interval [0,1].
# Usually scaling in the intervals [0,1] or [-1,1] tends to give better results.

maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)

# esta función obtiene todos los nombres de las columnas de train_
n <-names(train_)

# esta función produce una cadena de la siguiente manera "medv ~ crim + zn + indus + ... + lstat"
# y luego la guarda como una fórmula
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

# producimos una red neuronal con la anterior fórmula, los datos de train_,
# vector de enteros especificando el número de neuronas por capa oculta,
# linear.output es usada para especificar cuando queremos hacer regresión
# o clasificación
# http://ingenierobeta.com/clasificacion-vs-regresion-machine-learning/
nn <- neuralnet(f, data = train_, hidden = c(5, 3), linear.output = TRUE)

# visualizamos como ha quedado nuestra red neuronal
plot(nn)

#predecir medv usando la red neuronal
# Calcula las salidas de todas las neuronas para vectores de
# covariables arbitrarios específicos con una red neuronal entrenada. 
pr.nn <- compute(nn, test_[, 1:13])

# Acordémonos de que hay que "desescalar" nuestros valores.
pr.nn_ <- pr.nn$net.result * (max(data$medv) - min(data$medv)) + min(data$medv)
test.r <- (test_$medv) * (max(data$medv) - min(data$medv)) + min(data$medv)

# Nuestro error cuadrático medio producido por la red neuronal
MSE.nn <- sum((test.r - pr.nn_) ^ 2) / nrow(test_)

# Comparación del error cuadrático medio
print(paste(MSE.lm, MSE.nn))

par(mfrow = c(1, 2))
plot(test$medv, pr.nn_, col='red', main='Real vs predicted NN', pch=18,cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend='NN', pch=18, col='red', bty='n')
plot(test$medv, pr.lm, col='blue', main='Real vs predicted lm', pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend='LM', pch=18,col='blue', bty='n', cex=.95)

plot(test$medv, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
points(test$medv, pr.lm, col='blue', pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend=c('NN', 'LM'), pch=18, col=c('red', 'blue'))