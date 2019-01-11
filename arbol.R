# Vídeo https://www.youtube.com/watch?v=mgh4KdbYHv0
# Algoritmos de clasificación supervisada
# Árboles de decisión
# Árboles de regresión variable dependiente cuantitativa
# Árboles de clasificación variable dependiente cualitativa

set.seed(1546)

library(C50)  # para la base de datos
library(rpart)  # para el árbol de decisión
library(rpart.plot) # para graficar nuestro árbol de decisión


# Preparamos nuestro conjunto de datos
data(churn)
churn <- rbind(churnTest, churnTrain) # unimos los dos dataframe por filas con rbind
rm(churnTest, churnTrain) # borramos churnTest y churnTrain ya que están en churn, paso omitible 
churn <- churn[,c(4, 7, 8, 16, 19, 17, 20)]
names(churn) <- c("Tiene plan internacional", "Minutos/día", "Llamadas/día", "Minutos internacionales", "Reclamaciones", "Llamadas internacionales", "Cancelación" )

# Creamos el conjunto de entreamiento y de test
ind <- sample(2, nrow(churn), replace=T, prob=c(0.6, 0.4)) # 60% para el entrenamiento, 40% para el test
trainData <- churn[ind == 1, ]
testData <- churn[ind == 2, ]

# Creamos el árbol de decisión
arbolrpart <- rpart(Cancelación ~ ., method = "class", data = trainData)

# Imprimiendo nuestro árbol de decisión
print(arbolrpart)
rpart.plot(arbolrpart, extra=4, main="Árbol sin podar") # extra = 4: probabilidad de observaciones por clase

printcp(arbolrpart)
# plotcp(arbolrpart)  # lo ejecutaremos ahora pero primero el texto de abajo

# Podando el árbol ¿Por qué podar el árbol?
plotcp(arbolrpart)

# esta función busca el mínimo error con which.min en la columna 'xerror' del 'arbolrpart$cptable'
# con el ', "CP"' que hay después te devuelve el valor que esta al mismo nivel pero en la otra columna
pArbolRpart <- prune(arbolrpart, cp = arbolrpart$cptable[which.min(arbolrpart$cptable[,"xerror"]), "CP"])
# pArbolRpart <- prune(arbolrpart, cp = 0.01138211) # es equivalente pero tienes que buscar el xerror más
# pequeño que interese en printcp(arbolrpart)

# vemos como queda el arbol podado
printcp(pArbolRpart)
rpart.plot(pArbolRpart, extra=4, main="Árbol podado") # extra = 4: probabilidad de observaciones por clase

# Predecir cancelaciones en testData
testPredRpart <- predict(arbolrpart, newdata = testData, type = "class")

table(testPredRpart, testData$Cancelación)
# ¿Cómo se lee? vamos leyendo por columnas: testPredRpart dictaminó sí en 168 caso que eran sí y 39 casos que eran no....

# Calculamos el % de aciertos
(sum(testPredRpart == testData$Cancelación)) / length(testData$Cancelación) * 100
