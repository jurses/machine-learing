# Algoritmos de clasificación supervisada
# Árboles de decisión
# Árboles de regresión variable dependiente cuantitativa
# Árboles de clasificación variable dependiente cualitativa

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
rpart.plot(arbolrpart, extra=4) # extra = 4: probabilidad de observaciones por clase

printcp(arbolrpart)
plotcp(arbolrpart)