# ----------Ejercicio 1----------------------------------

# cargar el conjunto de datos
datos_cliente <- read.csv("../datos_cliente.csv")

summary(datos_cliente)
describe(datos_cliente)

# Agregar una variable 'churn'
#datos_cliente <- datos_cliente %>%
#  mutate(churn = ifelse(recencia > 90, 1, 0))  # añadir la columna churn y establecer una condición

# Crear variables dummies de la variable 'country'
dummies_m <- dummyVars(~ Country, data = datos_cliente)

# Generar las variables dummy
dummies <- predict(dummies_m, datos_cliente)

# Convertir las variables dummy a un dataframe
dummies_df <- data.frame(UnitedKingdom = dummies[,1])

# Eliminar la columna original 'Country' del dataframe y unirlo con las var dummies
df_glm <- datos_cliente[, !names(datos_cliente) %in% "Country"] %>% # elimina la variable Country
  cbind(dummies_df) %>% # Une el df con las var dummies
  ungroup() %>% # desagrupar el dataframe para poder eliminar CustomerID
  mutate(churn = ifelse(recencia > 90, 1, 0)) %>% # añadir la columna churn y establecer una condición
  select(-fecha_ultima_compra, -CustomerID, -recencia) # Eliminar la variable fecha_ultima_compra, CustomerID y recencia

# Verificar el tipo de cada variable
sapply(df_glm, class)

# Agrupar la variable churn y contar sus valores
df_glm %>% 
  group_by(churn) %>%
  count()  

# Establecer semilla
set.seed(123) # Para reproducibilidad

# Obtener el 70% de datos del total del conjunto de datos de forma aleatoria
train_indices <- sample(1:nrow(df_glm), # Crea un vector que contiene todos los índices de fila del dataframe
                        0.7 * nrow(df_glm)) # Se toma el 70% del total de índices de filas.

# Dividir los datos obtenidos en prueba y entrenamiento
train_data <- df_glm[train_indices, ] # toma los indices generados en el vector train_indices
test_data <- df_glm[-train_indices, ] # toma los indices que no están en el vector train_indices

# Construir el modelo de regresión logistica
modelo_glm <- glm(churn ~ ., # churn es la variable dependiente, (~ .) indica en función de todas las demás variables 
                  data = train_data, # dataset de  donde se van a tomar los datos
                  family = "binomial") # indica regresión logística (distribución binomial) para el modelo.

# Contar cuántos valores son 1 y cuántos son 0 en la variable churn
table(train_data$churn)
summary(modelo_glm)

# Predecir probabilidades en el conjunto de prueba
predictions_glm <- predict(modelo_glm, # Modelo que se va a usar para hacer la predicción
                           newdata = test_data, # dataset que se va a usar, pero no usa la variable dependiente
                           type = "response")

predictions_glm

# Generar un umbral (threshold)
threshold <- 0.5

# Convertir las probabilidades generadas en la predicción del modelo a valores 1 o 0
predicted_class <- ifelse(predictions_glm > threshold, 1, 0)

# Generar la matriz de confusión usando la función confusionMatrix()
conf_mtrx <- confusionMatrix(factor(predicted_class),
                             factor(test_data$churn))

conf_mtrx

# ------------------------------------------------------------------------------------------
# Modelo con variables estandarizadas
# Estandarizar las variables predictoras (excepto churn)
df_glm_scaled <- df_glm
#[, -ncol(df_glm)]: selecciona todas las columnas excepto la última. y las estandariza
df_glm_scaled[, -ncol(df_glm)] <- scale(df_glm[, -ncol(df_glm)])  # churn es la última columna

# Establecer semilla
set.seed(123) # Para reproducibilidad

# Obtener el 70% de datos del total del conjunto de datos de forma aleatoria
train_indices_scaled <- sample(1:nrow(df_glm_scaled), # Crea un vector que contiene todos los índices de fila del dataframe
                        0.7 * nrow(df_glm_scaled)) # Se toma el 70% del total de índices de filas.

# Dividir los datos obtenidos en prueba y entrenamiento
train_data_scaled <- df_glm_scaled[train_indices_scaled, ] # toma los indices generados en el vector train_indices
test_data_scaled <- df_glm_scaled[-train_indices_scaled, ] # toma los indices que no están en el vector train_indices

# Reentrenar el modelo con los datos estandarizados
modelo_glm_scaled <- glm(churn ~ ., # churn es la variable dependiente, (~ .) indica en función de todas las demás variables 
                  data = train_data_scaled, # dataset de  donde se van a tomar los datos
                  family = "binomial") # indica regresión logística (distribución binomial) para el modelo.

# Contar cuántos valores son 1 y cuántos son 0 en la variable churn
table(train_data_scaled$churn)
summary(modelo_glm_scaled)

# Predecir probabilidades en el conjunto de prueba
predictions_glm_scaled <- predict(modelo_glm_scaled, # Modelo que se va a usar para hacer la predicción
                           newdata = test_data_scaled, # dataset que se va a usar, pero no usa la variable dependiente
                           type = "response")

predictions_glm_scaled

# Generar un umbral (threshold)
threshold <- 0.5

# Convertir las probabilidades generadas en la predicción del modelo a valores 1 o 0
predicted_class_scaled <- ifelse(predictions_glm_scaled > threshold, 1, 0)

# Generar la matriz de confusión usando la función confusionMatrix()
conf_mtrx_scaled <- confusionMatrix(factor(predicted_class_scaled),
                             factor(test_data_scaled$churn),
                             positive = "1")

conf_mtrx_scaled

# ----
# Modelo SVM
# Se tomará el dataset df_glm para adecuarlo
df_svm <- df_glm 

# Eliminar la variable 'United Kingdom'
df_svm <- subset(df_svm, select = -UnitedKingdom)

# Convertir la variable churn a factor
df_svm$churn <- as.factor(df_svm$churn)
  
# verificar el tipo de las variables
is.factor(df_svm$churn)

# Estandarizar las variables numéricas
# Seleccionar las variables numéricas (excluyendo 'churn' y 'UnitedKingdom')
variables_numericas <- c("frecuencia_compra", "total_compra", "compra_minima", "compra_promedio")

#Aplicar estandarización a las variables numéricas
df_svm[variables_numericas] <- scale(df_svm[variables_numericas])

# Dividir en entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(df_svm$churn, p = 0.7, list = FALSE)

train_data_svm <- df_svm[trainIndex, ] # Datos para entrenamiento
test_data_svm <- df_svm[-trainIndex, ] # Datos para prueba

# Crear el modelo SVM
modelo_svm <- svm(churn ~ ., 
                  data = train_data_svm, 
                  probability = TRUE)

# Generar Predicciones con el modelo entrenado
predicciones_svm <- predict(modelo_svm, 
                            test_data_svm,
                            probability = TRUE)

# Agregar las predicciones al df de test
test_data_svm$predic_prob_svm <- predicciones_svm

# Comparar las predicciones con los valores reales
table(test_data_svm$churn,
      as.numeric(as.character(test_data_svm$predic_prob_svm)) > 0.5)

# Evaluación de rendimiento
matriz_confusion <- confusionMatrix(predicciones_svm, 
                                    test_data_svm$churn, 
                                    positive = "1")
print(matriz_confusion)

# -------------Ejercicio 2--------------
# Realiza una validación cruzada de 10 particiones utilizando el modelo de máquina de soporte vectorial 
# para evaluar su rendimiento en diferentes subconjuntos de datos. 

#library(caret) 
#library(e1071)

# datos para cross-validation

#variables_numericas <- c("frecuencia_compra", "total_compra", "compra_minima", "compra_promedio")

#Aplicar estandarización a las variables numéricas
df_num <- scale(df_glm[variables_numericas])

# seleccionar variables binarias
df_cat <- df_glm[, !names(df_glm) %in% variables_numericas]

# Generar el df para cross-validation
df_cv <- cbind(df_num, df_cat)

# Configura la validación cruzada de 10 particiones (10-fold cross-validation):
train_control <- trainControl(method = "cv",
                               number = 10)

# Entrenar el modelo SVM con validación cruzada
modelo_svm_cv <- train(churn ~ .,
                       data = df_cv,
                       method = "svmLinear",
                       trControl = train_control)

modelo_svm_cv

# Realiza una validación cruzada de 10 particiones con el modelo de regresión logística. 
modelo_logit_cv <- train(churn ~.,
                         data = df_cv,
                         method = 'glm',
                         family = 'binomial',
                         trControl = train_control)

modelo_logit_cv


# Entrena el modelo con los datos de entrenamiento y evalúa su rendimiento en los datos 
# de prueba utilizando la técnica de validación cruzada k-fold. 
# ------ general--------------------------
# Para reproducibilidad
set.seed(123)  

# Dividir los datos en entrenamiento (80%) y prueba (20%)
trainIndex <- createDataPartition(df_cv$churn, p = 0.8, list = FALSE)
cv_train <- df_cv[trainIndex, ]
cv_test <- df_cv[-trainIndex, ]

# Entrenar el modelo de regresión logística con cross-validation en el conjunto de entrenamiento
cv_logit <- train(churn ~ ., data = cv_train, 
                      method = "glm", 
                      family = "binomial",
                      trControl = train_control)

# Mostrar los resultados del modelo de regresión logística
cv_logit

# Entrenar el modelo SVM con cross-validation en el conjunto de entrenamiento
cv_svm <- train(churn ~ ., data = cv_train, 
                    method = "svmLinear",
                    trControl = train_control)

# Mostrar los resultados del modelo SVM
cv_svm

# Calcula la precisión, sensibilidad y especificidad para cada modelo y cada partición de la validación cruzada. 

# Predecir en el conjunto de prueba con regresión logística
pred_cvlogit <- predict(cv_logit, 
                        newdata = cv_test)

# Establecer un umbral para convertir probabilidades a clases 
threshold <- 0.5 
pred_cvlogit_classes <- ifelse(pred_cvlogit > threshold, 1, 0)

# Asegurarse de que ambas variables son factores con los mismos niveles 
pred_cvlogit_classes <- factor(pred_cvlogit_classes, levels = c(0, 1)) 
cv_test$churn <- factor(cv_test$churn, levels = c(0, 1))

# Evaluar el rendimiento (por ejemplo, con matriz de confusión)
confusionMatrix(pred_cvlogit_classes, 
                cv_test$churn,
                positive = "1")

# Predecir en el conjunto de prueba con SVM
pred_cvsvm <- predict(cv_svm, 
                        newdata = cv_test)

pred_cvsvm_classes <- ifelse(pred_cvsvm > threshold, 1, 0)

# Asegurarse de que ambas variables son factores con los mismos niveles 
pred_cvsvm_classes <- factor(pred_cvsvm_classes, levels = c(0, 1)) 

# Evaluar el rendimiento (por ejemplo, con matriz de confusión)
confusionMatrix(pred_cvsvm_classes, 
                cv_test$churn,
                positive = "1")

# ------Para cada partición----------
# Configuración de 10-fold cross-validation con métricas detalladas
train_control_lr <- trainControl(method = "cv", number = 10, 
                              summaryFunction = twoClassSummary, # Para obtener sensibilidad y especificidad
                              classProbs = TRUE, # Necesario para calcular ROC y métricas
                              savePredictions = "all") # Guarda todas las predicciones para análisis detallado

# Convertir la variable churn a factor
cv_train$churn <- as.factor(cv_train$churn)

# Usar make.names para asegurar que los niveles sean nombres válidos 
levels(cv_train$churn) <- make.names(levels(cv_train$churn))

# Entrenamiento de el modelo de regresión logística con validación cruzada
cv_logit2 <- train(churn ~ ., data = cv_train, 
                      method = "glm", 
                      family = "binomial",
                      trControl = train_control_lr,
                      metric = "Accuracy") # Métrica principal puede ser Accuracy, ROC, etc.

# Obtener métricas de cada fold para regresión logística

resultados_logit <- cv_logit2$resample

print(resultados_logit)

# Configuración de 10-fold cross-validation con métricas detalladas
train_control_svm <- trainControl(method = "cv", number = 10, 
                              summaryFunction = twoClassSummary, # Para obtener sensibilidad y especificidad
                              classProbs = TRUE, # Necesario para calcular ROC y métricas
                              savePredictions = "all") # Guarda todas las predicciones para análisis detallado

# Entrenamiento de un modelo SVM con validación cruzada
cv_svm2 <- train(churn ~ ., data = cv_train, 
                    method = "svmLinear",
                    trControl = train_control_svm,
                    metric = "Accuracy") # Métrica principal puede ser Accuracy, ROC, etc.

# Resultados
print(modelo_svm)

# Obtener métricas de cada fold para SVM

resultados_svm <- cv_svm2$resample

print(resultados_svm)


# Calcula el promedio y la desviación estándar de estas métricas para evaluar la consistencia del rendimiento 
# del modelo en diferentes subconjuntos de datos.

# Calcular promedio y desviación estándar para regresión logística
medidas_logit <- sapply(resultados_logit[, c("ROC","Sens","Spec")], 
                        function(x) c(Media = mean(x), DesvStd = sd(x)))

medidas_logit

# Calcular promedio y desviación estándar para regresión logística

medidas_svm <- sapply(resultados_svm[, c("ROC","Sens","Spec")], 
                        function(x) c(Media = mean(x), DesvStd = sd(x)))

medidas_svm


# -----Ejercicio 3 ------------
#Define una cuadrícula de parámetros para la optimización de hiperparámetros en la regresión logística. 
# Definir la cuadrícula de hiperparámetros 
param_grid <- expand.grid( alpha = c(0, 1), # Puedes ajustar alpha si usas elasticnet: 0 = Ridge, 1 = Lasso 
                           lambda = seq(0.0001, 0.1, 
                                        length = 10) # Valores de lambda para probar )
)

#Crea un objeto trainControl para la validación cruzada de 5 particiones. 
train_control_grid <- trainControl( method = "cv", 
                               number = 5, 
                               summaryFunction = twoClassSummary, 
                               classProbs = TRUE )

#Realiza la optimización de hiperparámetros para la regresión logística utilizando el método "glmnet". Utiliza la cuadrícula de parámetros definida y el objeto trainControl para la validación cruzada. 
# Entrenar el modelo de regresión logística regularizada usando la cuadrícula 
cv_logit_grid <- train( churn ~ ., 
                   data = cv_train, 
                   method = "glmnet", 
                   trControl = train_control_grid, 
                   tuneGrid = param_grid, 
                   metric = "ROC" )

#Recupera e informa los mejores hiperparámetros seleccionados por el proceso de optimización de hiperparámetros
# Recuperar los mejores hiperparámetros
mejores_hiperparametros <- cv_logit_grid$bestTune
print(mejores_hiperparametros)


# -----Ejercicio 4 ------------ 
#Define una cuadrícula de parámetros para la optimización de hiperparámetros en SVM. 
 
# Definir la cuadrícula de parámetros para svm
param_grid_svm <- expand.grid(C = c(0.1, 1, 10, 100), 
                          sigma = c(0.1, 0.01, 0.001, 0.0001))

#Crea otro objeto trainControl para la validación cruzada de 5 particiones específicamente para el modelo SVM. 
# Crear el objeto trainControl para la validación cruzada de 5 particiones 
train_control_svm <- trainControl(method = "cv", 
                                  number = 5)

#Realiza la optimización de hiperparámetros para SVM utilizando el método "svmRadial", la cuadrícula de parámetros y el objeto trainControl. 
# Configurar el modelo SVM usando el objeto trainControl y la cuadrícula de parámetros 
gd_svm <- train(churn ~ ., 
                   data = cv_train, 
                   method = "svmRadial", 
                   trControl = train_control_svm, 
                   tuneGrid = param_grid_svm)

#Recupera e informa los mejores hiperparámetros seleccionados para el modelo SVM.
# Ver los detalles del modelo 
print(gd_svm)

mejores_hiperparametros_svm <- gd_svm$bestTune
print(mejores_hiperparametros_svm)

#---------- Ejercicio 5------------------ 
# Entrena el modelo de regresión logística y el modelo de SVM utilizando los mejores hiperparámetros seleccionados. 
# Configurar los mejores hiperparámetros para glmnet (Regresión logística)
mejores_parametros_logit <- list(alpha = 1, lambda = 1e-04)

#---- Regresión Logística------
# En glmnet, los datos deben estar en formato de matriz para las variables predictoras (x)
x_train <- as.matrix(cv_train[, -which(names(cv_train) == "churn")]) # Variables predictoras
y_train <- cv_train$churn # Variable objetivo

# Se usa gmlnet para incluir regularización Lasso o Ridge (o una combinación de ambas, conocida como elastic net)
hyp_logit <- glmnet(x = x_train, y = y_train, 
                       alpha = mejores_parametros_logit$alpha, 
                       lambda = mejores_parametros_logit$lambda,
                       family = "binomial")

#---- SVM -----
# mejores hiperparámetros de SVM son:
mejores_parametros_svm <- data.frame(sigma = 0.1, C = 100)  

# Entrenar el modelo SVM usando svmRadial con los mejores hiperparámetros
modelo_svm_hyp <- train(churn ~ ., data = cv_train,
                    method = "svmRadial",
                    trControl = trainControl(method = "none"), # Evitar realizar validación cruzada aquí
                    tuneGrid = mejores_parametros_svm)
#----

# Evalúa el rendimiento de ambos modelos en tu conjunto de datos, informando sobre la precisión (accuracy), sensibilidad (sensitivity) y especificidad (specificity). 

#------
# Regresión Logística
# Preparar datos de prueba
x_test <- as.matrix(cv_test[, -which(names(cv_test) == "churn")])

# Obtener predicciones en probabilidad
prob_logit_hyp <- predict(hyp_logit, newx = x_test, type = "response")
predicciones_logit_hyp <- ifelse(prob_logit_hyp > 0.5, 1, 0) # Convertir a etiquetas binarias

# Crear matriz de confusión y extraer métricas
matrix_conf_logit_hyp <- confusionMatrix(as.factor(predicciones_logit_hyp), 
                                         as.factor(cv_test$churn))

# Extraer métricas
accuracy_logit_hyp <- matrix_conf_logit_hyp$overall["Accuracy"]
sensitivity_logit_hyp <- matrix_conf_logit_hyp$byClass["Sensitivity"]
specificity_logit_hyp <- matrix_conf_logit_hyp$byClass["Specificity"]

print("Resultados de Regresión Logística con Regularización:")
print(paste("Precisión:", accuracy_logit_hyp))
print(paste("Sensibilidad:", sensitivity_logit_hyp))
print(paste("Especificidad:", specificity_logit_hyp))

#----
# SVM
# Realiza predicciones en el conjunto de prueba
predicciones_svm_hyp <- predict(modelo_svm_hyp, newdata = cv_test)

# Ajustar los niveles de las predicciones para que coincidan con las etiquetas de referencia
predicciones_svm_hyp <- factor(predicciones_svm_hyp, 
                               levels = c("X0", "X1"), 
                               labels = c("0", "1"))

# Verificar los niveles ajustados
levels(predicciones_svm_hyp)


# Calcula la matriz de confusión para SVM
matrix_conf_svm_hyp <- confusionMatrix(predicciones_svm_hyp, cv_test$churn)

# Extrae las métricas para el modelo SVM
accuracy_svm_hyp <- matrix_conf_svm_hyp$overall["Accuracy"]
sensitivity_svm_hyp <- matrix_conf_svm_hyp$byClass["Sensitivity"]
specificity_svm_hyp <- matrix_conf_svm_hyp$byClass["Specificity"]

# Imprime los resultados para el modelo SVM
print("Resultados de SVM:")
print(paste("Precisión:", accuracy_svm_hyp))
print(paste("Sensibilidad:", sensitivity_svm_hyp))
print(paste("Especificidad:", specificity_svm_hyp))
#----

#	Compara el rendimiento de los modelos ajustados de regresión logística y SVM y saca conclusiones sobre cuál de ellos funciona mejor para el conjunto de datos dado.
# Evaluación final
if (accuracy_logit_hyp > accuracy_svm_hyp) {
  print("La regresión logística tiene mejor precisión.")
} else if (accuracy_logit_hyp < accuracy_svm_hyp) {
  print("SVM tiene mejor precisión.")
} else {
  print("Ambos modelos tienen precisión similar.")
}