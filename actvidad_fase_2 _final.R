
# ********** Ejercicio 1 ***********************

# cargar el conjunto de datos base (original)
datos <- read.csv("Appendix 1 - Database.csv")

# Ver el resumen
summary(datos)

# Convertir InvoiceDate de tipo 'chr' a 'date-time'
#install.packages("tidyverse")
#library(tidyverse)
# activar dplyr en packages para usar pipe(%>%)
#install.packages("lubridate")
#library(lubridate)

# Convertir la variable'InvoiceDate' a tipo POSIXct
# Convierte una fecha y hora en un objeto de tipo POSIXct, que es básicamente un número 
# (segundos desde el "epoch", es decir, desde el 1 de enero de 1970).
datos <- datos %>%
  mutate(InvoiceDate = mdy_hm(InvoiceDate))

summary(datos)

# Eliminar filas de datos negativos de las variables UnitPrice y Quantity
# y los valores na de la variable CustomerID
# Crear un nuevo dataframe con el filtro aplicado
datos_limpios <- datos %>% 
  filter(Quantity > 0, UnitPrice > 0, !is.na(CustomerID))

# Extraer fecha de la última compra en general
fecha_compra_max = max(datos_limpios$InvoiceDate)

# Convertir la variable Country a factor, y agruparla en niveles, dejando los tres más altos y los demás en una categoría llamada 'others'
# iibrary (forcats)
datos_limpios$Country <- fct_lump(datos_limpios$Country, 
                                    n = 3) # Se establece la cantidad de categorías

summary(datos_limpios)

# Crear dataset a nivel cliente
datos_cliente <- datos_limpios %>%
  group_by(CustomerID, Country) %>% # Agrupar por número de id del clientey país
  summarise( # Crea columnas
    fecha_ultima_compra = max(InvoiceDate), # fecha ult compra de cada cliente
    recencia = as.integer(fecha_compra_max - max(InvoiceDate)), # cantidad de días desde la última compra general
    frecuencia_compra = n_distinct(InvoiceNo), # cantidad de veces que compró cada cliente (ID)
    total_compra = sum(Quantity * UnitPrice), # total de las compras en dinero  
    compra_minima = min(Quantity * UnitPrice), # minimo de compras en dinero
    compra_promedio = mean(Quantity * UnitPrice), # promedio de compras en dinero
  ) %>%
  arrange(desc(total_compra)) # Ordena de mayor a menor la variable total_compra

# Para utilizar describe()
#install.packages("psych")
#library("psych")

summary(datos_cliente)
describe(datos_cliente)

# Gráficas de visualización
# Crear diagrama de distribución (histograma) de días desde última compra
ggplot(datos_cliente, aes(x = recencia)) +
  geom_histogram(binwidth = 10, fill = 'green', color = 'blue', alpha = 0.7)+
  labs(title = 'Distribución días desde la última compra',
       x = 'No. de días desde la última compra',
       y = 'Frecuencia') +
  theme_light()

# Cantidad de compras por país Gráfico de barras
ggplot(datos_cliente, aes(x = Country))+
  geom_bar(fill = "blue")+
  labs(title = "Cantidad de compras por país",
       x = "país",
       y = "No. de compras")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1))

# frecuencia del total de compras
ggplot(datos_cliente, aes(x = total_compra)) +
  geom_histogram(binwidth = 10, fill = 'green', color = 'blue', alpha = 0.7)+
  labs(title = 'Distribución valores totales de compra por cliente',
       x = 'Total compra',
       y = 'Frecuencia') +
  theme_light()

# Gráfica de frecuencia de recencia 
plot(datos_cliente$recencia, datos_cliente$frecuencia_compra,
     main = "Resencia / Frecuencia compra",
     xlab = "recencia",
     ylab = "Frecuencia compra")


# ********* Ejercicio 2 *********************
#library(caret) # contiene la función dummyVars
#library(dplyr) # librería de data munging
#library(funModeling) # df_status function
#install.packages("caret")

# Ver el resumen del estado de las variables
status = df_status(datos_cliente, print_results = F) 
status

# filtrar y seleccionar variables categóricas en 'status'
filter(status,  type %in% c("factor", "character")) %>% select(variable)

#summary(datos_cliente)

# Adecuar el dataset para poder aplicar el modelo
df_km <- datos_cliente %>%
  ungroup() %>% # desagrupar el dataframe para poder eliminar CustomerID
  select(-fecha_ultima_compra, -CustomerID) # Eliminar la variable fecha_ultima_compra y CustomerID

# Verificar el tipo de cada variable
sapply(df_km, class)

# Usar el paquete caret para convertir el factor a dummys
#library(caret)
# Definir el modelo de dummies
dummies_m <- dummyVars(~ Country, data = df_km)

# Generar las variables dummy
dummies <- predict(dummies_m, df_km)

# Convertir las variables dummy a un dataframe
dummies_df <- as.data.frame(dummies)

# Eliminar la columna original 'Country' del dataframe y unirlo con las var dummies
df_km <- df_km[, !names(df_km) %in% "Country"] %>% # elimina las variables  cuyo nombre sea Country
  cbind(dummies_df) # Une el df con las var dummies

# Estandarizar las variables de df_km, incluyendo las variables dummy
df_km_std <- as.data.frame(scale(df_km)) # estandariza todas las variables del df

summary(df_km_std)

# Método del codo para ver la cantidad óptima de clusters
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(df_km, i)$withinss)
}

# Graficar el método del codo
ggplot() + geom_point(aes(x = 1:10, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:10, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

# generar el modelo k-means
# establecer semilla, asegura que cada vez que se corra el código, el algoritmo generará los mismos resultados.
set.seed(1234)

# Aplicar k-means
model_kmeans <- kmeans(df_km_std, # Esta función realiza el agrupamiento
                       centers = 3, # cantidad de clusteres que se deben crear
                       iter.max = 100, # Número de iteraciones que debe hacer el algoritmo
                       nstart = 10) # Indica cuantas veces debe ejecutar el algoritmo con diferentes centro de cluster

# ver los valores y nombres de las columnas
model_kmeans$cluster
model_kmeans$centers
colnames(df_km_std)

# agregar la variable de cluster a los datos del modelo para poder graficar los cluster
df_km_std$cluster <- model_kmeans$cluster

# Contar cuántos elementos hay en cada clúster
table(df_km_std$cluster)


# ************* Ejercicio 3 *************************************
# Gráfica de visualización del agrupamiento de k-means
ggplot() +
  geom_point(aes(x = CustomerID, y = total_compra, color = model_kmeans$cluster), data = datos_cliente, size = 2)+
  scale_colour_gradientn(colours=rainbow(4))+
  geom_point(aes(x = model_kmeans$centers[, 1], y = model_kmeans$centers[, 2]), color = 'black', size = 3)+
  ggtitle('Clusters con k = 3 / k-medios')+
  xlab('CustomerID')+
  ylab('total_compra')


# *************** Ejercicio 4 *************************
# ***** 4a matriz de Spearman
summary(datos_cliente)

# Verificar el tipo de cada variable
sapply(datos_cliente, class)

# preparar datos para la matriz de Spearman
datos_spearman <- datos_cliente %>%
  ungroup() %>% # desagrupar el dataset datoscliente
  select(-CustomerID, -Country, -fecha_ultima_compra) # eliminar estas variables para dejar solo numéricas

# Generar matriz de correlación Spearman
matriz_spearman <- cor(datos_spearman, # usa la función cor para calcular la correlación entre var
                       method = "spearman", # usa el método spearman
                       use = "complete.obs") # toma todas las observaciones no nulas

# ***** 4b clustering aglomerativo
# matriz de disimilaridad
matriz_disim <- dist(matriz_spearman, # la función dist calcula la matriz de distancias
                     method = 'euclidean') # especifica el método de cálculo de las distancias

# aplicar agrupamiento jerárquico con el link completo
her_cls <- hclust(matriz_disim, 
                  method = 'complete')

# Cortar el dendrograma en k grupos (por ejemplo, 3 grupos)
grupos <- cutree(her_cls, k = 3)

grupos

# graficar el dendograma
plot(her_cls, cex = 0.6, hang = -1)

# se utiliza para crear mapas de calor (heatmaps) que visualizan datos en formato de matriz
#install.packages("pheatmap")
#library(pheatmap)

# Para visualizar los resultados, usar un mapa de calor que ayude visualmente a identificar las correlaciones.
pheatmap(matriz_spearman, 
         clustering_method = "ward.D2")

# *********Ejercicio 5a ****************

# Verificar el tipo de cada variable
sapply(datos_cliente, class)

# Crear un nuevo dataset para el glm
df_churn <- datos_cliente %>%
  mutate(churn = ifelse(recencia > 90, 1, 0))  # añadir la columna churn y establecer una condición

# Unir los df "df_km_std", con la variable churn del df "df_churn"
df_glm <- cbind(df_km_std, churn = df_churn$churn)

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
                  family = "binomial") # indica distribución binomial para el modelo.

# Contar cuántos valores son 1 y cuántos son 0 en la variable churn
table(train_data$churn)
summary(modelo_glm)

# Predecir probabilidades en el conjunto de prueba
predictions_glm <- predict(modelo_glm, # Modelo que se va a usar para hacer la predicción
                           newdata = test_data, # dataset que se va a usar
                           type = "response")

predictions_glm

# Convertir las probabilidades predichas a etiquetas binarias usando un umbral de 0.5
predicted_labels <- ifelse(predictions_glm > 0.5, 1, 0)

# Contar cuántos valores son 1 y cuántos son 0
table(predicted_labels)

# Gráfica de distribución de probabilidades predichas
hist(predictions_glm, breaks = 20, 
     main = "Distribución de Probabilidades Predichas", 
     xlab = "Probabilidad Predicha")

# ************* Ejercicio 5b *********
# Use the “svm()” function to build a support vector machine model
# Instalar y cargar la librería e1071
#install.packages("e1071")
#library(e1071)

# Construir el modelo SVM
modelo_svm <- svm(churn ~ ., # Se indica que la var dependiente esté en función de todas las var indep.
                  data = train_data, # dataset que se va a usar
                  probability = TRUE) # Calcular las probabilidades

# Ver el resumen del modelo
summary(modelo_svm)

# Obtener las predicciones del modelo svm
predict_svm <- predict(modelo_svm, # Modelo que se va a usar para hacer la predicción
                       test_data, # datos que se van a usar para hacer la predicción
                       probability = TRUE)

# Agregar las predicciones al dataframe de test
test_data$predicted_prob_svm <- predict_svm

# Comparar las predicciones con los valores reales
table(test_data$churn, test_data$predicted_prob_svm > 0.5)

# Graficar el histograma de las probabilidades predichas
ggplot(data = test_data, aes(x = predicted_prob_svm)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Probabilidades Predichas de Churn",
       x = "Probabilidad de Churn",
       y = "Frecuencia") +
  theme_minimal()













