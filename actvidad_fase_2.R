# ********** Ejercicio 1 ***********************

# cargar el conjunto de datos base (original)
datos <- read.csv("Appendix 1 - Database.csv")

# Ver el resumen
summary(datos)

# Convertir InvoiceDate de tipo 'chr' a 'date-time'
install.packages("tidyverse")
library(tidyverse)
# activar dplyr en packages para usar pipe(%>%)
install.packages("lubridate")
library(lubridate)

# Convertir la variable'InvoiceDate' a tipo POSIXct
# Convierte una fecha y hora en un objeto de tipo POSIXct, que es básicamente un número (segundos desde el "epoch", 
# es decir, desde el 1 de enero de 1970).
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

# Crear dataset a nivel cliente
datos_cliente <- datos_limpios %>%
  group_by(CustomerID, Country) %>% # Agrupar por número de id del clientey país
  summarise( # Crea columnas
    fecha_ultima_compra = max(InvoiceDate), # fecha ult compra de cada cliente
    recencia = as.integer(fecha_compra_max - max(InvoiceDate)), # cantidad de días desde la última compra general
    frecuencia_compra = n_distinct(InvoiceNo), # cantidad de veces que compró
    total_compra = sum(Quantity * UnitPrice), # total de las compras en dinero      http://127.0.0.1:39195/graphics/plot_zoom_png?width=1200&height=900
    compra_minima = min(Quantity * UnitPrice), # minimo de compras en dinero
    compra_promedio = mean(Quantity * UnitPrice), # promedio de compras en dinero
  ) %>%
  arrange(desc(total_compra)) # Ordena de mayor a menor la columna total_compra

# Para utilizar describe()
install.packages("psych")
library("psych")

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

ggplot(datos_cliente, aes(x = total_compra)) +
  geom_histogram(binwidth = 10, fill = 'green', color = 'blue', alpha = 0.7)+
  labs(title = 'Distribución valores totales de compra por cliente',
       x = 'Total compra',
       y = 'Frecuencia') +
  theme_light()


plot(datos_cliente$recencia, datos_cliente$frecuencia_compra,
     main = "Resencia / Frecuencia compra",
     xlab = "recencia",
     ylab = "Frecuencia compra")


# ********* Ejercicio 2 *********************
#library(caret) # contiene la función dummyVars
#library(dplyr) # librería de data munging
library(funModeling) # df_status function
install.packages("parallelly")
install.packages("parallelly", dependencies = TRUE)
install.packages("future")
install.packages("cli")
install.packages("recipes")
library(recipes)
install.packages("caret")

# Ver el resumen del estado de las variables
status=df_status(datos_cliente, print_results = F) 
status

# filtrar y seleccionar variables categóricas en 'status'
filter(status,  type %in% c("factor", "character")) %>% select(variable)

# Convertir las variables categóricas en varibles dummy (matriz)
dummys = dummyVars("~ Country", data = datos_cliente)

# Aplicar la transformación
datos_cliente_cod <- data.frame(predict(dummys, newdata = datos_cliente))

# Combinar las variables dummy con el resto de las variables originales y eliminar la variable Country
datos_modelo = cbind(datos_cliente[, !names(datos_cliente) %in% "Country"], datos_cliente_cod)

# establecer la semilla
set.seed(1234)

# Generar el modelo
model_kmeans <- kmeans(datos_modelo, 3, iter.max = 100, nstart = 10)

# Corregir los datos del modelo
# Convertir el tipo POSIXct a numérico (cantidad de días desde 01 ene 1970)
datos_modelo$fecha_ultima_compra_num <- as.numeric(datos_modelo$fecha_ultima_compra) / 86400

# Verificar tipos de columnas
sapply(datos_modelo, class)

# Elimina columnas no numéricas
datos_modelo_clean <- datos_modelo %>%
  select_if(is.numeric)

# Método del codo para ver la cantidad óptima de clusters
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(datos_modelo_clean, i)$withinss)
}

# Graficar el método del codo
ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

# Aplicar k-means
model_kmeans <- kmeans(datos_modelo_clean, centers = 4, iter.max = 100, nstart = 10) # cambiar los centros según el método del codo

# ver los valores y nombres de las columnas
model_kmeans$cluster
model_kmeans$centers
colnames(datos_modelo_clean)

# agregar la columna de cluster a los datos del modelo
datos_modelo_clean$cluster <- model_kmeans$cluster

# ************* Ejercicio 3 *************************************
# Gráfica de visualización del agrupamiento de k-means
ggplot() +
  geom_point(aes(x = CustomerID, y = total_compra, color = cluster), data = datos_modelo_clean, size = 2)+
  scale_colour_gradientn(colours=rainbow(4))+
  geom_point(aes(x = model_kmeans$centers[, 1], y = model_kmeans$centers[, 2]), color = 'black', size = 3)+
  ggtitle('Clusters con k = 4 / k-medios')+
  xlab('CustomerID')+
  ylab('total_compra')

# *************** Ejercicio 4 *************************
# ***** 4a matriz de Spearman
summary(datos_cliente)
# Convertir a factor la variable Country
datos_cliente <- datos_cliente %>%
  mutate(Country_f = as.factor(Country))

# preparar datos para la matriz de Pearson
datos_spearman <- datos_cliente %>%
  mutate(fecha_epoc = as.numeric(fecha_ultima_compra)/86400) %>% # para ver la cantidad de días
  mutate(Country_f = as.numeric(Country_f)) %>% # convertir el factor a numérico
  select(-CustomerID, -Country, -fecha_ultima_compra) # eliminar estas variables

# Generar matriz de correlación Spearman
matriz_spearman <- cor(datos_spearman, method = "spearman", use = "complete.obs")

# ***** 4b clustering aglomerativo
datos_aglomerative <- scale(datos_spearman)

# matriz de disimilaridad
matriz_disim <- dist(datos_aglomerative, method = 'euclidean')

# aplicar agrupamiento jerárquico con el link completo
her_cls <- hclust(matriz_disim, method = 'complete')

# graficar el dendograma
plot(her_cls, cex = 0.6, hang = -1)

# Cortar el dendrograma en k grupos (por ejemplo, 3 grupos)
grupos <- cutree(her_cls, k = 3)

grupos

# Para visualizar los resultados, usar un mapa de calor que ayude visualmente a identificar las correlaciones.
pheatmap(matriz_spearman, clustering_method = "ward.D2")

# *********Ejercicio 5 ****************
# Crear un nuevo dataset para el glm
datos_glm <- datos_cliente %>%
  select(-Country) %>% # eliminar la variable Country
  mutate(Country_f = as.numeric(Country_f)) %>% # Convertir el factor a numérico
  mutate(churn = ifelse(recencia > 90, 1, 0)) # añadir la columna churn y establecer una condición

# Agrupar la variable churn y contar sus valores
datos_glm %>% 
  group_by(churn) %>%
  count()

# Establecer semilla
set.seed(123) # Para reproducibilidad

# Obtener el 70% de datos del total del conjunto de datos de forma aleatoria
train_indices <- sample(1:nrow(datos_glm), 0.7 * nrow(datos_glm))

# Dividir los datos obtenidos en prueba y entrenamiento
train_data <- datos_glm[train_indices, ]
test_data <- datos_glm[-train_indices, ]

# Construir el modelo de regresión logistica
modelo_glm <- glm(churn ~ ., data = train_data, family = "binomial")

summary(modelo_glm)

# Predecir probabilidades en el conjunto de prueba
predictions_glm <- predict(modelo_glm, newdata = test_data, type = "response")

predictions_glm

# Gráfica de distribución de probabilidades predichas
hist(predictions_glm, breaks = 20, main = "Distribución de Probabilidades Predichas", xlab = "Probabilidad Predicha")

# ************* Ejercicio 5b *********
# Use the “svm()” function to build a support vector machine model
# Instalar y cargar la librería e1071
#install.packages("e1071")
#library(e1071)

# Construir el modelo SVM
modelo_svm <- svm(churn ~ ., data = train_data, probability = TRUE)

# Ver el resumen del modelo
summary(modelo_svm)

# Obtener las predicciones del modelo svm
predict_svm <- predict(modelo_svm, test_data, probability = TRUE)

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













