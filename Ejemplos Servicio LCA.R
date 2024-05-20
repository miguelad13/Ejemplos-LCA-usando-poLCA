install.packages("poLCA")

library(poLCA)
?poLCA

data(carcinoma)
f <- cbind(A,B,C,D,E,F,G)~1
M1 <- poLCA(f, carcinoma, nclass=1, nrep=10) 
"maximum log-likelihood: -524.4648 

AIC(1): 1062.93
BIC(1): 1082.324"

M2 <- poLCA(f, carcinoma, nclass=2, graphs=TRUE, nrep=10)
"maximum log-likelihood: -317.2568 
 
AIC(2): 664.5137
BIC(2): 706.0739"

M3 <- poLCA(f, carcinoma, nclass=3, nrep=10)
"maximum log-likelihood: -293.705 
 
AIC(3): 633.41
BIC(3): 697.1357"

M4 <- poLCA(f, carcinoma, nclass=4, nrep=10)
"maximum log-likelihood: -289.2858 
 
AIC(4): 640.5717
BIC(4): 726.4629"

#Se ve que el modelo 3 tiene tanto BIC como AIC con los menores valores.


# Crear una matriz con las probabilidades de contestar 1
matriz_probabilidades <- matrix(NA, nrow = 7, ncol = 3, dimnames = list(c("A", "B", "C", "D", "E", "F", "G"), c("clase1", "clase2", "clase3")))

#Llenar tabla(solo llenar la tabla con los valores del mejor modelo (3))
matriz_probabilidades["A", "clase1"] <- (0.4872)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["A", "clase2"] <- (0.0000)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["A", "clase3"] <- (0.9427)  # Reemplaza 0.1 por tu valor real

matriz_probabilidades["B", "clase1"] <- (0.0000)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["B", "clase2"] <- (0.0191)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["B", "clase3"] <- (0.8621)  # Reemplaza 0.1 por tu valor real

matriz_probabilidades["C", "clase1"] <- (1.0000)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["C", "clase2"] <- (0.1425)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["C", "clase3"] <- (1.0000)  # Reemplaza 0.1 por tu valor real

matriz_probabilidades["D", "clase1"] <- (0.9424)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["D", "clase2"] <- (0.4138)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["D", "clase3"] <- (1.0000)  # Reemplaza 0.1 por tu valor real

matriz_probabilidades["E", "clase1"] <- (0.2494)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["E", "clase2"] <- (0.0000)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["E", "clase3"] <- (0.9449)  # Reemplaza 0.1 por tu valor real

matriz_probabilidades["F", "clase1"] <- (1.0000)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["F", "clase2"] <- (0.5236)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["F", "clase3"] <- (1.0000)  # Reemplaza 0.1 por tu valor real

matriz_probabilidades["G", "clase1"] <- (0.3693)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades["G", "clase2"] <- (0.0000)  # Reemplaza 0.3 por tu valor real
matriz_probabilidades["G", "clase3"] <- (1.0000)  # Reemplaza 0.1 por tu valor real

# Mostrar la tabla
matriz_probabilidades

# Gráfica con los datos de la tabla
datos_tabla <- data.frame(
  clase1 = c(0.4872, 0.0000, 1.0000, 0.9424, 0.2494, 1.0000, 0.3693),
  clase2 = c(0.0000, 0.0191, 0.1425, 0.4138, 0.0000, 0.5236, 0.0000),
  clase3 = c(0.9427, 0.8621, 1.0000, 1.0000, 0.9449, 1.0000, 1.0000)
)
# Crear colores para las barras
colores <- c("blue", "red", "green")
# Crear el gráfico de barras
barplot(t(datos_tabla), beside = TRUE, col = colores, xlab = "Variables", ylab = "Probabilidad")
# Agregar la leyenda del gráfico
legend("top", legend = c("Clase 1", "Clase 2", "Clase 3"), fill = colores, ncol = 3, title = "Clases", y.intersp = 1, bty = "n")

#EJEMPLO 2 IDENTIFICAR DISTINTOS COMPORTAMIENTOS EN LA TOMA DE ALCOHOL FALTA PULIR EL ERRO QUE SALE AL CORRER M1_2"

"Cargar base de datos en archivo xlxs"
datos_2 <- read.csv("C:/Users/migue/Desktop/Ejemplos LCA Roy/Ejemplo LCA Roy 2.csv")

"Checar que si sean los datos"
head(datos_2)


# Suponiendo que tienes un dataframe llamado 'datos' con las respuestas a las preguntas
# Recodificar las respuestas a enteros positivos
datos_recodificados <- datos_2
datos_recodificados[] <- lapply(datos_recodificados, function(x) as.integer(x) + 1)

"Primero se especifica en f, cuales serán las variables incluídas en el análisis"
f_2 <- cbind(pregunta1, pregunta2, pregunta3, pregunta4, pregunta5, pregunta6, pregunta7, pregunta8, pregunta9)~1
"Se va a crear una variable para cada modelo que haremos, M1 es para modelo de una clase latente y etc."
"Se quiere comparar los valores de los criterios BIC y AIC, se prefiere un menor valor pues indica mejor ajuste"
M1_2 <- poLCA(f_2, datos_recodificados, nclass = 1, nrep = 10)
"AIC(1): 292.1059
BIC(1): 302.1438"
M2_2 <- poLCA(f_2, datos_recodificados, nclass = 2, nrep = 10)
"AIC(2): 217.1946
BIC(2): 238.7044"
M3_2 <- poLCA(f_2, datos_recodificados, nclass = 3, nrep = 10)
"AIC(3): 206.227
BIC(3): 239.2087"
M4_2 <- poLCA(f_2, datos_recodificados, nclass = 4, nrep = 10)
"AIC(4): 203.2865
BIC(4): 247.7401"

#En este caso la el mejor modelo consiste en 2 clases

# Crear una matriz con las probabilidades de contestar 1
matriz_probabilidades_1 <- matrix(NA, nrow = 9, ncol = 2, dimnames = list(c("pregunta1", "pregunta2", "pregunta3", "pregunta4", "pregunta5", "pregunta6", "pregunta7", "pregunta8", "pregunta9"), c("clase1", "clase2")))

#Llenar tabla(solo llenar la tabla con los valores del mejor modelo (3))
matriz_probabilidades_1["pregunta1", "clase1"] <- (0.0000)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta1", "clase2"] <- (0.4762)  # Reemplaza 0.3 por tu valor real

matriz_probabilidades_1["pregunta2", "clase1"] <- (0.0000)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta2", "clase2"] <- (1.0000)  # Reemplaza 0.3 por tu valor real

matriz_probabilidades_1["pregunta5", "clase1"] <- (0.8571)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta5", "clase2"] <- (0.0000)  # Reemplaza 0.3 por tu valor real

matriz_probabilidades_1["pregunta6", "clase1"] <- (0.9524)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta6", "clase2"] <- (0.0000)  # Reemplaza 0.3 por tu valor real

matriz_probabilidades_1["pregunta7", "clase1"] <- (0.2381)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta7", "clase2"] <- (0.4000)  # Reemplaza 0.3 por tu valor real

matriz_probabilidades_1["pregunta8", "clase1"] <- (0.8095)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta8", "clase2"] <- (0.5000)  # Reemplaza 0.3 por tu valor real

matriz_probabilidades_1["pregunta9", "clase1"] <- (0.7619)  # Reemplaza 0.2 por tu valor real
matriz_probabilidades_1["pregunta9", "clase2"] <- (0.0000)  # Reemplaza 0.3 por tu valor real

# Mostrar la tabla
matriz_probabilidades_1

# Crear un marco de datos con los datos de la nueva tabla
datos_tabla_1 <- data.frame(
  clase1 = c(0.0000, 0.0000, NA, NA, 0.8571, 0.9524, 0.2381, 0.8095, 0.7619),
  clase2 = c(0.4762, 1.0000, NA, NA, 0.0000, 0.0000, 0.4000, 0.5000, 0.0000)
)

# Crear un vector de colores para las barras
colores <- c("blue", "red")

# Crear el gráfico de barras
barplot(t(datos_tabla_1), beside = TRUE, col = colores, xlab = "Variables", ylab = "Probabilidad", legend.text = TRUE)

# Agregar la leyenda debajo del gráfico con más espacio
legend("top", legend = c("Clase 1", "Clase 2"), fill = colores, ncol = 2, title = "Clases", y.intersp = 2, bty = "n")
