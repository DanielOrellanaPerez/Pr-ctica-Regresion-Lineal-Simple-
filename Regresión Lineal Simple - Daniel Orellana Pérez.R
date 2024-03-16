getwd()
nuevo_dir <- "C:/regresión_lineal_simple"
setwd(nuevo_dir)
if(file.exists(nuevo_dir)) {
  cat("Directorio creado correctamente: ", nuevo_dir, "\n")
} else {
  cat("Fallo al crear direcorio: ", nuevo_dir, "\n")
}

#Ejercicio 8
cuentas <- c(110, 2, 6, 98, 40, 94, 31, 5, 8, 10)
distancia <- c(1.1, 100.2, 90.3, 5.4, 57.5, 6.6, 34.7, 65.8, 57.9, 86.1)
datos <- data.frame(cuentas, distancia)
modelo <- lm(cuentas ~ distancia, data = datos)

summary(modelo)
plot(distancia, cuentas, main = "Recta de regresión", xlab = "Distancia", ylab = "Cuentas")
abline(modelo, col = "blue")

#Ejercicio 13

cuentas_obs <- c(6, 98, 40, 94, 31, 5, 8, 10)
predicciones <- c(-6.682842, 85.520196, 28.938591, 84.216973, 53.69983, 19.924631, 28.504183, -2.121561)
residuos <- cuentas_obs - predicciones
print(residuos)

#Ejercicio 14

qqnorm(residuos)
qqline(residuos)

#Ejercicio 16
cuentas <- c(6, 98, 40, 94, 31, 5, 8, 10)
distancia <- c(1.1, 100.2, 90.3, 5.4, 57.5, 6.6, 34.7, 65.8)


set.seed(123)  
indices_entrenamiento <- sample(1:length(cuentas), 0.7 * length(cuentas))  
indices_prueba <- setdiff(1:length(cuentas), indices_entrenamiento)

cuentas_entrenamiento <- cuentas[indices_entrenamiento]
distancia_entrenamiento <- distancia[indices_entrenamiento]

cuentas_prueba <- cuentas[indices_prueba]
distancia_prueba <- distancia[indices_prueba]

modelo <- lm(cuentas_entrenamiento ~ distancia_entrenamiento)

predicciones <- predict(modelo, data.frame(distancia_entrenamiento = distancia_prueba))

error_cuadratico_medio <- sqrt(mean((cuentas_prueba - predicciones)^2))
r_cuadrado <- cor(predicciones, cuentas_prueba)^2

print(paste("Error cuadrático medio:", error_cuadratico_medio))
print(paste("Coeficiente de determinación (R^2):", r_cuadrado))