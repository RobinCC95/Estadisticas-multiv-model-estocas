# Datos para el ejemplo
x <- 1:10
y1 <- x^2
y2 <- x^3

# Gráfico básico sin leyenda
plot(x, y1, type = "l", col = "blue")
lines(x, y2, type = "l", col = "red")

# Agregar leyenda
legend("topright",
       legend = c("Serie A", "Serie B"),
       col = c("blue", "red"),
       lty = 1)
# datos
datos_circulares <- c(30, 20, 50)

# etiquetas
etiquetas <- c("A", "B", "C")

# gráfico circular
pie(datos_circulares, labels=etiquetas)


nivel_tcdd_plasma = c(2.5, 3.1, 2.1, 3.5, 3.1, 1.8, 6.0, 3, 36, 4.7, 6.9, 3.3, 4.6, 1.6, 7.2, 1.8, 20, 2, 2.5, 4.1)
nivel_tcdd_tejido = c(4.9, 5.9, 4.4, 6.9, 7, 4.2, 10, 5.5, 41, 4.4, 7.0, 2.9, 4.6, 1.4, 7.7, 1.1, 11, 2.5, 2.3, 2.5)
di = c(-2.4, -2.8, -2.3, -3.4, -3.9, -2.4, -4.0, -2.5, -5.0, 0.3, -0.1, 0.4, 0, 0.2, -0.5, 0.7, 9, -0.5, 0.2, 1.6)

# Cálculo del intervalo de confianza
alpha <- 0.05
n <- length(nivel_tcdd_plasma)
m <- length(nivel_tcdd_tejido)
df <- n + m - 2
s <- sqrt(((n-1)*var(nivel_tcdd_plasma) + (m-1)*var(nivel_tcdd_tejido))/df)

t_crit <- qt(1 - (alpha/2), df)

left_end <- mean(nivel_tcdd_plasma) - mean(nivel_tcdd_tejido) - t_crit*s*sqrt(1/n + 1/m)
right_end <- mean(nivel_tcdd_plasma) - mean(nivel_tcdd_tejido) + t_crit*s*sqrt(1/n + 1/m)

cat("Intervalo de confianza del 95% para la diferencia entre las medias verdaderas de los niveles de TCDD en plasma y tejido adiposo:")
cat(left_end, ",", right_end)



library(ggplot2)

# Creamos un dataset
data <- data.frame(x = c(1, 2, 3, 4, 5), y = c(2, 4, 5, 4, 2))

# Creamos el gráfico
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 3) +
  labs(title = "Ejemplo de gráfico con ggplot()", x = "Eje X", y = "Eje Y")


