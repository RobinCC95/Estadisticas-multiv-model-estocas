#regresion lineal
concentracion_x = c(.5,.6,.7,.8,1,1.3,1.7,2.5,3.3)
absorvancia_y= c(.15,.18,.23,.24,.3,.39,.52,.73,.88)
datos = data.frame(concentracion_x,absorvancia_y)
#obtener el modelo de minimos cuadrados 
#1- graficar los datos
plot(datos)

#2- calular el  modelo
modelo = lm(datos$absorvancia_y~datos$concentracion_x)
summary(modelo)

#3 ver el Pr sea diferente de cero de tabla anova
#da que es diferente de cero Pr = 9.18*10^-9

#4 ajuste del modelo
r = cor(datos$concentracion_x,datos$absorvancia_y)
r
#r se acerca a 1 tiene una asociacion fuerte con 99%
#organizamos los datos en una tabla
datos_tabla = data.frame(datos$concentracion_x,datos$absorvancia_y,modelo$fitted.values,modelo$residuals)
datos_tabla

#5 validacion de los supuestos 

#5.1 analisis de residuales 
error = datos_tabla$modelo.residuals
qqnorm(error, main = 'error graficado', xlab = 'quartil del error',ylab = 'error')
qqline(error)
hist(error)

#5.2 pruebas de normalidad
shapiro.test(error)

#igualdad de varianza 
#bptest(model) falta implementar ver apuntes
library(lmtest)
bptest(modelo, studentize = FALSE)

#5.3 preuba durvin wantson
dwtest(modelo, alternative='two.sided')
##pvalor = 0.1825
#no rechazamos el modelo
#para graficar: y no puede dar tendencia. ^y-> blite_valius
plot(error, modelo$fitted.values) 

#concusion se acepta el modelo


# ejercicio 2-> presion de un gas con ecuacion pv^y = c
volumen_x = c(50,60,70,90,100)
presion_y = c(64.7,51.3,40.5,25.9,7.8)
datos = data.frame(volumen_x,presion_y)
#graficar datos
plot(datos)
#calcular modelo
modelo = lm(datos$presion_y~datos$volumen_x)
summary(modelo)
#ajuste del modelo
r = cor(datos$volumen_x,datos$presion_y)
r
#validacion de suspuestos
error = modelo$residuals
qqnorm(error, main = 'error graficado', xlab = 'quartil del error',ylab = 'error')
qqline(error)
hist(error)
shapiro.test(error)

#para ver si se puede modelar se hace un cambio de variable


#regresion multiple
colesterol_y = c(350,190,263,320,280,198,232,320,303,220,405,190,230,227,
               440,318,212,340,195,223)
edad_x1 = c(80,30,42,50,45,35,18,32,49,35,50,20,40,30,30,23,35,18,22,41)
grasas_x2 = c(35,40,15,20,35,50,70,40,45,35,50,15,20,35,80,40,40,80,15,34)
#1- graficar
datos = data.frame(colesterol_y,edad_x1, grasas_x2)
plot(datos)
#2- modelo
modelo3 = lm(colesterol_y~edad_x1+grasas_x2)
summary(modelo3)
#ajuste del modelo
r_edad = cor(edad_x1, colesterol_y)
r_edad
r_grasa = cor(grasas_x2, colesterol_y)
r_grasa
#
error = modelo3$residuals
qqnorm(error, main = 'error graficado', xlab = 'quartil del error',ylab = 'error')
qqline(error)
hist(error)
shapiro.test(error)
#puede hacer las pruebas faltantes


#ejer 3
#encontrar modelo que explique la resistencia a la tencion en funcion de la
#madera dura
concentracion <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7, 8, 9, 10, 11, 12, 13, 14, 15)
resistencia <- c(6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1, 39.9, 42, 46.1, 53.1, 
           52, 52.5, 48, 42.8, 27.8, 21.9)

library(ggplot2)
datos = data.frame(concentracion, resistencia)
datos 
#graficamos 
ggplot(datos, aes(x=concentracion, y=resistencia))+
geom_point()+theme_light()

# para hacer un modelo lineal
modelo_normal = lm(resistencia~concentracion, data=datos)
modelo_cuadratico = lm(resistencia~concentracion + I(concentracion^2), data=datos)

ggplot(datos, aes(x=concentracion, y=resistencia))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x, se = FALSE, col= 'red' )+
  geom_smooth(method = 'lm', formula = y~x+I(x^2), se = FALSE, col= 'blue' )+
  theme_light()

#comparamos los dos modelos                                
anova(modelo_normal, modelo_cuadratico)

plot(modelo_cuadratico)

#evaluamos el modelo 
summary(modelo_normal)
summary(modelo_cuadratico)
