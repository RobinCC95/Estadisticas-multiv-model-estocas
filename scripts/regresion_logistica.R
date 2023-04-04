#####ejercicio sobre tabaquismo -> regresion logistica
#tabaquismo : si -> 1  no -> 0
#pelvis : control -> 0  casos -> 1
tabaquismo = c(rep(1,77), rep(0, 54), rep(1, 123), rep(0, 171))
pelvis = c(rep(1,77), rep(1, 54), rep(0, 123), rep(0, 171))
datos = data.frame(tabaquismo, pelvis)
table(datos)

## ananlisis de regresion
#transformacion a etiquetas numericas
# = as.numeric(as.factor(tabaquismo))
#pelvis1 = as.numeric(as.factor(pelvis))-1
## creamos el modelo
# pelvis es la presencia o ausencia de la enfermedad
modelo = glm(pelvis~tabaquismo, family = "binomial")
summary(modelo)


## ejercicio2 -> intoxicados
dosis = c(1,2,3,4,5)
nExpuestos = c(20,30,30,25,20)
nIntoxicados = c(2,6,9,10,12)
datos = data.frame(dosis, nExpuestos, nIntoxicados)

frecRelat = nIntoxicados/nExpuestos
fracaso = nExpuestos-nIntoxicados

datos = data.frame(dosis, nExpuestos, nIntoxicados, frecRelat, fracaso)
datos
#hay 3 formas de resolver y realizar la regresion logisitica

#modelo 1 -> datos en modo frecuencia relativa
model1 = glm(datos$frecRelat~datos$dosis,weights = datos$nExpuestos, family = "binomial")
summary(model1)

#model 2 -> matriz de exitos y fracasos 
#creamos una nueva tabla
md = cbind(datos$nIntoxicados,datos$fracaso)
model2 = glm(md~datos$dosis,family = "binomial")
summary(model2)

#model 3 -> datos en modo vector de exitos y fracasos
y = c(rep(1, sum(datos$nIntoxicados)),rep(0,sum(datos$fracaso)))
dosis2 = c(rep(datos$dosis, datos$nIntoxicados),rep(datos$dosis, datos$fracaso))
model3 = glm(y~dosis2, family = "binomial")
summary(model3)


## hacemos lo mismo pero con los datos de mujeres + hombres
dosisAll = c(1,2,3,4,5,6,7,8,9,10)
nIntoxicadosAll = c(2,6,9,10,12,4,10,16,20,19)
nExpuestosAll = c(20,30,30,25,20,20,30,30,25,20)
sexo = c(rep(1,5),rep(2,5))
#sexo = as.factor(sexo)
datos2 = data.frame(dosisAll, nExpuestosAll, nIntoxicadosAll, sexo)
datos2
model4 = glm(datos2$nIntoxicadosAll/datos2$nExpuestosAll~datos2$dosisAll+datos2$sexo, 
             weights = datos2$nExpuestosAll, family = "binomial")
summary(model4)
plot(model4)

# grafica del ajuste lineal
#newData = data.frame(pelvis=seq(min(datos)))




