#componentes principales
library(readxl)
ruta_archivo <- "datosEst.xlsx"
datos_excel <- read_excel(ruta_archivo)
datos_excel
datos_correl = cor(datos_excel)
r= det(datos_correl)
nab = 1 - r
nab
# no tiene sentido hacer componentes principales si no hay alta correlacion ->nabla

#calculo de los componentes
acp = princomp(datos_excel, cor=TRUE)
summary(acp)

#calculo de los valores propios
val_propios = acp$sdev^2
cat("varianza")
val_propios
#se toma los valores propios  > 1

#calculo de vectores propios
vector_propios = acp$loadings
vector_propios
#componentes sobre el que se recarga las variables

#carga por los individuos
acp$scores

#cambio de signo
acp$loadings[,2] = -acp$loadings[,2]
acp$scores[,2] = -acp$scores[,2]

#grafico de sedimentacion
plot(1:8, val_propios, type= "b", xlab="componentess", ylab="varianza", main = "sedimentacion")
#el grafico me dice que valores estan por encimam de 1 y debo escoger

#correlacion entre las componentes y las variables
desviacion_stand = acp$sdev
correlacion = sweep(vector_propios[1:8,1:8],2, desviacion_stand, "*" )
round(correlacion,3)
#con este dato se puede asignar un nombre o descripcion a los componentes
#componente 1 -> ciencias humanas
#componente 2 -> ciencias basicas

#grafico de cargas de las variables
library(MASS)
x11()
eqscplot(correlacion[,1:2], xlim=c(-1,1), ylim=c(-1,1), type = "n")
abline(h=0,v=0)
text(correlacion[,1:2],labels=colnames(datos),cex=0.8)
symbols(0,0,circles=1, inches=FALSE, add=TRUE)


#grafico de individuos
x11()
plot(acp$scores[,1:2], type= "n",)
abline(h=0, v= 0)
text(acp$scores[,1:2], 
labels= rownames(datos_excel), cex=0.8)
#si los puntso esta pegado al centro es que no aportan nada
#grafico de variables e individuos
x11()
biplot(acp, cex=0.75)

#para rotar el grafico
library(psych)
acp_varimax = principal(datos_excel, nfactors = 2, rotate = "varimax",scores = TRUE)
round(cor(datos_excel, acp_varimax$scores[,1:2]),3)#tabla de comunidades
x11()
biplot(acp_varimax, labels = rownames(datos_excel), cex= 0.75,  main = "")

