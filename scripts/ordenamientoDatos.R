#ordenamiento de datos
library(fdth)

Fuma = c(1,1,1,5,7,8,9,1,10,15,13,28,21,25,20,20,21,23,23,11,11,15,9,11,10,15,13,13,17,17,18,18,19,19,10,10,15,1,13,5,3,5,6,7,8)

Tabla = fdt(Fuma,breaks="Sturges")
Tabla
#rango de datos -> R
R = max(Fuma) -min(Fuma)
R
#numero de filas ->m
m = round(1+3.3*log10(length(Fuma)))
m
#amplotud -> A
A=R/m
A
#realizar un grafico histograma
hist(Fuma, breaks = "Sturges")

#poligono de frecuencias
plot(Fuma, type = "cfp")

#para ingresar a los datos especificos y graficar
pie(Tabla$table$rf)

#otro histograma de frecuencias
plot(Tabla, type = "rfh")
plot(Tabla, type = "fh")
plot(Tabla, type = "cdp")
#opciones como -> fh,fp,rfh,rfp,cdn,cdp, cfh, cfp

#para generar minimo, querttil, mediana, media quertil 3, maximo
summary(Fuma)
#para generar el diagrama de cajas
boxplot(Fuma)
# con los percentiles
quantile(Fuma, 0.25)
quantile(Fuma, c(0.1,0.2,0.3,0.4))

