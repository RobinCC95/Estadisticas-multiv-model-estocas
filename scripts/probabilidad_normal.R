#clase de probabilidad normal
#pnorm -> probabilidad acumulada
#dnorm -> probabilidad de un valor exacto
#rnorm -> valores aleatorios
#qnorm  -> valor de x
#peso de crias de perro [kg]
media1 = 2.5
desviacion1 = 1
n_muestra = 10
resultado = c(2.64,2.14,2.52,2.83,2.18,2.51,3.11,2.67,2.62,2.19,2.31,2.44,2.48,1.63,2.60,2.51,
              2.45,2.62,2.51,3.02,2.39,2.51,2.50,2.23)
resultado


plot(resultado,type = 'h')
#P(x>=2) = 1-p(x<=2)
x = pnorm(2, media1,desviacion1)
#P(x>=k) = 0.2    <=> 1-p(x<=k)
x= qnorm(0.8,media1,desviacion1)
#es distribucion normal grafica

#segundo ejercicio
media2 = 0
desviacion2 =1 
x = seq(-10,10, len=1000)
fx= dnorm(x, media2,desviacion2)
Fx = pnorm(x,media2,desviacion2)
plot(x,fx, type = 'l')

#inferencia

datos = c(0.060,1.827,0.372,0.610,0.521,1.189,0.537,0.898,0.319,0.603,0.614,0.374,0.411,
          0.406,0.533,0.788,0.449,0.348,0.413,0.662,0.273,0.262,1.925,0.767,1.177,2.464,
          0.448,0.55,0.385,0.307,0.571,0.971,0.622,0.674,1.499)
pacientes = 35
nivel_confianza = 0.95

t.test(datos, alternative = 'two.sided',mu=0.0, conf.level = nivel_confianza)

#proporcion poblacional ^p
resist = c(348.3,378.9,329.6)
n = length(resist)
x = sum(as.numeric(resist>350))
#falta completar ver diapositiva



#intervalo de confiaza de la diferencia de medias poblacionales y no dependientes
no_fuma = c(10,8.4,12.8,25,11.8,9.8,12.5,15.4,23.5,9.4,25.1,19.5,25.5,9.8,
            7.5,11.8,12.2,15)
fuma= c(30,30.1,15,24.1,30.5,17.8,16.8,14.8,13.4,28.5,17.5,14.4,12.5,20.4)

t.test(fuma,no_fuma, alternative = 'two.sided', conf.level = .95)
#si el intervalo no pasa por cero, hay diferencias significativas
#con una confianza del 95% presenta diferencias significativas
#al ser intervalo positiva deducimos que las fumadoras, con una probabilidad del 95% 
#las fumadoras hay mas presencia de cadmoio que en las no fumadoras




#intervalo de confiaza de la diferencia de medias poblacionales y  dependientes
matA = c(13.2,8.2,10.9,14.3,10.7,6.6,9.5,10.8,8.8,13.3)
matB =c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6)

t.test(matA,matB,alternative = 'two.sided', conf.level=.95, paired=TRUE)
