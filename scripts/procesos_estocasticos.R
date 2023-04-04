#procesos estocaticos

## markov
library(markovchain)
mat_transici = matrix(c(0,0.5,0.5,0.5,0,0.5,0.5,0.5,0), nrow = 3, byrow = TRUE)
mc= new("markovchain", transitionMatrix = mat_transici, states = c("a","b","c"), name = "cadena 1")
summary(mc)
plot(mc)
#para 5 pasos al futuro
mc^5
#si adicionamos una condicion inicial
x0 = c(.5,.2,.3)
#distribucion de probabilidad para 6 pasos
x6= x0*(mc)^6
x6
#para hallar la distribucion estacionaria de la cadena
dist_estacion = steadyStates(mc)
dist_estacion
#este sistema siempre va tender a esta distribucion
