#probabilidad discreta 
Tienda = function(x) {
  f = (factorial(16) / factorial(3 - x) * (factorial(13 + x))*(factorial(4) / 
  factorial(x)) * factorial(4 - x)) / (factorial(20) / factorial(3) * factorial(20 - 3))

  return(f)
}
x = 0:3
fx = Tienda(x)
Fx = cumsum(fx)
tabla = data.frame(x,fx,Fx)
tabla
#grafica de la funcion 
plot(x,fx,type = "h")



#para realizar un distribucion binomial
n= 10
x = 3
p= 0.3
dbinom(x,n,p)
#distribucion acumulada se coloca -1 para que de el lado izquierdo
1- pbinom(x,n,p)

#ejercicio con un vector de datos
x = 0:25
n=25
p= 0.3
fx= dbinom(x, n,p)
fx
Fx = cumsum(fx)
Fx= pbinom(x,n,p)
Tabla = data.frame(x,fx,Fx)
Tabla
plot(x,fx,type="h")

#distribucion de poisson
x = 0:10
laamda = 4
fx= dpois(x,laamda)
Fx = ppois(x, laamda)
Tabla = data.frame(x,fx,Fx)
Tabla
plot(x,fx,type="h")

#distribucion normal
x = seq(-10,10, len= 1000)
media = 5.5
desviacion = 0.4
fx = dnorm(x,media, desviacion )
plot(x,fx,type = "l")


