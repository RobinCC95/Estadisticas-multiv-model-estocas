# sacar vector de medias
peso = c(54.2,65,78,77.3,88.3)
edad = c(22,32,27,24,21)
altura = c(1.62,1.69,1.75,1.70,1.78)
x = data.frame(peso, edad, altura)
x_media = colMeans(x)
x
x_media

#matriz de covarianza
s = cov(x)
s
det(s) # varianza generalizada con matriz de covarianza
sum(diag(s)) # varianza total
#para sacar la inversa
s_inversa = solve(s)
s_inversa

#matriz de correlacion
r = cor(x)
r
plot(x)#dispersograma
#para ver la correlacion entre variables
det(r)
nab = 1 - det(r)
nab # si tiende a 1 hay alta correlacion


#distancias
# distancias por individuo
#distancia ecludiana
dist_individuo_eucli = dist(x)
dist_individuo_eucli
#distancia por vector de medias
dist_media_eucli = dist(rbind(x_media,x))
dist_media_eucli

#distancia mahalanovis
# Calcular la distancia de Mahalanobis por individuo
dist_individ_mahal = apply(x, 1, function(xi) {
  sqrt(mahalanobis(xi, x_media, s_inversa))
})
# Mostrar las distancias de cada individuo
dist_individ_mahal

#distancia con respecto a la media
dist_media_mahal = sqrt(mahalanobis(x, x_media, s_inversa))
dist_media_mahal


