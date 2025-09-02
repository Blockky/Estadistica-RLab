"EJERCICIO 1 DE LA PRÁCTICA 5 - PARTE 2"
options(scipen = 90)

#Lectura del archivo csv
encuesta = na.omit(read.csv2("encuesta.csv"))

#1A
"X -> N(m,d)"
(m = mean(encuesta$VIAJE))
(d = sd(encuesta$VIAJE))

#1B
(Esperanza = m)
(Varianza = d^2)

#1C
par(mfrow=c(1,2))
curve(dnorm(x,m,d),0,180, main = "Función de densidad")
curve(pnorm(x,m,d),0,180, main = "Función de distribución")
par(mfrow=c(1,1))

#1D
"Calcular P(X<60), en la variable aleatoria esto es P(X≤60)"
(p.menor.60 = pnorm(60,m,d))

"Ahora dibujamos en el diagrama de la función de densidad el área para P(X≤60)"
d.intervaloX = seq(0,60,0.01)
d.xBase = c(0,d.intervaloX,60)
d.yTapa = c(0,dnorm(d.intervaloX,m,d),0)
curve(dnorm(x,m,d),0, 180, ylab="f(x)",main='Tardan menos de una hora en llegar')
polygon(d.xBase, d.yTapa, col="cyan")
d.texto.porcentaje = paste(round(p.menor.60*100,2),"%")
text(15, 0.008, d.texto.porcentaje)

#1E
"Calcular P(60≤X≤90), que es lo mismo que P(X≤90)-P(X≤60)"
(p.menor.igual.90 = pnorm(90,m,d))
(p.entre.60_90 = p.menor.igual.90 - p.menor.60)

"Ahora dibujamos en la función de densidad su área correspondiente"
e.intervaloX = seq(60,90,0.01)
e.xBase = c(60,e.intervaloX,90)
e.yTapa = c(0,dnorm(e.intervaloX,m,d),0)
curve(dnorm(x,m,d),5, 180, ylab="f(x)",main='Tardan entre 60 y 90min en llegar')
polygon(e.xBase, e.yTapa, col="cyan")
e.texto.porcentaje = paste(round(p.entre.60_90*100,2),"%")
text(93, 0.01, e.texto.porcentaje)

#1F
"Calcular P(X>90), que es lo mismo que calcular 1 - P(X<90)"
(p.mayor.90 = 1 - pnorm(90,m,d))

"Ahora dibujamos en la función de densidad su área correspondiente"
f.intervaloX = seq(90,180,0.01)
f.xBase = c(90,f.intervaloX,180)
f.yTapa = c(0,dnorm(f.intervaloX,m,d),0)
curve(dnorm(x,m,d),0, 180, ylab="f(x)",main='Tardan más de 90min en llegar')
polygon(f.xBase, f.yTapa, col="cyan")
f.texto.porcentaje = paste(round(p.mayor.90*100,2),"%")
text(130, 0.004, f.texto.porcentaje)

#1G
(cuartiles = qnorm(c(0.25,0.50,0.75),m,d))
(segundo.cuartil = cuartiles[2])
(p.segundo.cuartil= pnorm(segundo.cuartil,m,d))

"Ahora dibujamos el área del segundo cuartil en la función de densidad"
g.intervaloX = seq(0,segundo.cuartil,0.01)
g.xBase = c(0,g.intervaloX,segundo.cuartil)
g.yTapa = c(0,dnorm(g.intervaloX,m,d),0)
curve(dnorm(x,m,d),0,180,ylab="f(x)",main='Segundo cuartil')
polygon(g.xBase, g.yTapa,col="cyan")
g.texto.porcentaje = paste(round(p.segundo.cuartil*100,2),"%")
text(25, 0.009, g.texto.porcentaje)

#1H
tiempos.reales = encuesta$VIAJE
hist(tiempos.reales, freq=FALSE)
curve(dnorm(x,m,d),0,180, add = TRUE)

#1I
plot(density(tiempos.reales))
curve(dnorm(x,m,d),0,180, add=TRUE, col="red")
legend("topright",legend=c("Tiempo","N(64.1,37.1)"), col=c("black", "red"), lty=1)


"EJERCICIO 2"


"X -> B(50,1/6)"
(n = 50)
(p = 1/6)

#2A
"B(n,p) ~> N(m,d)"
(m = n*p)
(d = sqrt(n*p*(1-p)))

#2B
(esperanza = m)
(varianza = d^2)

#2C
par(mfrow=c(1,2))
curve(dnorm(x,m,d),0,50, main = "Función de densidad")
curve(pnorm(x,m,d),0,50, main = "Función de distribución")
par(mfrow=c(1,1))

#2D
dnorm(-0.5,m,d)

#2E
dnorm(50.5,m,d)

#2F
pnorm(9.5,m,d)

#2G
1 - pnorm(10.5,m,d)

#2H
pnorm(40.5,m,d) - pnorm(19.5,m,d)

#2I
qnorm(c(0.50,0.90),m,d)

#2J
muestra = rnorm(100,m,d)
hist(muestra, breaks = 0:n)
lines(0:n,100*dnorm(0:n,m,d),col="red")


"EJERCICIO 3"


"X -> P(λ)"
(lambda = 120/24)

#3A
"P(λ) ~> N(m,d)"
(m = lambda)
(d = sqrt(lambda))

#3B
(Esperanza = m)
(Varianza = d^2)

#3C
par(mfrow=c(1,2))
curve(dnorm(x,m,d),0,20, main = "Función de densidad")
curve(pnorm(x,m,d),0,20, main = "Función de distribución")
par(mfrow=c(1,1))

#3D
dnorm(4.5,m,d)

#3E
pnorm(4.5,m,d)

#3F
1 - pnorm(4.5,m,d)

#3G
dnorm(-0.5,m,d)

#3H
pnorm(6.5,m,d) - pnorm(3.5,m,d)

#3I
qnorm(c(0.25,0.50,0.75,1),m,d)

#3J
muestra = rnorm(100,m,d)
hist(muestra, breaks = 0:20)
lines(0:20,100*dnorm(0:20,m,d),col="red")

#3K
lambda = 120
m = lambda
d = sqrt(lambda)

dnorm(100.5,m,d)

#3L
muestra = rnorm(100,m,d)
hist(muestra, breaks = 0:200)
lines(0:200,100*dnorm(0:200,m,d),col="red")
