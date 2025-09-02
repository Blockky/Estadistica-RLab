" PRÁCTICA 5 - PARTE 1 "
options(scipen = 90)

#1A
"X -> B(n,p)"
n=50
p=1/6

#1B
Esperanza = n*p
Varianza = n*p*(1-p)

#1C
par(mfrow=c(1,2))
plot(0:n,dbinom(0:n,n,p),type="h",main="Función de probabilidad")
lines(0:n,dbinom(0:n,n,p))
plot(0:n,pbinom(0:n,n,p),type="s",main="Función de distribución")
par(mfrow=c(1,1))

#1D
dbinom(0,n,p)

#1E
dbinom(50,n,p)

#1F
pbinom(9,n,p)

#1G
1 - pbinom(10,n,p)

#1H
pbinom(40,n,p) - pbinom(20,n,p)

#1I
qbinom(c(0.50,0.90),n,p)

#1J
muestra = rbinom(100,n,p)
hist(muestra, breaks = 0:n)
lines(0:n,100*dbinom(0:n,n,p))

"EJERCICIO 2"

#2A
encuesta = read.csv2("encuesta.csv")
encuesta = na.omit(encuesta)
table(encuesta$MOVIL)
(n=6)
(p=26/74)

#2B
Esperanza = n*p
Varianza = n*p*(1-p)

#2C
par(mfrow=c(1,2))
plot(0:n,dbinom(0:n,n,p),type="h",main="Función de probabilidad")
lines(0:n,dbinom(0:n,n,p))
plot(0:n,pbinom(0:n,n,p),type="s",main="Función de distribución")
par(mfrow=c(1,1))

#2D
"P[X=3]"
(P.3=dbinom(3,n,p))

#2E
"P[X≤3]"
(P.menor.igual.3=pbinom(3,n,p))

(P.menor.igual.3=dbinom(0,n,p)+dbinom(1,n,p)+dbinom(2,n,p)+dbinom(3,n,p))

#2F
#P[X>3]
(P.mayor.3=1-P.menor.igual.3)

(P.mayor.3=dbinom(4,n,p)+dbinom(5,n,p)+dbinom(6,n,p))

#2G
(P.todos=dbinom(6,n,p))

#2H
(P.ninguno=dbinom(0,n,p))

#2I
(P.entre.2.y.4=dbinom(2,n,p)+dbinom(3,n,p)+dbinom(4,n,p))

(P.entre.2.y.4=pbinom(4,n,p)-pbinom(1,n,p))

#2J
qbinom(c(0.25,0.50,0.75,1),n,p)

pbinom(1,n,p)
pbinom(2,n,p)
pbinom(3,n,p)


#2K
encuesta = read.csv2("encuesta.csv")
nrow(encuesta)
encuesta = na.omit(encuesta)
nrow(encuesta)

encuesta$MOVIL

(jazztel = c(1,1,3,3,1,2,4,3,2,3,2,1))
hist(jazztel, breaks = 0:n)
lines(0:n,12*dbinom(0:n,n,p))

#2L
(muestra=rbinom(12,n,p))
hist(muestra, breaks = 0:n)
lines(0:n,12*dbinom(0:n,n,p))

"EJERCICIO 3"

#3A
"X -> P(λ)"
(lambda = 120/24)

#3B
(Esperanza = lambda)
(Varianza = lambda)

#3C
par(mfrow=c(1,2))
plot(0:20,dpois(0:20,5),type="h",main="Función de probabilidad")
lines(0:20,dpois(0:20,5))
plot(0:20,ppois(0:20,5),type="s",main="Función de distribución")
par(mfrow=c(1,1))

#3D
dpois(4,5)

#3E
ppois(4,5)

#3F
1 - ppois(4,5)

#3G
dpois(0,5)

#3H
ppois(6,5) - ppois(3,5)

#3I
qpois(c(0.25,0.50,0.75,1),5)

#3J
(muestra=rpois(100,5))
hist(muestra, breaks = 0:20)
lines(0:20,100*dpois(0:20,5),col="red")

#3K
dpois(100,120)

#3L
(muestra=rpois(100,120))
hist(muestra, breaks = 0:200)
lines(0:200,100*dpois(0:200,120),col="red")
