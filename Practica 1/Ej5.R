#Ej 5. Dibujar los siguientes diagramas, uno junto a otro.
y = function (x) {-3*x+50}

par(mfrow=c(1,2))
x=c(0:100)

plot(x, -3*x+50, type="l", main="plot", col="blue")
curve(y, from=0, to=100, main="curve", col="blue")
