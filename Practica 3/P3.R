#Lectura del archivo encuesta.csv
encuesta = read.csv2("encuesta.csv")
encuesta = na.omit(encuesta)

#Funciones
moda = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

#1.Coeficiente de correlación lineal (NOTA,DORMIR)
x = encuesta$NOTA
y = encuesta$DORMIR
coef.cor = cor(x,y)
coef.cor

#2.Diagrama de dispersión y recta de regresión (NOTA,DORMIR)
par(mfrow = c(1, 1))
plot(x,y)
coef.b = cov(x,y) / var(x)
coef.a = mean(y) - coef.b*mean(x)
abline(a=coef.a, b=coef.b) #En función de los coeficientes 'a' y 'b'

#3.Valor de la NOTA si el valor de DORMIR es 10 horas
nota.10horas = coef.a + coef.b*10
nota.10horas

#4.Tabla de frecuencias absolutas con las distribuciones marginales (MOVIL,SO)
MOVIL = encuesta$MOVIL
SO = encuesta$SO
addmargins(table(MOVIL,SO))

#5.Medidas de la variable NOTA condicionada a cada valor de la variable GRADO
var1 = encuesta$NOTA
var2 = encuesta$GRADO
media = tapply(var1, var2, mean)
mediana = tapply(var1, var2, median)
moda = tapply(var1, var2, moda)
varianza = tapply(var1, var2, var)
des.tip = tapply(var1, var2, sd)
cuartiles = tapply(var1, var2, quantile)
media
mediana
moda
varianza
des.tip
cuartiles

#6.Diagramas de barras agrupadas en vertical (DORMIR,SO) y (SO,DORMIR)
t.dormir.so = table(encuesta$GRUPO,encuesta$SO)
t.so.dormir = table(encuesta$SO,encuesta$GRUPO)
par(mfrow = c(1, 2))
barplot(t.dormir.so,
        xlab="DORMIR",
        col=rainbow(4),
        legend.text = rownames(t.dormir.so))
barplot(t.so.dormir,
        xlab="SO",
        col=rainbow(2),
        legend.text = rownames(t.so.dormir))
