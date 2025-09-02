#Lectura y definición de la variable 'viaje' expresada en horas
encuesta = read.csv2("encuesta.csv")
viaje = encuesta$VIAJE
viajeH = round(viaje/60,digits=2)

#Funciones
moda = function(x){
  u = unique(x)
  m = match(x,u)
  t = tabulate(m)
  return(u[t==max(t)])
}
asimetria = function (x) {
  n = length(x)
  return ((1/n)*sum((x-mean(x))^3)/sd(x)^3)
}
curtosis = function (x) {
  n = length(x)
  return (((1/n)*sum((x-mean(x))^4)/sd(x)^4)-
            3)
}

#1.Medidas de centralización
media = round(mean(viajeH),digits = 3)
mediana = median(viajeH)
moda = moda(viajeH)
media; mediana; moda

#2.Medidas de dispersión
vector.rango = range(viajeH)
rango = max(viajeH) - min(viajeH)
varianza = var(viajeH)
desviacion.tipica = sd(viajeH)
coefi.variacion = sd(viajeH)/mean(viajeH)
vector.rango; rango; varianza; desviacion.tipica; coefi.variacion

#3.Medidas de localización
Q1 = quantile(viajeH,0.25)
Q2 = quantile(viajeH,0.5)
Q3 = quantile(viajeH,0.75)
Q4 = quantile(viajeH,1)
P33 = quantile(viajeH,0.33)
P66 = quantile(viajeH,0.66)
(c(Q1, Q2, Q3, Q4))
(c(P33, P66))

#4.Medidas de forma
coe.asi = asimetria(viajeH)
coe.cur = curtosis(viajeH)
coe.cur
if(coe.cur>0) {
  print("Es leptocúrtica")
} else {
  if(coe.cur<0) {
    print("Es platicúrtica")
  } else {
    print("Es mesocúrtica")
  }  }
coe.asi

#5.Diagrama de caja
boxplot(viajeH) #no hay ningún dato atípico
summary(viajeH)

#6.Tabla de frecuencias (datos agrupados)
K = 10
C = diff(range(viajeH))/K
Li= min(viajeH)
L = Li+C*(0:K)
viajeH.agrupada = cut(viajeH, breaks=L, right=FALSE, include.lowest=TRUE)
frec.abs.viajeH.agrupada = table(viajeH.agrupada)
frec.abs.viajeH.agrupada

#7.Media (datos agrupados)
marcas = (L[1:K]+L[1:K+1])/2
media.agrupada = mean(marcas)
media.agrupada

#8.Histograma y diagrama de tarta (datos agrupados)
par(mfrow=c(1,1))
hist(frec.abs.viajeH.agrupada, main="Histograma")
pie(frec.abs.viajeH.agrupada, main="Diagrama de tarta")
