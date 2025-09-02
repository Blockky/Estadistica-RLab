#Funciones
moda = function(x) {
  u = unique(x)
  m = match(x,u)
  t = tabulate(m)
  return (u[t == max(t)])
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

#Lectura del archivo csv original
acc.csv.original = read.csv2("2024_Accidentalidad.csv")
acc.csv.original = na.omit(acc.csv.original)

#Nueva base de datos teniendo en cuenta únicamente el distrito y el sexo
distrito = acc.csv.original$distrito
sexo = acc.csv.original$sexo
acc = data.frame(distrito, sexo)

#Tablas de frecuencia
f.abs = table(acc)
f.abs
f.abs.con.margns = addmargins(f.abs)
f.abs.con.margns
f.rel = prop.table(f.abs)
f.rel
f.rel.con.margns = addmargins(f.rel)
f.rel.con.margns

#Medidas estadísticas de los accidentes por distrito
margnl.distrito = margin.table(f.abs, margin=1)
margnl.distrito

num.distritos = length(margnl.distrito)
num.distritos

#...centralización
media.distrito = round(mean(margnl.distrito), digits=0)
media.distrito
mediana.distrito = median(margnl.distrito)
mediana.distrito
moda.distrito = moda(distrito)
moda.distrito

#...dispersión
min.distrito = min(margnl.distrito)
min.distrito
max.distrito = max(margnl.distrito)
max.distrito
rango.distrito = max.distrito - min.distrito
rango.distrito
vari.distrito = var(margnl.distrito)
vari.distrito
des.tip.distrito = sd(margnl.distrito)
des.tip.distrito
coe.var.distrito = des.tip.distrito / media.distrito
coe.var.distrito

#...posición
cuartiles.distrito = quantile(margnl.distrito,c(0,0.25,0.5,0.75,1))
cuartiles.distrito
ric.disrito = IQR(margnl.distrito)
ric.disrito

#...forma
coe.asi.distrito = asimetria(margnl.distrito)
coe.asi.distrito
coe.cur.distrito = curtosis(margnl.distrito)
coe.cur.distrito
if(coe.cur.distrito>0) {
  print("Es leptocúrtica")
} else {
  if(coe.cur.distrito<0) {
    print("Es platicúrtica")
  } else {
    print("Es mesocúrtica")
  }  }

#Medidas de los accidentes por distrito condicionadas por el sexo
acc.mujer = f.abs[ ,"Mujer"]
acc.mujer
acc.hombre = f.abs[ ,"Hombre"]
acc.hombre
acc.des = f.abs[ ,"Desconocido"]
acc.des
tamaño.acc.mujer = sum(acc.mujer)
tamaño.acc.hombre = sum(acc.hombre)
tamaño.acc.des = sum(acc.des)
tamaño.acc.mujer
tamaño.acc.hombre
tamaño.acc.des

#...centralización
media.mujer = round(mean(acc.mujer), digits=0)
media.hombre = round(mean(acc.hombre), digits=0)
media.des = round(mean(acc.des), digits=0)
mediana.mujer = median(acc.mujer)
mediana.hombre = median(acc.hombre)
mediana.des = median(acc.des)
media.mujer
media.hombre
media.des
mediana.mujer
mediana.hombre
mediana.des

#...dispersión
min.mujer = min(acc.mujer)
min.hombre = min(acc.hombre)
min.des = min(acc.des)
max.mujer = max(acc.mujer)
max.hombre = max(acc.hombre)
max.des = max(acc.des)
rango.mujer = max.mujer - min.mujer
rango.hombre = max.hombre - min.hombre
rango.des = max.des - min.des
d.tip.mujer = sd(acc.mujer)
d.tip.hombre = sd(acc.hombre)
d.tip.des = sd(acc.des)
coe.var.mujer = d.tip.mujer / media.mujer
coe.var.hombre = d.tip.hombre / media.hombre
coe.var.des = d.tip.des / media.des
min.mujer
min.hombre
min.des
max.mujer
max.hombre
max.des
rango.mujer
rango.hombre
rango.des
d.tip.mujer
d.tip.hombre
d.tip.des
coe.var.mujer
coe.var.hombre
coe.var.des

#...posición
cuartiles.mujer = quantile(acc.mujer,c(0,0.25,0.5,0.75,1))
cuartiles.hombre = quantile(acc.hombre,c(0,0.25,0.5,0.75,1))
cuartiles.des = quantile(acc.des,c(0,0.25,0.5,0.75,1))
ric.mujer = IQR(acc.mujer)
ric.hombre = IQR(acc.hombre)
ric.des = IQR(acc.des)
cuartiles.mujer
cuartiles.hombre
cuartiles.des
ric.mujer
ric.hombre
ric.des

#...forma
coe.asi.mujer = asimetria(acc.mujer)
coe.asi.hombre = asimetria(acc.hombre)
coe.asi.des = asimetria(acc.des)
coe.cur.mujer = curtosis(acc.mujer)
coe.cur.hombre = curtosis(acc.hombre)
coe.cur.des = curtosis(acc.des)
coe.asi.mujer
coe.asi.hombre
coe.asi.des
coe.cur.mujer
coe.cur.hombre
coe.cur.des

#Diagrama de caja
par(mfrow=c(1,4))

boxplot(margnl.distrito, main="Todos")
boxplot(acc.mujer, main="Mujer")
boxplot(acc.hombre, main="Hombre")
boxplot(acc.des, main="Des")

#Diagrama de barras
par(mfrow = c(1, 1))
barplot(table(acc$sexo,acc$distrito),
        xlab="ACCIDENTES MADRID 2024",
        col=rainbow(3),
        legend.text = rownames(table(acc$sexo,acc$distrito)))
