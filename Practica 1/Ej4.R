#Ej 4. Del archivo futbol.csv, obtener la suma del presupuesto de todos los equipos.
getwd()
tabla = read.csv2("futbol.csv")
tabla = na.omit(tabla)
presupuesto = tabla$PRESUPUESTO
presupuesto_total = sum(presupuesto)
(presupuesto_total)
