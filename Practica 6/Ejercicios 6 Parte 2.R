source("IC.R")
encuesta = na.omit(read.csv2("encuesta.csv"))

#1.A
encuesta.mañana=encuesta[(encuesta$GRUPO=="A1")|(encuesta$GRUPO=="A2"),]
encuesta.tarde=encuesta[(encuesta$GRUPO=="B1")|(encuesta$GRUPO=="B2"),]
viaje.mañana=encuesta.mañana$VIAJE
viaje.tarde=encuesta.tarde$VIAJE

ic.1=ic.media(viaje.mañana,0.95)
li.1=ic.1[1]
ls.1=ic.1[2]
ic.2=ic.media(viaje.tarde,0.95)
li.2=ic.2[1]
ls.2=ic.2[2]
plot(c(1,1,2,2),c(li.1,ls.1,li.2,ls.2), xlim = c(0,4))
lines(c(1,1),c(li.1,ls.1))
lines(c(2,2),c(li.2,ls.2))

ic.dif.medias(viaje.mañana,viaje.tarde,0.90)
ic.dif.medias(viaje.mañana,viaje.tarde,0.95)
ic.dif.medias(viaje.mañana,viaje.tarde,0.99)

test=t.test(viaje.mañana,viaje.tarde,conf.level=0.90)
test$conf.int
test=t.test(viaje.mañana,viaje.tarde,conf.level=0.95)
test$conf.int
test=t.test(viaje.mañana,viaje.tarde,conf.level=0.99)
test$conf.int

"No hay diferencia significativa de medias"

#2.A
viaje.ida=encuesta$VIAJE
viaje.vuelta=0.9*encuesta$VIAJE

ic.dif.medias(viaje.ida,viaje.vuelta,0.90,pareados=TRUE)
ic.dif.medias(viaje.ida,viaje.vuelta,0.95,pareados=TRUE)
ic.dif.medias(viaje.ida,viaje.vuelta,0.99,pareados=TRUE)

test=t.test(viaje.ida,viaje.vuelta,conf.level=0.90,paired=TRUE)
test$conf.int
test=t.test(viaje.ida,viaje.vuelta,conf.level=0.95,paired=TRUE)
test$conf.int
test=t.test(viaje.ida,viaje.vuelta,conf.level=0.99,paired=TRUE)
test$conf.int


#3.A
movil.mañana=encuesta.mañana$MOVIL
movil.tarde=encuesta.tarde$MOVIL
n=c(length(movil.mañana),length(movil.tarde))

movil.mañana.jazztel=movil.mañana[movil.mañana=="Jazztel/Orange/Simyo"]
movil.tarde.jazztel=movil.tarde[movil.tarde=="Jazztel/Orange/Simyo"]
ng=c(length(movil.mañana.jazztel),length(movil.tarde.jazztel))

ic.1=ic.proporcion(ng[1],n[1],0.95)
li.1=ic.1[1]
ls.1=ic.1[2]
ic.2=ic.proporcion(ng[2],n[2],0.95)
li.2=ic.2[1]
ls.2=ic.2[2]
plot(c(1,1,2,2),c(li.1,ls.1,li.2,ls.2), xlim = c(0,4))
lines(c(1,1),c(li.1,ls.1))
lines(c(2,2),c(li.2,ls.2))


ic.dif.proporciones(ng,n,0.90)
ic.dif.proporciones(ng,n,0.95)
ic.dif.proporciones(ng,n,0.99)

test=prop.test(ng,n,conf.level=0.90)
test$conf.int
test=prop.test(ng,n,conf.level=0.95)
test$conf.int
test=prop.test(ng,n,conf.level=0.99)
test$conf.int
