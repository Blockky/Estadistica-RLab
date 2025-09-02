source("IC.R")
encuesta = na.omit(read.csv2("encuesta.csv"))
viaje=encuesta$VIAJE

#1.A
ic.media(viaje,0.90)
ic.media(viaje,0.95)
ic.media(viaje,0.99)

test=t.test(viaje, conf.level = 0.90)
test$conf.int
test=t.test(viaje, conf.level = 0.95)
test$conf.int
test=t.test(viaje, conf.level = 0.99)
test$conf.int

#1.B
ic.varianza(viaje,0.90)
ic.varianza(viaje,0.95)
ic.varianza(viaje,0.99)

#2.A
encuesta.A1=encuesta[encuesta$GRUPO=="A1",]
viaje.A1=encuesta.A1$VIAJE

ic.media(viaje.A1,0.90)
ic.media(viaje.A1,0.95)
ic.media(viaje.A1,0.99)

test=t.test(viaje.A1, conf.level = 0.90)
test$conf.int
test=t.test(viaje.A1, conf.level = 0.95)
test$conf.int
test=t.test(viaje.A1, conf.level = 0.99)
test$conf.int

#2.B
ic.varianza(viaje.A1,0.90)
ic.varianza(viaje.A1,0.95)
ic.varianza(viaje.A1,0.99)

#3.A
movil=encuesta$MOVIL
n=length(movil)

movil.jazztel=movil[movil=="Jazztel/Orange/Simyo"]
ng=length(movil.jazztel)

ic.proporcion(ng,n,0.90)
ic.proporcion(ng,n,0.95)
ic.proporcion(ng,n,0.99)

test=prop.test(ng, n, conf.level = 0.90)
test$conf.int
test=prop.test(ng, n, conf.level = 0.95)
test$conf.int
test=prop.test(ng, n, conf.level = 0.99)
test$conf.int

#3.B
(estudiantes.jazztel = 108*ic.proporcion(ng,n,0.90))
(estudiantes.jazztel = 108*ic.proporcion(ng,n,0.95))
(estudiantes.jazztel = 108*ic.proporcion(ng,n,0.99))
