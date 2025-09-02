#Notación científica
options(scipen=100)

#Funciones
P = function(m){
  factorial(m)
}
PR = function(m,n1,n2){
  factorial(m)/(factorial(n1)*factorial(n2))
}
V = function(m,n){
  factorial(m)/factorial(m-n)
}
VR = function(m,n){
  m^n
}
C = function(m,n){
  choose(m,n)
}
CR = function(m,n){
  choose(m+n-1,n)
}

#Ejercicio 1.a
a1.casos.favorables = 1
a1.casos.posibles = P(3)
a1.prob = a1.casos.favorables/a1.casos.posibles
print(a1.prob)

#Ejercicio 1.b
b1.casos.favorables = 1
b1.casos.posibles = VR(3,3)
b1.prob = b1.casos.favorables/b1.casos.posibles
print(b1.prob)

#Ejercicio 1.c
"El primer suceso (1.a) es más probable (0.167 > 0.037)"

#Ejercicio 2.a
a2.casos.favorables = V(6,2)
a2.casos.posibles = VR(6,2)
a2.prob = a2.casos.favorables/a2.casos.posibles
print(a2.prob)

#Ejercicio 2.b
b2.prob.distintos = a2.prob
b2.prob.iguales = 1 - b2.prob.distintos
print(b2.prob.iguales)

#Ejercicio 2.c
"El primer suceso (2.a) es más probable (0.833 > 0.167)"

#Ejercicio 3.a
a3.casos.favorables = 1
a3.casos.posibles = C(40,4)
a3.prob = a3.casos.favorables/a3.casos.posibles
print(a3.prob)

#Ejercicio 3.b
b3.casos.favorables = 1 + 48*C(4,3)
b3.casos.posibles = C(52,4)
b3.prob = b3.casos.favorables/b3.casos.posibles
print(b3.prob)

#Ejercicio 3.c
"El segundo suceso (3.b) es más probable (0.0007 > 0.00001)"
