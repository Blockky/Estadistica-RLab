# Función para calcular el intervalo de confianza de la media poblacional
# a partir de los datos de una muestra y un nivel de confianza (entre 0 y 1)

ic.media = function(muestra, nivel.confianza) {
  alfa=1-nivel.confianza
  n=length(muestra)
  s=sd(muestra)
  m=mean(muestra)
  if (n>=30)  #Muestra grande
  {
    z=qnorm(1-alfa/2,0,1)
    li=m-z*(s/sqrt(n))
    ls=m+z*(s/sqrt(n))
  }
  else   #Muestra pequeña
  { 
    t=qt(1-alfa/2,n-1)
    li=m-t*(s/sqrt(n))
    ls=m+t*(s/sqrt(n))
  }
  c(li,ls)
}


# Función para calcular el intervalo de confianza de la varianza poblacional
# a partir de los datos de una muestra y un nivel de confianza (entre 0 y 1)

ic.varianza = function(muestra, nivel.confianza) {
  alfa=1-nivel.confianza
  n=length(muestra)
  s=sd(muestra)
  chi.li=qchisq(1-alfa/2,n-1)
  chi.ls=qchisq(alfa/2,n-1)
  li=((n-1)*s^2)/chi.li
  ls=((n-1)*s^2)/chi.ls
  c(li,ls)
}


# Función para calcular el intervalo de confianza de una proporción poblacional
# de un grupo de la población que cumple una condición,
# a partir del tamaño de un grupo (ng) que cumple la condición en una muestra,
# del tamaño de la muestra (n) y de un nivel de confianza (entre 0 y 1)

ic.proporcion = function(ng, n, nivel.confianza) {
  alfa=1-nivel.confianza
  p=ng/n
  z=qnorm(1-alfa/2,0,1)
  li=p-z*sqrt(p*(1-p)/n)
  ls=p+z*sqrt(p*(1-p)/n)
  c(li,ls)
}


# Función para calcular el intervalo de confianza de la diferencia de medias
# de dos poblaciones
# a partir de los datos de una muestra de cada población,
# de un nivel de confianza (entre 0 y 1)
# y de un parámetro que indica si se trata de datos pareados (TRUE) o no (FALSE)

ic.dif.medias = function(muestra1, muestra2, nivel.confianza, pareados=FALSE) {
  alfa=1-nivel.confianza
  if (pareados==FALSE) 
    {
      n1=length(muestra1)
      n2=length(muestra2)
      m1=mean(muestra1)
      m2=mean(muestra2)
      s1=sd(muestra1)
      s2=sd(muestra2)
      if ((n1>=30)&(n2>=30)) 
        {  
        print("pareados=FALSE, n1 & n2>=30")
        z=qnorm(1-alfa/2,0,1)
        li=m1-m2-z*sqrt((s1^2/n1)+(s2^2/n2))
        ls=m1-m2+ z*sqrt((s1^2/n1)+(s2^2/n2))
        }
      else 
        { 
        print("pareados=FALSE, n1+n2<30")
        #g=aproximación de Welch
        g=((((s1^2/n1)+(s2^2/n2))^2)/((s1^2/n1)^2/(n1+1)+(s2^2/n2)^2/(n2+1)))-2
        t=qt(1-alfa/2,g)
        li=m1-m2-t*sqrt((s1^2/n1)+(s2^2/n2))
        ls=m1-m2+ t*sqrt((s1^2/n1)+(s2^2/n2))
        }
    } 
  else 
    {  
    d=muestra1-muestra2
    m=mean(d)
    s=sd(d)
    n=length(d)
    if (n>=30) 
      { 
      print("pareados=TRUE, n>=30")
      z=qnorm(1-alfa/2,0,1)
      li=m-z*(s/sqrt(n))
      ls=m+z*(s/sqrt(n))
      }
    else 
      { 
      print("pareados=TRUE, n<30")
      t=qt(1-alfa/2,n-1)
      li=m-t*(s/sqrt(n))
      ls=m+t*(s/sqrt(n))
      }
    }
  c(li,ls)
}


# Función para calcular el intervalo de confianza de la diferencia de proporciones
# de dos poblaciones, con un grupo de cada población 
# que cumple una misma condición,
# a partir de los tamaños de las muestras de cada población (n1, n2),
# y del tamaño del grupo de cada muestra (ng1,ng2) que cumple la condición.
# y de un nivel de confianza (entre 0 y 1)

ic.dif.proporciones = function(ng, n, nivel.confianza) {
  alfa=1-nivel.confianza
  p1=ng[1]/n[1]
  p2=ng[2]/n[2]
  n1=n[1]
  n2=n[2]
  z=qnorm(1-alfa/2,0,1)
  li=p1-p2-z*sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  ls=p1-p2+z*sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2))
  c(li,ls)
}



  

