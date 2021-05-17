#Bases de datos

ozono = read.table("./f.test.data.txt", header=T)
jardines = read.table("./gardens.txt", header=T)
refugio = read.table("./refuge.txt", header=T)

#Partimos del supuesto de que TODAS las muestras cuyas varianzas vamos a comparar, tienen una distribución normal. Si una de éstas NO tiene una distribución normal, no tiene sentido corroborar si sus varianzas son iguales. Ya que los análisis estadísticos como ANOVA, necesitan que todas las muestras evaluadas posean una distribución normal Y varianzas iguales.

#Varianza
ozono
var(ozono$gardenB)
var(ozono$gardenC)

#Grados de libertad (degrees of freedom, df)
length(ozono$gardenB)-1
length(ozono$gardenC)-1

#Prueba de F de Fisher (2 muestras)
var.test(ozono$gardenB, ozono$gardenC)

var(ozono$gardenB)/var(ozono$gardenC)

#Las varianzas del ozono entre los jardines B y C son diferentes (F=0.0937; P=0.0016)

#Pruebas para más de 2 muestras.
#Observamos la base de datos
jardines
#Creamos los vectores de la variable de respuesta (ozono.jar) y de la variable explicativa (tipo.jar)
ozono.jar = c(jardines$gardenA, jardines$gardenB, jardines$gardenC)
tipo.jar = factor(rep(c("A","B","C"), c(10,10,10)))
#Queremos saber si la variación en la concentración de ozono difiere entre los 3 jardines.

#Prueba de Bartlett
bartlett.test(ozono.jar~tipo.jar)
#Las varianzas en la concentración de ozono entre los jardines son diferentes (K=16.758; P=0.0002)

#Prueba de Fligner-Killeen
fligner.test(ozono.jar~tipo.jar)
#Las varianzas son iguales (X^2=1.8; P=0.4)

#¿Por qué tenemos resultados diferentes? 
#La prueba de Bartlett es sensible a los datos atípicos, la de Fligner-Killeen no (Crawley, 2013). 

#¿A quién le hacemos caso?
#A la literatura: se sabe que valores de una concentración de ozono mayores a 8 partes por cien millones son dañinas (Crawley, 2013). El jardín C es el único que experimentó concentraciones mayores a 8, por lo tanto esta diferencia biológica es lo suficientemente importante, como para merecer ser estadísticamente significativa. De ahí, que lo más certero sería escoger el resultado de la prueba de Bartlett.

#Esto puede variar, ya que, usualmente, la prueba de Fligner-Killeen se prefiere por sobre la de Bartlett, para más información consulta el libro de Crawley (2013). Vínculo para su descarga gratuita en la descripción del video.