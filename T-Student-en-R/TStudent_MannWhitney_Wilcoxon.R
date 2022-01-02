# Instrucciones para elegir entre pruebas estadisticas para dos muestras: 
# T de Student, Wilcoxon, U de Mann-Whitney.


# Establecer directorio de trabajo y checar que sea el correcto
setwd(".")
getwd()
dir()


# Importar bases de datos con las que se va a trabajar
# Modificadas de Crawley (2013)
ozono = read.table("ozono.txt", header=T)


# PRUEBA DE T DE STUDENT INDEPENDIENTE
# 1. Dos muestras seran comparadas,
# 2. Varianzas iguales (homocedasticidad),
# 3. Datos tienen distribucion normal
# 4. Las muestras son independientes
t.test(ozono$jardinA, ozono$jardinB)
# La cantidad de ozono medida en los jardines A y B
# varia significativamente (t = -3.873, gl = 18, P = 0.0011)


# PRUEBA DE T DE STUDENT PAREADA
# 1. Dos muestras seran comparadas,
# 2. Varianzas iguales (homocedasticidad),
# 3. Datos tienen distribucion normal
# 4. Las muestras son dependientes
t.test(ozono$jardinA, ozono$jardinB, paired = TRUE)
# La cantidad de ozono medida en los jardines A y B
# varia significativamente (t = -6.7082, gl = 9, P < 0.0001)


# Importar bases de datos con las que se va a trabajar
# Modificadas de Crawley (2013)
rio = read.table("rio.txt", header=T)


# U DE MANN-WHITNEY (PRUEBA DE SUMA DE RANGOS DE WILCOXON)
# 1. Dos muestras seran comparadas,
# 2. Varianzas iguales (homocedasticidad),
# 3. Datos NO tienen distribucion normal,
# 4. Las muestras son independientes
wilcox.test(rio$abajo, rio$arriba)
# Los datos obtenidos rio abajo son iguales a los datos
# obtenidos rio arriba (W = 112, P = 0.5559)


# PRUEBA DE RANGOS CON SIGNO DE WILCOXON
# 1. Dos muestras seran comparadas,
# 2. Varianzas iguales (homocedasticidad),
# 3. Datos NO tienen distribucion normal
# 4. Las muestras son dependientes
wilcox.test(rio$abajo, rio$arriba, paired = TRUE)
# Las medidas tomadas corriente abajo difieren
# significativamente de las medidas tomadas 
# corriente arriba en el mismo rio (V = 8, P = 0.01406)


# ______________________________________
# Referencias
# Crawley, M. J. (2013). The R book. 2nd Ed. John Wiley & Sons.
# Crawley, M. (2015). Statistics: an introduction using R. 2 Ed. West Sussex, UK: John Wiley & Sons Ltd. 357 pp.