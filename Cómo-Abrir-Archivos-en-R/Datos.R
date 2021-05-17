#Abrir base de datos delimitada por tabulaciones (.txt)

datos <- read.table("./datos.txt", header = T, sep = "\t")

datos

#Abrir base de datos delimitada por comas (.csv)

lombrices <- read.csv("./datos.csv", header = T, sep = ",")

lombrices

#Abrir base de datos de excel (.xls .xlsx)

install.packages("readxl")
library("readxl")

lombrices =  read_excel("./datos.xlsx", col_names = T, sheet = "Datos")

lombrices


#Abrir base de datos que copié con Ctrl+C

lombrices2 = read.table("clipboard")

lombrices2

#NOTA: R reconoce al signo "=" y al signo "<-" indistintamente. 