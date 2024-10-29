# CARGA DE BASE (CSV)----------

# Cargar Readr para la lectura de la base 
library(readr)

# Asignar ruta de acceso - CAMBIAR DEPENDIENDO DE LA UBICACIÓN
ruta_base <- "C:/Users/manue/Desktop/PYE2/garments_worker_productivity.csv"

# Carga base, vista de datos
datos_base <- read.csv(ruta_base, header = TRUE, sep = ",", stringsAsFactors = FALSE)
View(datos_base)


# Limpiar la base----------
# Eliminacion de instancias que cuentan con datos faltantes en la variable wip
library(dplyr)
datos_base <- datos_base %>% filter(!is.na(wip)) # ojo que quita la mitad de las instancias


