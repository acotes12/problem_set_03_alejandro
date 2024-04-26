#############################################################################################################
###-----------------------------------------------Problem Set 3-------------------------------------------###
############################################################################################################
#Alejandro Cotes 
rm(list = ls())   #limpiamos el area de trabajo
require("rio")
pacman::p_load(data.table , tidyverse,stringr, rio, ggplot2) #cargamos las librerias

#indicamos el directorio con el que vamos a trabajar o tenemos nuestras bases de datos
setwd('C:/Users/User/Downloads/ProblemSet3_AlejandroCotes')
#fijamos la ruta con la cual vamos a llamar nuestras bases de datos
rutas <- list.files("pset-3/input" , recursive=T , full.names=T)

## Punto 1.2 

## Extraer las rutas
# definimos el patron "Fuerza de trabajo", "No ocupados" o "Ocupados"
patron <- "Fuerza de trabajo|No ocupados|Ocupados"
rutas_resto <- str_subset(string = rutas , pattern = patron)

## importamos la lista que pertenece a cada uno de los meses con el patron de la ruta que fijamos
lista_resto <- import_list(file = rutas_resto)

## Punto 1.3 Combina todos los data.frame 
# Filtrar las coincidencias que contienen "Fuerza de trabajo", "Ocupado" y "No ocupado"
lista_fuerza_trabajo <- lapply(rutas_resto[grep("Fuerza de trabajo", rutas_resto)], readRDS)
lista_ocupado <- lapply(rutas_resto[grep("Ocupado", rutas_resto)], readRDS)
lista_no_ocupado <- lapply(rutas_resto[grep("No ocupado", rutas_resto)], readRDS)

# Consolidar los datos utilizando rbindlist
df_fuerza_trabajo <- rbindlist(lista_fuerza_trabajo, use.names = TRUE, fill = TRUE)
df_ocupado <- rbindlist(lista_ocupado, use.names = TRUE, fill = TRUE)
df_no_ocupado <- rbindlist(lista_no_ocupado, use.names = TRUE, fill = TRUE)

## Punto 2
## Punto 2.1
# Filtrar  FT == 1 para la fuerza de trabajo (Hacen parte de la fuerza laboral)
suma_ft_fTrabajo <- aggregate(FT ~ MES, data = df_fuerza_trabajo[df_fuerza_trabajo$FT == 1, ], FUN = length)
# Filtrar  FT == 1 para la fuerza de trabajo (Poblacion en edad de Trabajar)
suma_pet_fTrabajo <- aggregate(PET ~ MES, data = df_fuerza_trabajo[df_fuerza_trabajo$PET == 1, ], FUN = length)

## Punto 2.2
# Filtrar  PET == 1 para ocupados (Individuos Empleados)
suma_ft_ocupados <- aggregate(FT ~ MES, data = df_ocupado[df_ocupado$FT == 1, ], FUN = length)

## Punto 2.3
# Filtrar  DSI == 1 para no ocupados (Individuos desempleados)
suma_pet_no_ocupados <- aggregate(DSI ~ MES, data = df_no_ocupado[df_no_ocupado$DSI == 1, ], FUN = length)

## Punto 2 2 Merge
merged_data <- merge(suma_ft_fTrabajo, suma_pet_fTrabajo, by = "MES")
merged_data <- merge(merged_data, suma_ft_ocupados, by = "MES")
merged_data <- merge(merged_data, suma_pet_no_ocupados, by = "MES")

ruta_salida <- "C:/Users/User/Downloads/ProblemSet3_AlejandroCotes/pset-3/output/merged_data.rds"
saveRDS(merged_data, file = ruta_salida)
## Punto 2 3 
merged_data$tasa_desempleo <- merged_data$DSI/merged_data$FT.x
merged_data$tasa_ocupacion <- merged_data$FT.y/merged_data$PET


# Punto 3

# Graficar las tasas de desempleo y ocupación
ggplot(merged_data, aes(x = MES)) +
  geom_line(aes(y = tasa_desempleo, color = "Tasa de Desempleo", group = 1)) +
  geom_line(aes(y = tasa_ocupacion, color = "Tasa de Ocupación", group = 1)) +
  labs(x = "MES", y = "Tasa", color = "Variable", title = "Tasas de Desempleo y Ocupación por Mes en 2023") +
  theme_minimal()

