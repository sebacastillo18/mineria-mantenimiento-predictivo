# ==============================================================================
# 1. PREPARACIÓN Y VERIFICACIÓN DE DATOS (Nacho)
# ==============================================================================
library(dplyr)
library(ggplot2)

# Carga de datos (busca el archivo en la carpeta 'data' del repositorio)
datos <- read.csv("../data/dataset_mantenimiento_minero.csv")

# Diagnóstico inicial (Dimensiones, tipos de datos y valores faltantes)
dim(datos)
str(datos)
summary(datos)
colSums(is.na(datos))

# Ajuste de tipos de datos (factores para clasificación y agrupaciones)
datos <- datos %>%
  mutate(
    falla = as.factor(falla),
    tipo_equipo = as.factor(tipo_equipo)
  )

# Partición de datos (70% Train / 30% Test)
set.seed(123) # Semilla para trazabilidad y reproducibilidad
n_total <- nrow(datos)
indices_train <- sample(1:n_total, size = 0.7 * n_total)

train <- datos[indices_train, ]
test <- datos[-indices_train, ]
