###############################################################################
# 0_setup.R
# Configuración inicial del proyecto
# Autor: Pablo Ruiz Burgos
#
# Este script configura el entorno de trabajo para el análisis del TFG:
###############################################################################

# Limpiar la memoria
rm(list = ls())
gc()

###############################################################################
# 1. CONFIGURACIÓN DE DIRECTORIOS
###############################################################################

# Directorio principal = carpeta del proyecto
directorio_principal <- getwd()

# Definir subdirectorios
directorio_datos     <- file.path("datos")
directorio_metadatos <- file.path("metadatos")
directorio_tablas    <- file.path("resultados", "tablas")
directorio_graficos  <- file.path("resultados", "graficos")

# Crear subdirectorios si no existen
for (dir in c(directorio_datos, directorio_metadatos,
              directorio_tablas, directorio_graficos)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message(paste("Directorio creado:", dir))
  }
}

###############################################################################
# 2. INSTALACIÓN Y CARGA DE PAQUETES
###############################################################################

# Lista de paquetes necesarios (solo necesitan ser instalados la primera vez que se usan)

# install.packages("tidyverse")
# install.packages("sandwich")
# install.packages("lmtest")
# install.packages("ggplot2")
# install.packages("haven")
# install.packages("readxl")
# install.packages("ecb")
# install.packages("lubridate")

# Cargar paquetes
library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(haven)
library(readxl)

###############################################################################
# 3. PARÁMETROS GLOBALES DEL ANÁLISIS
###############################################################################

# Fechas sacadas de "https://www.ecb.europa.eu/stats/ecb_surveys/safe/html/all-releases.en.html"

# NOTA: A partir de 2024, el BCE cambió a encuestas trimestrales. Sin embargo,
# las waves Q2 y Q4 (31, 33 y 35) usan un cuestionario reducido que NO incluye
# la pregunta Q7A sobre solicitud de préstamos bancarios. Por lo tanto:

waves_estudio <- c(18:29, 30, 32, 34, 36)  # Excluir waves 31, 33 y 35 (sin datos Q7A)

# Crear correspondencia wave-fecha para etiquetas en gráficos
wave_fecha <- data.frame(
  wave = waves_estudio,
  fecha = c("2018H1", "2018H2", "2019H1", "2019H2", "2020H1", "2020H2", "2021H1", "2021H2",
            "2022H1", "2022H2", "2023H1", "2023H2", "2024H1", "2024H2", "2025H1", "2025H2"),
  año    =  c(rep(2018:2025, each = 2))
)

write.csv(wave_fecha, paste0(directorio_datos,"/wave_fecha.csv"), row.names=FALSE, quote=FALSE)
