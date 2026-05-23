###############################################################################
# 1_descarga_datos.R
# Descarga de datos SAFE del BCE
# Autor: Pablo Ruiz Burgos
#
# Este script descarga los microdatos de la encuesta SAFE (Survey on the
# Access to Finance of Enterprises) directamente desde el sitio del BCE.
#
# IMPORTANTE: Ejecutar 0_setup.R antes de este script.
###############################################################################

# Limpiar la memoria
rm(list = ls())
gc()

# Cargar paquetes
library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(haven)
library(readxl)
library(ecb)
library(lubridate)

###############################################################################
# 1. DESCARGA DEL ARCHIVO DE DATOS
###############################################################################

directorio_principal <- getwd()

options(timeout = 300)

url <- "https://www.ecb.europa.eu/stats/ecb_surveys/safe/shared/pdf/ecb.SAFE_microdata.zip"

directorio_datos <- paste0(getwd(), "/datos")
directorio_metadatos <- paste0(getwd(), "/metadatos")

# Ruta completa donde se guardará
datos_zip <- file.path(directorio_datos, "ecb_SAFE_microdata.zip")

# Descargar archivo
download.file(url, destfile = datos_zip, mode = "wb")

# Descomprimir el ZIP en la misma carpeta
unzip(zipfile = datos_zip, exdir = directorio_datos)

# Eliminar el archivo zip luego de descomprimirlo
file.remove(datos_zip)

# Descargar información complementaria, útil para entender los datos
url_metadatos       <- "https://www.ecb.europa.eu/stats/pdf/surveys/sme/ecb.safemi.en.pdf"
url_metadatos_anexo <- "https://www.ecb.europa.eu/stats/pdf/surveys/sme/Annex_3.en.xlsx"

# Ruta completa donde se guardará
metadatos_pdf   <- file.path(directorio_metadatos, "SAFE_metodologia.pdf")
metadatos_anexo <- file.path(directorio_metadatos, "SAFE_metodologia_anexo.xlsx")

# Descargar archivo
download.file(url_metadatos, destfile = metadatos_pdf, mode = "wb")
download.file(url_metadatos_anexo, destfile = metadatos_anexo, mode = "wb")

###############################################################################
# 2. DESCARGA DE LA TASA DE POLÍTICA MONETARIA DEL BCE
###############################################################################

# Descargar tasas de política monetaria del BCE usando el paquete ecb
# Series: DFR (Deposit Facility Rate), MRO (Main Refinancing Operations), MLF (Marginal Lending Facility)

# Se inicia en 2017-10-01 (no en 2018-01-01) porque la wave 18, etiquetada
# como "2018H1", cubre el periodo oct-2017 a mar-2018. La logica de promedios
# en 2_limpieza_datos.R necesita el cierre de diciembre de 2017 como primer
# punto de referencia de esa wave.

# DFR (Deposit Facility Rate) es la tasa de facilidad de depósito
dfr <- get_data("FM.D.U2.EUR.4F.KR.DFR.LEV",
                filter = list(startPeriod = "2017-10-01", endPeriod = "2025-12-31"))

# Procesar datos y mantener solo observaciones de fin de trimestre
tasas_bce <- dfr %>%
  mutate(tipo_tasa = "Deposit Facility Rate") %>%
  select(fecha = obstime, tasa = obsvalue, tipo_tasa) %>%
  mutate(
    fecha = as.Date(fecha),
    tasa = as.numeric(tasa),
    mes = lubridate::month(fecha)
  ) %>%
  # Mantener solo fin de trimestre (marzo, junio, septiembre, diciembre)
  filter(mes %in% c(3, 6, 9, 12)) %>%
  # Mantener última observación de cada mes
  group_by(lubridate::year(fecha), mes) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(fecha, tasa, tipo_tasa)

# Guardar datos de tasas BCE
write.csv(tasas_bce, file.path(directorio_datos, "tasas_bce.csv"), row.names = FALSE)
