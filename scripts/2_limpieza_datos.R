###############################################################################
# 2_limpieza_datos.R
# Limpieza de datos y creación de variables
# Autor: Pablo Ruiz Burgos
#
# Este script:
# 1. Carga los datos SAFE descargados
# 2. Selecciona las variables relevantes para el análisis
# 3. Filtra el periodo de estudio (2018-2025)
# 4. Crea las variables derivadas necesarias para responder las preguntas
#
# IMPORTANTE: Ejecutar 0_setup.R y 1_descarga_datos.R antes de este script.
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
# 1. CARGA DE DATOS
###############################################################################

directorio_datos <- paste0(getwd(), "/datos")
archivo_datos <- file.path(directorio_datos, "safepanel_allrounds.csv")
datos_raw <- read.csv(archivo_datos)

###############################################################################
# 2. SELECCIÓN DE VARIABLES
###############################################################################

# Variables a mantener organizadas por categoría:
#
# IDENTIFICADORES Y PONDERADORES:
# - permid: Identificador único de empresa
# - wave: Número de ronda de la encuesta
# - wgtcommon: Ponderador común para representatividad
#
# CARACTERÍSTICAS DE LA EMPRESA (Sección D del cuestionario):
# - d0: País de residencia
# - d1_rec: Tamaño de empresa (número de empleados)
# - d2: Forma legal
# - d3_rec: Sector de actividad
# - d4: Ventas
# - d5_rec: Antigüedad de la empresa
# - d7: Porcentaje de ventas de exportación
#
# RELEVANCIA DE FUENTES DE FINANCIACIÓN (Q4):
# - q4_a_rec: Ganancias retenidas
# - q4_b_rec: Subvenciones/subsidios
# - q4_c_rec: Líneas de crédito bancario
# - q4_d_rec: Préstamos bancarios
# - q4_e_rec: Crédito comercial
# - q4_f_rec: Otros préstamos
# - q4_h_rec: Deuda
# - q4_j_rec: Capital
# - q4_m_rec: Leasing
# - q4_p_rec: Otros
# - q4_r_rec: Factoring
#
# OBTENCIÓN DE FINANCIACIÓN EN ÚLTIMOS 6 MESES (Q4a):
# - q4a_a: Ganancias retenidas
# - q4a_b: Subvenciones
# - q4a_c: Líneas de crédito
# - q4a_d: Préstamos bancarios
# - q4a_e: Crédito comercial
# - q4a_f: Otros préstamos
# - q4a_h: Deuda
# - q4a_j: Capital
# - q4a_m: Leasing
# - q4a_p: Otros
# - q4a_r: Factoring
#
# CAMBIO EN NECESIDAD DE FINANCIACIÓN (Q5):
# - q5_a_rec: Préstamos bancarios
# - q5_b_rec: Crédito comercial
# - q5_c_rec: Capital
# - q5_d_rec: Deuda
# - q5_f_rec: Líneas de crédito
# - q5_g_rec: Leasing
# - q5_h_rec: Otros préstamos
#
# SOLICITUD DE FINANCIACIÓN BANCARIA (Q7a):
# - q7a_a_rec: Préstamos bancarios
# - q7a_b_rec: Crédito comercial
# - q7a_c_rec: Capital
# - q7a_d_rec: Líneas de crédito
#
# RESULTADO DE SOLICITUD (Q7b):
# - q7b_a_rec: Resultado préstamos bancarios
# - q7b_b_rec: Resultado crédito comercial
# - q7b_c_rec: Resultado capital
# - q7b_d_rec: Resultado líneas de crédito
#
# CAMBIO EN DISPONIBILIDAD DE FINANCIACIÓN (Q9):
# - q9_a_rec: Préstamos bancarios
# - q9_b_rec: Crédito comercial
# - q9_c_rec: Capital
# - q9_d_rec: Deuda
# - q9_f_rec: Líneas de crédito
# - q9_g_rec: Leasing
# - q9_h_rec: Otros préstamos

variables_seleccionadas <- c(
  # Identificadores
  "permid", "wave", "wgtcommon",
  
  # Características empresa
  "d0", "d1_rec", "d2", "d3_rec", "d4", "d5_rec", "d7",
  
  # Relevancia fuentes (Q4)
  "q4_a_rec", "q4_b_rec", "q4_c_rec", "q4_d_rec", "q4_e_rec", "q4_f_rec",
  "q4_h_rec", "q4_j_rec", "q4_m_rec", "q4_p_rec", "q4_r_rec",
  
  # Obtención financiación (Q4a)
  "q4a_a", "q4a_b", "q4a_c", "q4a_d", "q4a_e", "q4a_f",
  "q4a_h", "q4a_j", "q4a_m", "q4a_p", "q4a_r",
  
  # Cambio necesidad (Q5)
  "q5_a_rec", "q5_b_rec", "q5_c_rec", "q5_d_rec", "q5_f_rec", "q5_g_rec", "q5_h_rec",
  
  # Solicitud (Q7a)
  "q7a_a_rec", "q7a_b_rec", "q7a_c_rec", "q7a_d_rec",
  
  # Resultado solicitud (Q7b)
  "q7b_a_rec", "q7b_b_rec", "q7b_c_rec", "q7b_d_rec",
  
  # Cambio disponibilidad (Q9)
  "q9_a_rec", "q9_b_rec", "q9_c_rec", "q9_d_rec", "q9_f_rec", "q9_g_rec", "q9_h_rec"
)

# Seleccionar variables
datos <- datos_raw %>% select(all_of(variables_seleccionadas))

# Liberar memoria
rm(datos_raw); gc()

###############################################################################
# 3. FILTRO DE PERIODO DE ESTUDIO
###############################################################################

# NOTA: Waves 31, 33 y 35 se eliminan porque no incluyen pregunta Q7A sobre préstamos
waves_estudio <- c(18:29, 30, 32, 34, 36)

# Ordenar datos
datos <- datos %>% arrange(permid, wave)

# Filtrar waves del periodo de estudio
datos <- datos %>% filter(wave %in% waves_estudio)

###############################################################################
# 4. CREACIÓN DE VARIABLE: RELEVANCIA DE BANCA
###############################################################################

# Para el análisis nos enfocamos en empresas para las cuales la financiación
# bancaria es relevante. Esto significa que reportan que las líneas de crédito
# o los préstamos bancarios son relevantes para su negocio.
#
# Códigos de respuesta Q4 (relevancia):
# 1 = Muy relevante
# 2 = Bastante relevante
# 3 = No muy relevante (pero existe)
# 7 = No aplicable (no usa esta fuente)
# 9 = No sabe
#
# Consideramos relevante si q4_c_rec o q4_d_rec están en {1, 2, 3}
# Excluimos si ambas son 7 (no aplicable) o 9 (no sabe)

datos <- datos %>%
  mutate(
    bancos_relevantes = case_when(
      (q4_c_rec %in% c(7, 9)) & (q4_d_rec %in% c(7, 9)) ~ 0,
      TRUE ~ 1
    )
  )

# Distribucion de esta variable
print(table(datos$bancos_relevantes))

###############################################################################
# 5. CREACIÓN DE VARIABLES: RESTRICCIONES FINANCIERAS
###############################################################################

# Variable principal: empresa_restringida_financieramente
#
# Una empresa está restringida financieramente si experimenta alguno de estos obstáculos
# al intentar obtener un préstamo bancario:
#
# 1. Costo excesivo: Solicitó, recibió oferta pero rechazó por costo (q7a_a_rec=1 y q7b_a_rec=3)
# 2. Rechazada: Solicitó pero fue rechazada (q7a_a_rec=1 y q7b_a_rec=4)
# 3. Desalentada: No solicitó por temor a rechazo (q7a_a_rec=2)
# 4. Racionada: Solicitó pero solo recibió cantidad limitada (q7a_a_rec=1 y q7b_a_rec=6)
#
# Códigos Q7a (solicitud préstamos bancarios):
# 1 = Solicitó
# 2 = No solicitó por temor a rechazo (desalentada)
# 3 = No solicitó porque tenía fondos suficientes
# 4 = No solicitó por otras razones
# 9 = No sabe / no responde
#
# Códigos Q7b (resultado de solicitud):
# 1 = Recibió todo lo solicitado
# 2 = Recibió la mayor parte
# 3 = Recibió oferta pero rechazó por costo elevado
# 4 = Solo recibió cantidad limitada
# 5 = Solicitud pendiente
# 6 = Rechazada
# 9 = No sabe / no responde

datos <- datos %>%
  mutate(
    # Tipo de obstáculo para PRÉSTAMOS BANCARIOS
    # Siguiendo metodología BCE: explícitamente definir constrained, unconstrained, y missing
    obstaculos_obtencion_prestamo_bancario = case_when(
      # CONSTRAINED (valores 1-4):
      # 1: Costo excesivo - solicitó, recibió oferta, rechazó por costo
      (q7b_a_rec == 3) ~ 1,
      # 2: Rechazada - solicitó y fue rechazada
      (q7b_a_rec == 4) ~ 2,
      # 3: Desalentada - no solicitó por temor a rechazo
      (q7a_a_rec == 2) ~ 3,
      # 4: Racionada - solicitó, solo recibió cantidad limitada
      (q7b_a_rec == 6) ~ 4,
      
      # No solicitó por fondos suficientes o por otras razones
      (q7a_a_rec %in% c(3, 4)) ~ 0,
      # Recibió todo lo solicitado o al menos 75%
      (q7b_a_rec %in% c(1, 5)) ~ 0,
      
      # MISSING: todo lo demás (don't know, pending, etc.)
      TRUE ~ NA_real_
    ),
    
    # Tipo de obstáculo para LÍNEAS DE CRÉDITO
    obstaculos_obtencion_linea_credito = case_when(
      # CONSTRAINED (valores 1-4):
      (q7b_d_rec == 3) ~ 1,  # Costo excesivo
      (q7b_d_rec == 4) ~ 2,  # Rechazada
      (q7a_d_rec == 2) ~ 3,  # Desalentada
      (q7b_d_rec == 6) ~ 4,  # Racionada
      
      # UNCONSTRAINED (valor 0):
      (q7a_d_rec %in% c(3, 4)) ~ 0,
      (q7b_d_rec %in% c(1, 5)) ~ 0,
      
      # MISSING
      TRUE ~ NA_real_
    ),
    
    # Variable binaria: restricción en préstamos bancarios
    restringida_prestamos = case_when(
      obstaculos_obtencion_prestamo_bancario > 0 ~ 1,
      obstaculos_obtencion_prestamo_bancario == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Variable binaria: restricción en líneas de crédito
    restringida_linea_credito = case_when(
      obstaculos_obtencion_linea_credito > 0 ~ 1,
      obstaculos_obtencion_linea_credito == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Variable combinada: restricción en CUALQUIER financiación bancaria
    # (préstamos O líneas de crédito)
    empresa_restringida_financieramente = case_when(
      # Restringida si tiene restricción en préstamos O en líneas de crédito
      (restringida_prestamos == 1 | restringida_linea_credito == 1) ~ 1,
      # No restringida si no tiene restricción en NINGUNO (y tiene info de al menos uno)
      (restringida_prestamos == 0 & restringida_linea_credito == 0) ~ 0,
      TRUE ~ NA_real_
    )
  )


# Distribución de la variable de obstaculos
table(datos$obstaculos_obtencion_prestamo_bancario)

# Número de empresas que reportan estar restringidas financieramente
table(datos$empresa_restringida_financieramente)

###############################################################################
# 6. CREACIÓN DE VARIABLES: CAMBIOS EN NECESIDAD Y DISPONIBILIDAD
###############################################################################

# Estas variables capturan la percepción de cambio en los últimos 6 meses
#
# Códigos Q5 (cambio en necesidad) y Q9 (cambio en disponibilidad):
# 1 = Aumentó
# 2 = Se mantuvo igual
# 3 = Disminuyó
# 7 = No aplicable
# 9 = No sabe

# Función para recodificar cambios a escala numérica (-1, 0, 1)
recodificar_cambio <- function(x) {
  case_when(
    x == 1 ~ 1,   # Aumentó
    x == 2 ~ 0,   # Sin cambio
    x == 3 ~ -1,  # Disminuyó
    x %in% c(7, 9) ~ NA_real_  # No aplicable / No sabe
  )
}

datos <- datos %>%
  mutate(
    # Cambio en necesidad de financiación
    cambio_necesidad_prestamos = recodificar_cambio(q5_a_rec),
    cambio_necesidad_linea_credito = recodificar_cambio(q5_f_rec),
    cambio_necesidad_credito_comercial = recodificar_cambio(q5_b_rec),
    cambio_necesidad_capital = recodificar_cambio(q5_c_rec),
    cambio_necesidad_deuda = recodificar_cambio(q5_d_rec),
    cambio_necesidad_leasing = recodificar_cambio(q5_g_rec),
    
    # Cambio en disponibilidad de financiación
    cambio_disponibilidad_prestamos = recodificar_cambio(q9_a_rec),
    cambio_disponibilidad_linea_credito = recodificar_cambio(q9_f_rec),
    cambio_disponibilidad_credito_comercial = recodificar_cambio(q9_b_rec),
    cambio_disponibilidad_capital = recodificar_cambio(q9_c_rec),
    cambio_disponibilidad_deuda = recodificar_cambio(q9_d_rec),
    cambio_disponibilidad_leasing = recodificar_cambio(q9_g_rec)
  )

###############################################################################
# 7. CREACIÓN DE VARIABLES: BRECHA FINANCIERA
###############################################################################

# La "brecha" captura el desajuste entre necesidad y disponibilidad
# Brecha positiva = necesidad aumenta más que disponibilidad
# Brecha negativa = disponibilidad aumenta más que necesidad

datos <- datos %>%
  mutate(
    brecha_prestamos = case_when(
      # necesidad aumenta Y disponibilidad disminuye
      (cambio_necesidad_prestamos == 1 & cambio_disponibilidad_prestamos == -1) ~ 1,
      
      # solo uno de los dos
      (cambio_necesidad_prestamos == 1 & cambio_disponibilidad_prestamos != -1) ~ 0.5,
      (cambio_necesidad_prestamos != 1 & cambio_disponibilidad_prestamos == -1) ~ 0.5,
      
      # necesidad disminuye y disponibilidad aumenta
      (cambio_necesidad_prestamos == -1 & cambio_disponibilidad_prestamos == 1) ~ -1,
      
      # necesidad disminuye o disponibilidad aumenta
      (cambio_necesidad_prestamos == -1 & cambio_disponibilidad_prestamos != 1) ~ -0.5,
      (cambio_necesidad_prestamos != -1 & cambio_disponibilidad_prestamos == 1) ~ -0.5,
      
      # Equilibrio o NA
      (is.na(cambio_necesidad_prestamos) | is.na(cambio_disponibilidad_prestamos)) ~ NA_real_,
      TRUE ~ 0
    )
  )

###############################################################################
# 8. RECODIFICACIÓN DE VARIABLES DE CARACTERÍSTICAS
###############################################################################

# Crear versiones con etiquetas legibles de las variables de características
datos <- datos %>%
  mutate(
    # Tamaño de empresa (d1_rec)
    tamano_empresa = case_when(
      d1_rec == 1 ~ "Micro (1-9)",
      d1_rec == 2 ~ "Pequeña (10-49)",
      d1_rec == 3 ~ "Mediana (50-249)",
      d1_rec == 4 ~ "Grande (250+)",
      TRUE ~ NA_character_
    ),
    tamano_empresa = factor(tamano_empresa,
                            levels = c("Micro (1-9)", "Pequeña (10-49)",
                                       "Mediana (50-249)", "Grande (250+)")),
    
    # Antigüedad (d5_rec)
    antiguedad = case_when(
      d5_rec == 1 ~ "10+ años",
      d5_rec == 2 ~ "5-9 años",
      d5_rec == 3 ~ "2-4 años",
      d5_rec == 4 ~ "< 2 años",
      TRUE ~ NA_character_
    ),
    antiguedad = factor(antiguedad,
                        levels = c("10+ años", "5-9 años", "2-4 años", "< 2 años")),

    # Evolución de ventas (d4)
    # Códigos SAFE: 2=Aumentaron, 3=Sin cambio, 4=Disminuyeron
    ventas = case_when(
      d4 == 2 ~ "2-10 mill",
      d4 == 3 ~ "10-50 mill",
      d4 == 4 ~ "50+ mill",
      d4 == 5 ~ "0-0,5 mill",
      d4 == 6 ~ "0,5-1 mill",
      d4 == 7 ~ "1-2 mill",
      TRUE ~ NA_character_
    ),
    pais = d0   
  )

###############################################################################
# 9. AÑADIR FECHAS A LAS WAVES
###############################################################################

wave_fecha <- read.csv(paste0(directorio_datos,"/wave_fecha.csv"))

# Unir con la tabla de correspondencia wave-fecha
datos <- datos %>%
  left_join(wave_fecha, by = "wave")

###############################################################################
# 10: PYME, SUBPERIODO Y TASA BCE POR WAVE
###############################################################################

# 10.1 Variable binaria PYME vs Grande
#      Definición oficial UE / BCE:
#        PYME  = menos de 250 empleados. Es decir, d1_rec pertenece a {1, 2, 3}
#        Grande = 250 o más empleados. Es decir, d1_rec == 4

datos <- datos %>%
  mutate(
    es_pyme = case_when(
      d1_rec %in% c(1L, 2L, 3L) ~ 1L,
      d1_rec == 4L ~ 0L,
      TRUE ~ NA_integer_
    ),
    tipo_empresa = factor(
      es_pyme,
      levels = c(0L, 1L),
      labels = c("Grande (250+)", "PYME (<250)")
    )
  )

# Comporbación: distribución absoluta y porcentaje no ponderado
cat("\n\t Distribución PYME vs Grande (obs. no ponderadas) \n")
print(table(datos$tipo_empresa, useNA = "ifany")); cat("\n")
print(round(prop.table(table(datos$tipo_empresa, useNA = "ifany")) * 100, 1))

# 10.2 Variable subperiodo del ciclo monetario (factor ordenado)
#      Laxa: 2018-2021
#      Endurecimiento: 2022-2023
#      Relajación: 2024-2025

datos <- datos %>%
  mutate(
    periodo_monetario = case_when(
      año %in% 2018:2021 ~ "1_lax",
      año %in% 2022:2023 ~ "2_tightening",
      año %in% 2024:2025 ~ "3_easing",
      TRUE               ~ NA_character_
    ),
    periodo_monetario = factor(
      periodo_monetario,
      levels  = c("1_lax", "2_tightening", "3_easing"),
      labels  = c("Política laxa (2018-2021)",
                  "Endurecimiento (2022-2023)",
                  "Relajación (2024-2025)")
    )
  )

cat("\n\t Observaciones por subperiodo monetario \n")
print(table(datos$periodo_monetario, useNA = "ifany"))

# 10.3 Tasa BCE (DFR) por wave
#
# Cada wave cubre un semestre de referencia con dos cierres de trimestre:
#   Waves H1 (Oct Y-1 → Mar Y): promedio DFR de Dic(Y-1) y Mar(Y)
#   Waves H2 (Apr Y  → Sep Y): promedio DFR de Jun(Y) y Sep(Y)
#
# El join se hace por año + mes (para evitar variaciones en el día exacto
# que devuelve slice_tail en la descarga del BCE)

# Cargar tasas BCE y meter columnas de año y mes para el join
dfr_data <- read.csv(file.path(directorio_datos, "tasas_bce.csv")) %>%
  mutate(
    fecha     = as.Date(fecha),
    dfr_year  = lubridate::year(fecha),
    dfr_month = lubridate::month(fecha)
  ) %>%
  select(dfr_year, dfr_month, dfr_rate = tasa)

# Construir tabla wave (dos trimestres de referencia)
wave_dfr_map <- wave_fecha %>%
  mutate(
    # Extraer semestre ("H1" o "H2") de la columna fecha
    semester = stringr::str_extract(fecha, "H[12]"),
    
    # Año y mes del primer trimestre del periodo de referencia
    q1_year  = if_else(semester == "H1", año - 1L, año),
    q1_month = if_else(semester == "H1", 12L, 6L),
    
    # Año y mes del segundo trimestre del periodo de referencia
    q2_year  = año,
    q2_month = if_else(semester == "H1", 3L, 9L)
  ) %>%
  # Join con DFR para el primer trimestre
  left_join(dfr_data,
            by = c("q1_year" = "dfr_year", "q1_month" = "dfr_month")) %>%
  rename(dfr_q1 = dfr_rate) %>%
  
  # Join con DFR para el segundo trimestre
  left_join(dfr_data,
            by = c("q2_year" = "dfr_year", "q2_month" = "dfr_month")) %>%
  rename(dfr_q2 = dfr_rate) %>%
  
  # Promedio de los dos trimestres
  mutate(dfr_wave_avg = (dfr_q1 + dfr_q2) / 2) %>%
  
  select(wave, dfr_wave_avg, dfr_q1, dfr_q2)

# Verificar que no hay waves sin tasa asignada
waves_missing_dfr <- wave_dfr_map %>% filter(is.na(dfr_wave_avg))
if (nrow(waves_missing_dfr) > 0) {
  cat("\n WARNING: Las siguientes waves no tienen tasa BCE asignada: \n")
  print(waves_missing_dfr)
} else {
  cat("\n OK: Todas las waves tienen tasa BCE asignada \n")
}

cat("\n\t Tasa BCE promedio por wave \n")
print(wave_dfr_map %>% select(wave, dfr_q1, dfr_q2, dfr_wave_avg))

# Unir al dataframe principal (solo la columna a usar en regresiones)
datos <- datos %>%
  left_join(wave_dfr_map %>% select(wave, dfr_wave_avg),
            by = "wave") %>%
  rename(tasa_dfr = dfr_wave_avg)

cat("\n\t Verificación final: estadísticos de tasa_dfr \n")
print(summary(datos$tasa_dfr))
cat(sprintf("Observaciones con NA en tasa_dfr: %d\n", sum(is.na(datos$tasa_dfr))))

###############################################################################
# 11. GUARDAR DATOS PROCESADOS
###############################################################################

output_file <- file.path(directorio_datos, "datos_procesados.rds")
saveRDS(datos, output_file)
cat(sprintf("\n OK: Datos guardados en: %s\n", output_file))
cat(sprintf("  Dimensiones finales: %d filas × %d columnas\n",
            nrow(datos), ncol(datos)))
