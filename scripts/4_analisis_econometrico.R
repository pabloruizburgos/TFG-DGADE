###############################################################################
# 4_analisis_econometrico.R
# Análisis econométrico para la pregunta de investigación 2
# Autor: Pablo Ruiz Burgos
#
# Metodología:
# - Modelo de Probabilidad Lineal (LPM)
# - Errores estándar robustos HC1 (corrección heterocedasticidad)
# - Ponderadores de encuesta (wgtcommon)
# - Variable dependiente: empresa_restringida_financieramente (0/1)
#
# Modelos estimados:
#   Modelo 1: LPM base (tamaño + antigüedad)
#   Modelo 2: Modelo 1 + efectos fijos de país
#   Modelo 3: Modelo 2 + subperiodo monetario
#   Modelo 4: Modelo 2 + subperiodo + interacción es_pyme × periodo_monetario
#   Modelo 5: Modelo 3 estimado por separado para PYMEs y grandes
#   Modelo 6: DFR continua × es_pyme
#
# IMPORTANTE: Ejecutar 0_setup.R, 1_descarga_datos.R y 2_limpieza_datos.R antes.
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
library(lubridate)

###############################################################################
# 1. CARGA DE DATOS PROCESADOS
###############################################################################

directorio_datos    <- paste0(getwd(), "/datos")
directorio_tablas   <- paste0(getwd(), "/resultados/tablas")
directorio_graficos <- paste0(getwd(), "/resultados/graficos")

datos <- readRDS(file.path(directorio_datos, "datos_procesados.rds"))

# Filtrar muestra de regresión:
# Solo empresas para las que la banca es relevante
# Solo observaciones con variable dependiente existente
datos_regresion <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(empresa_restringida_financieramente)) %>%
  filter(!is.na(es_pyme)) %>%
  filter(!is.na(periodo_monetario))

cat(sprintf("Muestra de regresión: %d observaciones\n", nrow(datos_regresion)))

###############################################################################
# 2. PREPARACIÓN DE VARIABLES PARA REGRESIÓN
###############################################################################

# Crear variables dummy/factor para las regresiones
datos_regresion <- datos_regresion %>%
  mutate(
    # Tamaño de empresa (referencia: Micro)
    tamano_pequena = as.numeric(d1_rec == 2),
    tamano_mediana = as.numeric(d1_rec == 3),
    tamano_grande  = as.numeric(d1_rec == 4),
    
    # Antigüedad (referencia: < 2 años)
    antiguedad_2_4 = as.numeric(d5_rec == 3),
    antiguedad_5_9 = as.numeric(d5_rec == 2),
    antiguedad_10mas = as.numeric(d5_rec == 1),
    
    # Factores para efectos fijos
    factor_pais = factor(d0),
    factor_wave = factor(wave),
    factor_sector = factor(d3_rec),
    
    # periodo_monetario con referencia explícita: fase laxa 2018-2021
    periodo_monetario = relevel(periodo_monetario, ref = "Política laxa (2018-2021)")
  )

###############################################################################
# 3. FUNCIÓN AUXILIAR: estimar LPM con errores HC1
###############################################################################

# Encapsula la estimación y el coeftest con HC1 para no repetir código
estimar_lpm <- function(formula, datos, pesos = NULL) {
  if (!is.null(pesos)) {
    datos$.w <- pesos  # Añadir pesos como columna dentro del dataframe
    modelo <- lm(formula, data = datos, weights = .w)
  } else {
    modelo <- lm(formula, data = datos)
  }
  resultado <- coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))
  list(modelo = modelo, resultado = resultado)
}

###############################################################################
# 4. MODELO 1: LPM BASE (tamaño + antigüedad)
###############################################################################

cat("\n\t MODELO 1: LPM base \n")

# Especificación del modelo
formula_m1 <- empresa_restringida_financieramente ~
  tamano_pequena + tamano_mediana + tamano_grande +
  antiguedad_2_4 + antiguedad_5_9 + antiguedad_10mas

m1 <- estimar_lpm(formula_m1, datos_regresion, pesos = datos_regresion$wgtcommon)
print(m1$resultado)

###############################################################################
# 5. MODELO 2: LPM + EFECTOS FIJOS DE PAÍS
###############################################################################

cat("\n\t MODELO 2: LPM + efectos fijos de país \n")

formula_m2 <- empresa_restringida_financieramente ~
  tamano_pequena + tamano_mediana + tamano_grande +
  antiguedad_2_4 + antiguedad_5_9 + antiguedad_10mas +
  factor_pais

m2 <- estimar_lpm(formula_m2, datos_regresion, pesos = datos_regresion$wgtcommon)
print(m2$resultado)

###############################################################################
# 6. MODELO 3: LPM + EFECTOS FIJOS DE PAÍS + SUBPERIODO MONETARIO
###############################################################################

cat("\n\t MODELO 3: LPM + país + subperiodo monetario \n")

# periodo_monetario captura el efecto medio del ciclo sobre la probabilidad
# de restricción, controlando por características de la empresa y país
# Categoría de referencia: Política laxa (2018-2021)
formula_m3 <- empresa_restringida_financieramente ~
  tamano_pequena + tamano_mediana + tamano_grande +
  antiguedad_2_4 + antiguedad_5_9 + antiguedad_10mas +
  factor_pais +
  periodo_monetario

m3 <- estimar_lpm(formula_m3, datos_regresion, pesos = datos_regresion$wgtcommon)
print(m3$resultado)

###############################################################################
# 7. MODELO 4: LPM + INTERACCIÓN es_pyme × periodo_monetario
###############################################################################

cat("\n\t MODELO 4: LPM + interacción PYME × subperiodo (modelo central) \n")

# La interacción es_pyme:periodo_monetario permite que el efecto del ciclo
# monetario sobre la restricción financiera sea diferente para PYMEs y grandes.
# El coeficiente de periodo_monetario captura el efecto para grandes empresas.
# Los coeficientes de la interacción capturan el diferencial PYME respecto a grande.

formula_m4 <- empresa_restringida_financieramente ~
  tamano_pequena + tamano_mediana + # Grande = referencia dentro de PYMEs
  antiguedad_2_4 + antiguedad_5_9 + antiguedad_10mas +
  factor_pais +
  periodo_monetario +
  es_pyme +
  es_pyme:periodo_monetario

m4 <- estimar_lpm(formula_m4, datos_regresion, pesos = datos_regresion$wgtcommon)
print(m4$resultado)

# Nota de interpretación:
# - periodo_monetario "Endurecimiento": efecto del endurecimiento para grandes empresas
# - es_pyme:periodo_monetario "Endurecimiento": diferencial adicional para PYMEs
# - Efecto total para PYMEs en endurecimiento = coef(periodo_monetario) + coef(interacción)

###############################################################################
# 8. MODELO 5: ESTIMACIÓN SEPARADA POR TIPO (robustez del modelo 4)
###############################################################################

cat("\n\t MODELO 5a: LPM solo PYMEs \n")

datos_pymes  <- datos_regresion %>% filter(es_pyme == 1)
datos_grandes <- datos_regresion %>% filter(es_pyme == 0)

cat(sprintf("PYMEs: %d obs.\n", nrow(datos_pymes)))
cat(sprintf("Grandes: %d obs.\n", nrow(datos_grandes)))

formula_m5 <- empresa_restringida_financieramente ~
  tamano_pequena + tamano_mediana +
  antiguedad_2_4 + antiguedad_5_9 + antiguedad_10mas +
  factor_pais +
  periodo_monetario

m5_pymes  <- estimar_lpm(formula_m5, datos_pymes,   pesos = datos_pymes$wgtcommon)
m5_grandes <- estimar_lpm(formula_m5, datos_grandes, pesos = datos_grandes$wgtcommon)

print(m5_pymes$resultado)

cat("\n\t MODELO 5b: LPM solo grandes empresas\n")
print(m5_grandes$resultado)

###############################################################################
# 9. TABLA COMPARATIVA DE COEFICIENTES CLAVE
###############################################################################

# Extraer los coeficientes de periodo_monetario de los modelos 3, 4 y 5
# para facilitar la comparación
extraer_monetary <- function(resultado, nombre_modelo) {
  mat <- unclass(resultado)  # Elimina la clase coeftest, deja una matriz plana
  
  tibble(
    modelo   = nombre_modelo,
    variable = rownames(mat),
    coef     = mat[, "Estimate"],
    se       = mat[, "Std. Error"],
    pval     = mat[, "Pr(>|t|)"]
  ) %>%
    filter(grepl("periodo_monetario|es_pyme", variable))
}

tabla_comparativa <- bind_rows(
  extraer_monetary(m3$resultado, "M3: Base + periodo"),
  extraer_monetary(m4$resultado, "M4: Interacción PYME × periodo"),
  extraer_monetary(m5_pymes$resultado, "M5a: Solo PYMEs"),
  extraer_monetary(m5_grandes$resultado, "M5b: Solo grandes")
)

cat("\n\t TABLA COMPARATIVA: coeficientes clave \n")
print(tabla_comparativa, digits = 4)

# Guardar tabla
write.csv(tabla_comparativa,
          file.path(directorio_tablas, "tabla_coefs_monetary.csv"),
          row.names = FALSE)

cat("\n OK: Tabla guardada en resultados/tablas/tabla_coefs_monetary.csv\n")

###############################################################################
# 10. MODELO 6: tasa_dfr CONTINUA × es_pyme
# Cuantifica directamente la sensibilidad de la restricción financiera
# a la política monetaria, diferenciando PYME vs grande
###############################################################################

cat("\n\t MODELO 6: DFR continua × PYME (mecanismo de transmisión) \n")

# Comprobar que tasa_dfr no tiene NAs en la muestra de regresión
cat(sprintf("NAs en tasa_dfr: %d\n", sum(is.na(datos_regresion$tasa_dfr))))

formula_m6 <- empresa_restringida_financieramente ~
  tamano_pequena + tamano_mediana +
  antiguedad_2_4 + antiguedad_5_9 + antiguedad_10mas +
  factor_pais +
  tasa_dfr +
  es_pyme +
  tasa_dfr:es_pyme

m6 <- estimar_lpm(formula_m6, datos_regresion, pesos = datos_regresion$wgtcommon)
print(m6$resultado)

# Interpretación de los coeficientes clave:
# tasa_dfr: efecto de +1pp en DFR sobre grandes empresas
# es_pyme: diferencial estructural PYME vs grande cuando DFR = 0
# tasa_dfr:es_pyme: diferencial adicional del efecto DFR para PYMEs

# Extraer y mostrar solo los coeficientes relevantes
mat_m6 <- unclass(m6$resultado)
coefs_m6 <- tibble(
  variable = rownames(mat_m6),
  coef     = mat_m6[, "Estimate"],
  se       = mat_m6[, "Std. Error"],
  pval     = mat_m6[, "Pr(>|t|)"]
) %>%
  filter(grepl("tasa_dfr|es_pyme", variable))

cat("\n\t Coeficientes clave Modelo 6 \n")
print(coefs_m6, digits = 4)

# Guardar
write.csv(coefs_m6,
          file.path(directorio_tablas, "tabla_coefs_m6_dfr.csv"),
          row.names = FALSE)
cat("\n OK: Tabla M6 guardada en resultados/tablas/tabla_coefs_m6_dfr.csv\n")

###############################################################################
# 11. EXTRACCIÓN DE TABLAS PARA EL TFG
###############################################################################

extraer_coefs <- function(resultado, vars) {
  mat <- unclass(resultado)
  tibble(
    variable = rownames(mat),
    coef     = mat[, "Estimate"],
    se       = mat[, "Std. Error"],
    pval     = mat[, "Pr(>|t|)"]
  ) %>% filter(variable %in% vars)
}

# Variables para Tabla 1 (M1 y M2)
vars_t1 <- c("tamano_pequena", "tamano_mediana", "tamano_grande",
             "antiguedad_2_4", "antiguedad_5_9", "antiguedad_10mas",
             "(Intercept)")

# Variables para Tabla 2 (M3, M4, M5a, M5b)
vars_t2 <- c("periodo_monetarioEndurecimiento (2022-2023)",
             "periodo_monetarioRelajación (2024-2025)",
             "es_pyme",
             "es_pyme:periodo_monetarioEndurecimiento (2022-2023)",
             "es_pyme:periodo_monetarioRelajación (2024-2025)")

cat("\n========== TABLA 1: M1 ==========\n")
print(extraer_coefs(m1$resultado, vars_t1), digits = 4)
cat(sprintf("N = %d | R2 = %.4f\n",
            nobs(m1$modelo), summary(m1$modelo)$r.squared))

cat("\n========== TABLA 1: M2 ==========\n")
print(extraer_coefs(m2$resultado, vars_t1), digits = 4)
cat(sprintf("N = %d | R2 = %.4f\n",
            nobs(m2$modelo), summary(m2$modelo)$r.squared))

cat("\n========== TABLA 2: M3 ==========\n")
print(extraer_coefs(m3$resultado, vars_t2), digits = 4)
cat(sprintf("N = %d | R2 = %.4f\n",
            nobs(m3$modelo), summary(m3$modelo)$r.squared))

cat("\n========== TABLA 2: M4 ==========\n")
print(extraer_coefs(m4$resultado, c(vars_t2, "tamano_pequena", "tamano_mediana")), digits = 4)
cat(sprintf("N = %d | R2 = %.4f\n",
            nobs(m4$modelo), summary(m4$modelo)$r.squared))

cat("\n========== TABLA 2: M5a (PYMEs) ==========\n")
print(extraer_coefs(m5_pymes$resultado, vars_t2), digits = 4)
cat(sprintf("N = %d | R2 = %.4f\n",
            nobs(m5_pymes$modelo), summary(m5_pymes$modelo)$r.squared))

cat("\n========== TABLA 2: M5b (Grandes) ==========\n")
print(extraer_coefs(m5_grandes$resultado, vars_t2), digits = 4)
cat(sprintf("N = %d | R2 = %.4f\n",
            nobs(m5_grandes$modelo), summary(m5_grandes$modelo)$r.squared))

cat("\n========== M6: coeficientes clave ==========\n")
print(coefs_m6, digits = 4)

# Estadísticas descriptivas para Anexo C
cat("\n========== ANEXO C: DESCRIPTIVAS ==========\n")

# 1. Por subperiodo
tab_periodo <- datos_regresion %>%
  group_by(periodo_monetario) %>%
  summarise(
    n = n(),
    tasa_restriccion = weighted.mean(empresa_restringida_financieramente,
                                     wgtcommon, na.rm = TRUE),
    dfr_media = weighted.mean(tasa_dfr, wgtcommon, na.rm = TRUE),
    pct_pyme = weighted.mean(es_pyme, wgtcommon, na.rm = TRUE)
  )
print(tab_periodo, digits = 4)

# 2. Por tamaño
tab_tamano <- datos_regresion %>%
  mutate(tamano = case_when(
    d1_rec == 1 ~ "1 Micro",
    d1_rec == 2 ~ "2 Pequeña",
    d1_rec == 3 ~ "3 Mediana",
    d1_rec == 4 ~ "4 Grande"
  )) %>%
  group_by(tamano) %>%
  summarise(
    n = n(),
    tasa_restriccion = weighted.mean(empresa_restringida_financieramente,
                                     wgtcommon, na.rm = TRUE)
  )
print(tab_tamano, digits = 4)

# 3. Global
cat(sprintf("\nTasa restricción global (pond.): %.4f\n",
            weighted.mean(datos_regresion$empresa_restringida_financieramente,
                          datos_regresion$wgtcommon, na.rm = TRUE)))
cat(sprintf("N PYMEs: %d | N Grandes: %d\n",
            sum(datos_regresion$es_pyme == 1), sum(datos_regresion$es_pyme == 0)))
cat(sprintf("R2 Modelo 6: %.4f\n", summary(m6$modelo)$r.squared))
