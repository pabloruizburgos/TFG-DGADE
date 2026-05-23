###############################################################################
# 3_analisis_descriptivo.R
# Análisis descriptivo para las preguntas de investigación 1 y 3
# Autor: Pablo Ruiz Burgos
#
# Este script genera:
# - Estadísticas descriptivas de la muestra
# - Gráficos de evolución de restricciones financieras (Pregunta 1)
# - Gráficos de fuentes alternativas de financiación (Pregunta 3)
#
# IMPORTANTE: Ejecutar 0_setup.R y 2_limpieza_datos.R antes de este script.
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

# Paleta y tema

AMB_1 <- "#78350F"
AMB_2 <- "#B45309"
AMB_3 <- "#D97706"
AMB_4 <- "#F59E0B"
AMB_GRIS <- "#A8A29E"

tema_tfg <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#F5F5F4", linewidth = 0.4),
    axis.text        = element_text(color = "#57534E"),
    axis.title       = element_text(color = "#57534E", size = 10),
    legend.text      = element_text(size = 9, color = "#57534E"),
    legend.title     = element_text(size = 9, color = "#44403C"),
    strip.text       = element_text(color = "#44403C", face = "bold"),
    plot.caption     = element_text(size = 8, color = "#78716C", hjust = 0),
    legend.position  = "bottom"
  )

###############################################################################
# 0. CARGA DE DATOS PROCESADOS
###############################################################################

directorio_datos <- paste0(getwd(), "/datos")
directorio_graficos <- paste0(getwd(), "/resultados/graficos")
archivo_procesado <- file.path(directorio_datos, "datos_procesados.rds")
datos <- readRDS(archivo_procesado)

###############################################################################
# 1. EVOLUCIÓN DE TASAS DE POLÍTICA MONETARIA DEL BCE (fig0)
###############################################################################

# Cargar datos de tasas BCE
tasas_bce <- read.csv(file.path(directorio_datos, "tasas_bce.csv"))
tasas_bce$fecha <- as.Date(tasas_bce$fecha)

grafico_tasas_bce <- ggplot(tasas_bce, aes(x = fecha, y = tasa, color = tipo_tasa)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = AMB_GRIS) +
  labs(
    x = "",
    y = "Tasa de interés (%)",
    color = "Tipo de tasa",
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c(AMB_1, AMB_2, AMB_4)) +
  tema_tfg

print(grafico_tasas_bce)
ggsave(file.path(directorio_graficos, "fig0_tasas_bce.png"),
       grafico_tasas_bce, width = 9, height = 5, dpi = 300)

###############################################################################
# 2. ESTADÍSTICAS DESCRIPTIVAS DE LA MUESTRA
###############################################################################

# 2.1 Distribución por tamaño de empresa
tabla_tamano <- datos %>%
  group_by(tamano_empresa) %>%
  summarise(
    n = n(),
    n_ponderado = sum(wgtcommon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct_ponderado = n_ponderado / sum(n_ponderado) * 100
  )
print(tabla_tamano)

# 2.2 Distribución por país (top 10)
tabla_pais <- datos %>%
  group_by(pais) %>%
  summarise(
    n = n(),
    n_ponderado = sum(wgtcommon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  head(10)
print(tabla_pais)

###############################################################################
# 3. PREGUNTA 1: EVOLUCIÓN DE LAS RESTRICCIONES CREDITICIAS
###############################################################################

# 3.1 Gráfico: Tasa de restricción financiera por wave (fig1)
# Calcular tasa ponderada por wave
evolucion_restriccion <- datos %>%
  filter(bancos_relevantes == 1) %>%  # Solo empresas para las que banca es relevante
  filter(!is.na(empresa_restringida_financieramente)) %>%
  group_by(año) %>%
  summarise(
    n = n(),
    tasa_restriccion = weighted.mean(empresa_restringida_financieramente, wgtcommon, na.rm = TRUE) * 100,
    .groups = "drop"
  )

grafico_restriccion <- ggplot(evolucion_restriccion, aes(x = año, y = tasa_restriccion)) +
  geom_line(linewidth = 1.2, group = 1, color = AMB_2) +
  geom_point(size = 3, color = AMB_2) +
  labs(
    x = "",
    y = "Tasa de restricción (%)",
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  tema_tfg

print(grafico_restriccion)
ggsave(file.path(directorio_graficos, "fig1_restriccion_agregada.png"),
       grafico_restriccion, width = 8, height = 5, dpi = 300)

# 3.2 Gráfico: Composición de obstáculos por wave (fig2)
composicion_obstaculos <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(obstaculos_obtencion_prestamo_bancario)) %>%
  filter(obstaculos_obtencion_prestamo_bancario > 0) %>%  # Solo restringidas
  group_by(fecha, obstaculos_obtencion_prestamo_bancario) %>%
  summarise(w = sum(wgtcommon, na.rm = TRUE), .groups = "drop") %>%
  group_by(fecha) %>%
  mutate(pct = w / sum(w) * 100) %>%
  ungroup() %>%
  mutate(
    tipo_obstaculo = factor(
      obstaculos_obtencion_prestamo_bancario,
      levels = 1:4,
      labels = c("Costo excesivo", "Cantidad limitada", "Desalentada", "Rechazada")
    )
  )

grafico_composicion <- ggplot(composicion_obstaculos,
                              aes(x = fecha, y = pct, fill = tipo_obstaculo)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(
    x = "",
    y = "Porcentaje (%)",
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(
    "Costo excesivo"    = AMB_4,
    "Cantidad limitada" = AMB_3,
    "Desalentada"       = AMB_2,
    "Rechazada"         = AMB_1
  )) +
  tema_tfg +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_composicion)
ggsave(file.path(directorio_graficos, "fig2_composicion_obstaculos.png"),
       grafico_composicion, width = 9, height = 5, dpi = 300)

# 3.3 Gráfico: Evolución de necesidad vs disponibilidad de préstamos (fig3)
evolucion_brecha <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(cambio_necesidad_prestamos) & !is.na(cambio_disponibilidad_prestamos)) %>%
  group_by(año) %>%
  summarise(
    Necesidad = weighted.mean(cambio_necesidad_prestamos, wgtcommon, na.rm = TRUE),
    Disponibilidad = weighted.mean(cambio_disponibilidad_prestamos, wgtcommon, na.rm = TRUE),
    Brecha = weighted.mean(brecha_prestamos, wgtcommon, na.rm = TRUE),
    .groups = "drop"
  )

# Datos para líneas (Necesidad y Disponibilidad)
datos_lineas <- evolucion_brecha %>%
  pivot_longer(cols = c(Necesidad, Disponibilidad),
               names_to = "indicador", values_to = "valor")

grafico_necesidad_disp <- ggplot() +
  geom_bar(data = evolucion_brecha, aes(x = año, y = Brecha, fill = "Brecha"),
           stat = "identity", alpha = 0.5, width = 0.6) +
  geom_line(data = datos_lineas, aes(x = año, y = valor, color = indicador, group = indicador),
            linewidth = 1.2) +
  geom_point(data = datos_lineas, aes(x = año, y = valor, color = indicador),
             size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = AMB_GRIS) +
  scale_fill_manual(values = c("Brecha" = AMB_GRIS)) +
  scale_color_manual(values = c("Necesidad" = AMB_2, "Disponibilidad" = AMB_4)) +
  labs(
    x = "",
    y = "Índice de difusión",
    color = "",
    fill = "",
  ) +
  tema_tfg

print(grafico_necesidad_disp)
ggsave(file.path(directorio_graficos, "fig3_brecha_agregada.png"),
       grafico_necesidad_disp, width = 9, height = 5, dpi = 300)

# Modelo 1: Permite ver cómo evoluciona la brecha año a año respecto al año base
modelo_brecha <- lm(brecha_prestamos ~ factor(año),
                    data = datos, weights = wgtcommon); summary(modelo_brecha)

# Modelo 2: Mismo objetivo que el modelo 1, pero controlando por variables relevantes como el tamaño de la empresa y la antigüedad
modelo_brecha2 <- lm(brecha_prestamos ~ factor(año) + 
                       tamano_empresa + antiguedad,
                     data = datos, weights = wgtcommon); summary(modelo_brecha2)

# Modelo 3: Mismo objetivo que el modelo 1, pero controlando por variables relevantes como el tamaño de la empresa y la antigüedad, y efectos fijos de país
modelo_brecha3 <- lm(brecha_prestamos ~ factor(año) + 
                       tamano_empresa + antiguedad + pais,
                     data = datos, weights = wgtcommon); summary(modelo_brecha3)

# 3.4 Gráfico: Restricción por tamaño de empresa (fig4)
restriccion_tamano <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(empresa_restringida_financieramente) & !is.na(tamano_empresa)) %>%
  group_by(año, tamano_empresa) %>%
  summarise(
    tasa = weighted.mean(empresa_restringida_financieramente, wgtcommon, na.rm = TRUE) * 100,
    .groups = "drop"
  )

grafico_tamano <- ggplot(restriccion_tamano,
                         aes(x = año, y = tasa, color = tamano_empresa, group = tamano_empresa)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "",
    y = "Tasa de restricción (%)",
    color = "Tamaño",
  ) +
  scale_color_manual(values = c(
    "Micro (1-9)"      = AMB_1,
    "Pequeña (10-49)"  = AMB_2,
    "Mediana (50-249)" = AMB_3,
    "Grande (250+)"    = AMB_4
  )) +
  tema_tfg

print(grafico_tamano)
ggsave(file.path(directorio_graficos, "fig4_restriccion_tamano.png"),
       grafico_tamano, width = 8, height = 5, dpi = 300)

###############################################################################
# 4. PREGUNTA 3: FUENTES ALTERNATIVAS DE FINANCIACIÓN
###############################################################################

# 4.1 Gráfico: Relevancia de diferentes fuentes de financiación (fig5)
# Preparar datos de relevancia (códigos 1, 2, 3 = relevante)
relevancia_fuentes <- datos %>%
  summarise(
    `Ganancias retenidas` = weighted.mean(q4_a_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Líneas de crédito` = weighted.mean(q4_c_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Préstamos bancarios` = weighted.mean(q4_d_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Crédito comercial` = weighted.mean(q4_e_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Leasing` = weighted.mean(q4_m_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Subvenciones` = weighted.mean(q4_b_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Factoring` = weighted.mean(q4_r_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Emisión de deuda` = weighted.mean(q4_h_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100,
    `Emisión de capital` = weighted.mean(q4_j_rec %in% c(1, 2, 3), wgtcommon, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(everything(), names_to = "fuente", values_to = "relevancia") %>%
  arrange(desc(relevancia))

grafico_relevancia <- ggplot(relevancia_fuentes,
                             aes(x = reorder(fuente, relevancia), y = relevancia)) +
  geom_bar(stat = "identity", width = 0.7, fill = AMB_2) +
  coord_flip() +
  labs(
    x = "",
    y = "Porcentaje (%)",
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  tema_tfg

print(grafico_relevancia)
ggsave(file.path(directorio_graficos, "fig5_relevancia_fuentes.png"),
       grafico_relevancia, width = 8, height = 5, dpi = 300)

# 4.2 Gráfico: Uso de fuentes alternativas por empresas restringidas vs no restringidas (fig6)
# Comparar uso de fuentes entre empresas restringidas y no restringidas
uso_por_restriccion <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(empresa_restringida_financieramente)) %>%
  group_by(restringida = factor(empresa_restringida_financieramente,
                                levels = c(0, 1),
                                labels = c("No restringida", "Restringida"))) %>%
  summarise(
    `Ganancias retenidas` = weighted.mean(q4a_a == 1, wgtcommon, na.rm = TRUE) * 100,
    `Subvenciones` = weighted.mean(q4a_b == 1, wgtcommon, na.rm = TRUE) * 100,
    `Líneas de crédito` = weighted.mean(q4a_c == 1, wgtcommon, na.rm = TRUE) * 100,
    `Préstamos bancarios` = weighted.mean(q4a_d == 1, wgtcommon, na.rm = TRUE) * 100,
    `Crédito comercial` = weighted.mean(q4a_e == 1, wgtcommon, na.rm = TRUE) * 100,
    `Otros préstamos` = weighted.mean(q4a_f == 1, wgtcommon, na.rm = TRUE) * 100,
    `Emisión de deuda` = weighted.mean(q4a_h == 1, wgtcommon, na.rm = TRUE) * 100,
    `Emisión de capital` = weighted.mean(q4a_j == 1, wgtcommon, na.rm = TRUE) * 100,
    `Leasing` = weighted.mean(q4a_m == 1, wgtcommon, na.rm = TRUE) * 100,
    `Factoring` = weighted.mean(q4a_r == 1, wgtcommon, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-restringida, names_to = "fuente", values_to = "uso")

grafico_uso_restriccion <- ggplot(uso_por_restriccion,
                                  aes(x = fuente, y = uso, fill = restringida)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    x = "",
    y = "Porcentaje que usó la fuente (%)",
    fill = "",
  ) +
  scale_fill_manual(values = c("Restringida" = AMB_2, "No restringida" = AMB_4)) +
  tema_tfg +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico_uso_restriccion)
ggsave(file.path(directorio_graficos, "fig6_uso_por_restriccion.png"),
       grafico_uso_restriccion, width = 9, height = 5, dpi = 300)

###############################################################################
# 5. FIGURAS DE HETEROGENEIDAD PYME VS GRANDE
###############################################################################

# 5.1 fig1b: Tasa de restricción por wave, separada PYME vs Grande
evolucion_restriccion_sme <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(empresa_restringida_financieramente)) %>%
  filter(!is.na(es_pyme)) %>%
  group_by(año, tipo_empresa) %>%
  summarise(
    tasa_restriccion = weighted.mean(empresa_restringida_financieramente,
                                     wgtcommon, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  )

fig1b <- ggplot(evolucion_restriccion_sme,
                aes(x = año, y = tasa_restriccion,
                    color = tipo_empresa, group = tipo_empresa)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = AMB_GRIS, alpha = 0.7) +
  geom_vline(xintercept = 2024, linetype = "dotted", color = AMB_GRIS, alpha = 0.7) +
  annotate("text", x = 2022.1, y = Inf, label = "Endurecimiento",
           hjust = 0, vjust = 1.5, size = 3, color = AMB_GRIS) +
  annotate("text", x = 2024.1, y = Inf, label = "Relajación",
           hjust = 0, vjust = 1.5, size = 3, color = AMB_GRIS) +
  labs(
    x = "",
    y = "Tasa de restricción (%)",
    color = "",
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  scale_color_manual(values = c("PYME (<250)" = AMB_2, "Grande (250+)" = AMB_4)) +
  tema_tfg

print(fig1b)
ggsave(file.path(directorio_graficos, "fig1b_restriccion_sme.png"),
       fig1b, width = 8, height = 5, dpi = 300)

# 5.2 fig3b: Necesidad, disponibilidad y brecha por PYME vs Grande
evolucion_brecha_sme <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(cambio_necesidad_prestamos) &
           !is.na(cambio_disponibilidad_prestamos)) %>%
  filter(!is.na(es_pyme)) %>%
  group_by(año, tipo_empresa) %>%
  summarise(
    Necesidad     = weighted.mean(cambio_necesidad_prestamos,
                                  wgtcommon, na.rm = TRUE),
    Disponibilidad = weighted.mean(cambio_disponibilidad_prestamos,
                                   wgtcommon, na.rm = TRUE),
    Brecha        = weighted.mean(brecha_prestamos,
                                  wgtcommon, na.rm = TRUE),
    .groups = "drop"
  )

# Formato largo para líneas
datos_lineas_sme <- evolucion_brecha_sme %>%
  pivot_longer(cols = c(Necesidad, Disponibilidad),
               names_to = "indicador", values_to = "valor")

fig3b <- ggplot() +
  geom_bar(data = evolucion_brecha_sme,
           aes(x = año, y = Brecha, fill = "Brecha"),
           stat = "identity", alpha = 0.4, width = 0.6) +
  geom_line(data = datos_lineas_sme,
            aes(x = año, y = valor, color = indicador, group = indicador),
            linewidth = 1.1) +
  geom_point(data = datos_lineas_sme,
             aes(x = año, y = valor, color = indicador),
             size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = AMB_GRIS) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = AMB_GRIS, alpha = 0.7) +
  geom_vline(xintercept = 2024, linetype = "dotted", color = AMB_GRIS, alpha = 0.7) +
  facet_wrap(~ tipo_empresa, ncol = 1) +
  scale_fill_manual(values = c("Brecha" = AMB_GRIS)) +
  scale_color_manual(values = c("Necesidad" = AMB_2, "Disponibilidad" = AMB_4)) +
  labs(
    x = "",
    y = "Índice de difusión",
    color = "",
    fill  = "",
  ) +
  tema_tfg

print(fig3b)
ggsave(file.path(directorio_graficos, "fig3b_brecha_sme.png"),
       fig3b, width = 8, height = 7, dpi = 300)

# 5.3 fig6b: Uso de fuentes alternativas por restricción y subperiodo
# Comparación 2018-2021 (laxa) vs 2022-2025 (endurecimiento + relajación)
uso_por_restriccion_periodo <- datos %>%
  filter(bancos_relevantes == 1) %>%
  filter(!is.na(empresa_restringida_financieramente)) %>%
  filter(!is.na(periodo_monetario)) %>%
  # Agrupar en dos grandes periodos para que la comparación sea legible
  mutate(
    periodo_simple = case_when(
      año %in% 2018:2021 ~ "2018-2021\n(Política laxa)",
      año %in% 2022:2025 ~ "2022-2025\n(Endurecimiento y relajación)"
    ),
    restringida_label = factor(
      empresa_restringida_financieramente,
      levels = c(0, 1),
      labels = c("No restringida", "Restringida")
    )
  ) %>%
  group_by(periodo_simple, restringida_label) %>%
  summarise(
    `Ganancias retenidas` = weighted.mean(q4a_a == 1, wgtcommon, na.rm = TRUE) * 100,
    `Subvenciones`        = weighted.mean(q4a_b == 1, wgtcommon, na.rm = TRUE) * 100,
    `Crédito comercial`   = weighted.mean(q4a_e == 1, wgtcommon, na.rm = TRUE) * 100,
    `Otros préstamos`     = weighted.mean(q4a_f == 1, wgtcommon, na.rm = TRUE) * 100,
    `Leasing`             = weighted.mean(q4a_m == 1, wgtcommon, na.rm = TRUE) * 100,
    `Factoring`           = weighted.mean(q4a_r == 1, wgtcommon, na.rm = TRUE) * 100,
    `Emisión de capital`  = weighted.mean(q4a_j == 1, wgtcommon, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -c(periodo_simple, restringida_label),
    names_to  = "fuente",
    values_to = "uso"
  )

fig6b <- ggplot(uso_por_restriccion_periodo,
                aes(x = fuente, y = uso, fill = restringida_label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~ periodo_simple, ncol = 1) +
  labs(
    x = "",
    y = "Porcentaje que usó la fuente (%)",
    fill = "",
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Restringida" = AMB_2, "No restringida" = AMB_4)) +
  tema_tfg +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig6b)
ggsave(file.path(directorio_graficos, "fig6b_fuentes_periodo.png"),
       fig6b, width = 9, height = 8, dpi = 300)

cat("\n OK: Todas las figuras guardadas en resultados/graficos/ \n")
