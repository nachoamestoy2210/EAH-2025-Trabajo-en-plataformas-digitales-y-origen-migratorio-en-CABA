# ==============================================================================
# 02 - DESCRIPTIVO (Parte A): extremos, plataformas por origen, edad, llegada,
#      horas e ingresos, educación, clusters (k-means) y tablas de nulos
# Lee:    input/eah2025_limpia.rds
# Guarda: output/tablas y output/graficos
# ==============================================================================

source("script/00_config.R")
cargar_datos()

# ==============================================================================
# PARTE A - DESCRIPTIVO
# ==============================================================================

## A.1 Valores extremos: se marcan (no se modifican) ----
# Gráfico: rango intercuartílico y ubicación de los extremos
limites <- bind_rows(
  mutate(as_tibble_row(lim_horas),    variable = "Horas semanales"),
  mutate(as_tibble_row(lim_ingresos), variable = "Ingreso laboral ($)")
)

base_iqr <- universo |>
  mutate(trabajo = if_else(plataforma, "Trabajan en plataformas", "No trabajan en plataformas"))

puntos_iqr <- bind_rows(
  transmute(base_iqr, origen, trabajo, variable = "Horas semanales",
            valor = horas, extremo = extremo_horas),
  transmute(base_iqr, origen, trabajo, variable = "Ingreso laboral ($)",
            valor = ingresos, extremo = extremo_ingresos)
) |>
  filter(!is.na(valor))

g_extremos <- ggplot(puntos_iqr, aes(origen, valor)) +
  geom_rect(data = limites, aes(xmin = -Inf, xmax = Inf, ymin = q1, ymax = q3),
            inherit.aes = FALSE, fill = "steelblue", alpha = 0.2) +
  geom_jitter(aes(color = extremo), width = 0.25, size = 0.7, alpha = 0.5) +
  geom_hline(data = limites, aes(yintercept = inf), linetype = "dashed") +
  geom_hline(data = limites, aes(yintercept = sup), linetype = "dashed") +
  facet_grid(variable ~ trabajo, scales = "free_y") +
  scale_color_manual(values = c(`FALSE` = "grey60", `TRUE` = "red"),
                     labels = c(`FALSE` = "Dentro de los límites", `TRUE` = "Extremo")) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Valores extremos: rango intercuartílico y ubicación de los casos",
       subtitle = "Banda azul: Q1-Q3. Líneas punteadas: Q1 - 1,5 IQR y Q3 + 1,5 IQR (cuartiles ponderados de los ocupados).",
       x = NULL, y = NULL, color = NULL, caption = FUENTE_DESC) +
  theme(legend.position = "bottom")
print(g_extremos)


## A.2 % de ocupados que trabaja en plataformas, según origen ----
pct_plataforma <- universo |>
  group_by(origen) |>
  summarise(casos            = n(),
            casos_plataforma = sum(plataforma),
            pct_plataforma   = round(sum(fexp[plataforma]) / sum(fexp) * 100, 2))

pct_total <- sum(universo$fexp[universo$plataforma]) / sum(universo$fexp) * 100

mostrar("A.2 % de ocupados en plataformas según origen", pct_plataforma)

g_pct_plataforma <- ggplot(pct_plataforma, aes(origen, pct_plataforma, fill = origen)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = pct_total, linetype = "dashed") +
  geom_text(aes(label = paste0(pct_plataforma, "%\n(", casos_plataforma, " casos)")),
            vjust = -0.3) +
  scale_fill_manual(values = COLORES) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"),
                     expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Trabajo en plataformas según lugar de nacimiento",
       subtitle = paste0("Línea punteada: promedio de todos los ocupados (",
                         round(pct_total, 1), "%)"),
       x = NULL, y = "% de ocupados", caption = FUENTE_DESC)
print(g_pct_plataforma)


## A.3 Edad de los trabajadores de plataformas ----
edad_plataformas <- plataformas |>
  group_by(origen) |>
  summarise(casos   = n(),
            media   = round(weighted.mean(edad, fexp), 1),
            mediana = mediana_pond(edad, fexp),
            desvio  = round(desvio_pond(edad, fexp), 1))

mostrar("A.3 Edad de los trabajadores de plataformas", edad_plataformas)

# Densidad ponderada: cada grupo suma 100%, así se comparan aunque tengan distinto tamaño
g_edad <- plataformas |>
  group_by(origen) |>
  mutate(peso = fexp / sum(fexp)) |>
  ggplot(aes(edad, weight = peso, fill = origen, color = origen)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = edad_plataformas, aes(xintercept = mediana, color = origen),
             linetype = "dashed") +
  scale_fill_manual(values = COLORES) +
  scale_color_manual(values = COLORES) +
  labs(title = "Edad de los trabajadores de plataformas",
       subtitle = "Líneas punteadas: mediana de cada grupo",
       x = "Edad", y = "Densidad", fill = NULL, color = NULL, caption = FUENTE_DESC) +
  theme(legend.position = "bottom")
print(g_edad)


## A.4 Año de llegada y años de residencia (inmigrantes ocupados) ----
llegada <- universo |>
  filter(origen == "Inmigrantes", !is.na(anio_llegada)) |>
  group_by(plataforma) |>
  summarise(casos                = n(),
            anio_llegada_medio   = round(weighted.mean(anio_llegada, fexp), 1),
            anio_llegada_mediano = mediana_pond(anio_llegada, fexp),
            residencia_media     = round(weighted.mean(anios_residencia, fexp), 1),
            residencia_mediana   = mediana_pond(anios_residencia, fexp))

mostrar("A.4 Año de llegada y años de residencia (inmigrantes ocupados)", llegada)


## A.5 Horas trabajadas e ingresos ----

horas_ingresos <- resumen_laboral(plataformas)
mostrar("A.5 Horas e ingresos de los trabajadores de plataformas", horas_ingresos)

# Diagramas de caja ponderados (caja = p25-p75, bigotes = p10-p90, rombo = media)
caja <- plataformas |>
  select(origen, fexp, `Horas semanales` = horas, `Ingreso laboral ($)` = ingresos) |>
  pivot_longer(-c(origen, fexp), names_to = "variable", values_to = "valor") |>
  group_by(variable, origen) |>
  summarise(p10   = cuantil_ponderado(valor, fexp, 0.10),
            p25   = cuantil_ponderado(valor, fexp, 0.25),
            p50   = cuantil_ponderado(valor, fexp, 0.50),
            p75   = cuantil_ponderado(valor, fexp, 0.75),
            p90   = cuantil_ponderado(valor, fexp, 0.90),
            media = weighted.mean(valor, fexp, na.rm = TRUE),
            .groups = "drop")

g_cajas <- ggplot(caja, aes(origen, fill = origen,
                            ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90)) +
  geom_boxplot(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_point(aes(y = media), shape = 23, size = 3, fill = "white") +
  facet_wrap(~ variable, scales = "free_y") +
  scale_fill_manual(values = COLORES) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Horas e ingresos de los trabajadores de plataformas",
       subtitle = "Caja: percentiles 25-75 y mediana. Bigotes: percentiles 10-90. Rombo: media.",
       x = NULL, y = NULL, caption = FUENTE_DESC)
print(g_cajas)


## A.6 Nivel educativo ----
# complete(): si un nivel no tiene casos en algún origen, aparece con 0 (barras alineadas)
tabla_educacion <- plataformas |>
  filter(!is.na(nivel_educativo)) |>
  group_by(origen, nivel_educativo) |>
  summarise(casos = n(), personas = sum(fexp), .groups = "drop") |>
  complete(origen, nivel_educativo, fill = list(casos = 0L, personas = 0)) |>
  group_by(origen) |>
  mutate(pct = round(personas / sum(personas) * 100, 2)) |>
  ungroup()

# Mismo universo que la tabla anterior (nivel_educativo no NA)
pct_superior <- plataformas |>
  filter(!is.na(nivel_educativo)) |>
  group_by(origen) |>
  summarise(casos                 = n(),
            pct_superior_alguno   = round(sum(fexp[educacion >= 6]) / sum(fexp) * 100, 2),
            pct_superior_completo = round(sum(fexp[educacion == 7]) / sum(fexp) * 100, 2))

mostrar("A.6 Nivel educativo de los trabajadores de plataformas", tabla_educacion)
mostrar("A.6 % con estudios superiores", pct_superior)

g_educ <- ggplot(tabla_educacion, aes(nivel_educativo, pct, fill = origen)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), position = position_dodge(width = 0.8),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = COLORES) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Nivel educativo de los trabajadores de plataformas",
       x = NULL, y = "% dentro de cada origen", fill = NULL, caption = FUENTE_DESC) +
  theme(legend.position = "bottom")
print(g_educ)


## A.7 Clusterización de trabajadores de plataformas (k-means) ----
# Adultos de plataformas con datos completos en las cuatro variables
VARS_CLUSTER <- c("edad", "horas", "log_ingresos", "anios_escolaridad")

trabajadores_cluster <- plataformas |>
  filter(edad >= 18) |>
  drop_na(all_of(VARS_CLUSTER))

cat("\nA.7 Observaciones disponibles para el k-means:", nrow(trabajadores_cluster), "\n")
stopifnot(nrow(trabajadores_cluster) > K_CLUSTERS)

# Variables estandarizadas; el k-means no usa ponderadores
mat_cluster <- scale(select(trabajadores_cluster, all_of(VARS_CLUSTER)))

# Criterio del codo: suma de cuadrados intra-cluster para distintos k
# (sirve para justificar la elección de K_CLUSTERS)
K_MAX <- min(8, nrow(mat_cluster) - 1)
codo <- tibble(k = 1:K_MAX,
               suma_cuadrados_intra = map_dbl(1:K_MAX, \(k) {
                 set.seed(SEMILLA)
                 kmeans(mat_cluster, centers = k, nstart = 25)$tot.withinss
               }))
mostrar("Criterio del codo (suma de cuadrados intra-cluster por k)", codo)

g_codo <- ggplot(codo, aes(k, suma_cuadrados_intra)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = K_CLUSTERS, linetype = "dashed") +
  scale_x_continuous(breaks = codo$k) +
  labs(title = "Criterio del codo para el k-means",
       subtitle = paste0("Línea punteada: k elegido (", K_CLUSTERS, ")"),
       x = "Cantidad de clusters (k)", y = "Suma de cuadrados intra-cluster", caption = FUENTE_DESC)
print(g_codo)

# Modelo final
set.seed(SEMILLA)
modelo_cluster <- kmeans(mat_cluster, centers = K_CLUSTERS, nstart = 25)
trabajadores_cluster$cluster <- factor(modelo_cluster$cluster)

# Resultados del modelo por consola
cat("\nTamaño de cada cluster (casos muestrales):\n")
print(table(cluster = trabajadores_cluster$cluster))
cat("\nCentros de los clusters (variables estandarizadas):\n")
print(round(modelo_cluster$centers, 2))
cat("\nVarianza explicada por el modelo (%):",
    round(100 * modelo_cluster$betweenss / modelo_cluster$totss, 1), "\n")

# Perfil de cada cluster (medias ponderadas)
perfiles <- trabajadores_cluster |>
  group_by(cluster) |>
  summarise(observaciones          = n(),
            poblacion_estimada     = sum(fexp),
            edad_media             = round(weighted.mean(edad, fexp), 1),
            horas_medias           = round(weighted.mean(horas, fexp), 1),
            ingreso_medio          = round(weighted.mean(ingresos, fexp)),
            escolaridad_media      = round(weighted.mean(anios_escolaridad, fexp), 1),
            porcentaje_inmigrantes = round(
              sum(fexp[origen == "Inmigrantes"]) / sum(fexp) * 100, 2))
mostrar("Perfil de cada cluster (medias ponderadas)", perfiles)

# Composición de cada cluster por origen (¿qué % de cada cluster es inmigrante?)
composicion <- trabajadores_cluster |>
  group_by(cluster, origen) |>
  summarise(poblacion_estimada = sum(fexp), .groups = "drop_last") |>
  mutate(porcentaje = round(poblacion_estimada / sum(poblacion_estimada) * 100, 2)) |>
  ungroup()
mostrar("Composición de cada cluster por origen", composicion)

# Distribución de cada origen entre los clusters (¿en qué cluster está la mayoría de los inmigrantes?)
distribucion_origen <- trabajadores_cluster |>
  group_by(origen, cluster) |>
  summarise(casos = n(), personas = sum(fexp), .groups = "drop_last") |>
  mutate(pct_del_origen = round(personas / sum(personas) * 100, 2)) |>
  ungroup()
mostrar("Distribución de cada origen entre los clusters", distribucion_origen)

g_composicion <- ggplot(composicion, aes(cluster, porcentaje, fill = origen)) +
  geom_col() +
  scale_fill_manual(values = COLORES) +
  labs(title = "Composición de los clusters por origen",
       x = "Cluster", y = "% ponderado",
       fill = "Lugar de nacimiento", caption = FUENTE_DESC)
print(g_composicion)


## A.8 Valores nulos y extremos: tablas por consola ----
# Universo: ocupados. Nulo = código de no respuesta o error ya convertido en NA.
VARS_NULOS <- c("origen", "plataforma", "nivel_educativo", "anios_escolaridad",
                "horas", "ingresos")

# a) Cantidad de nulos por variable (el año de llegada sólo corresponde a inmigrantes)
inmigrantes_ocup <- filter(ocupados, origen == "Inmigrantes")

tabla_nulos <- bind_rows(
  ocupados |>
    summarise(across(all_of(VARS_NULOS), ~ sum(is.na(.x)))) |>
    pivot_longer(everything(), names_to = "variable", values_to = "nulos") |>
    mutate(total = nrow(ocupados)),
  tibble(variable = "anio_llegada (solo inmigrantes)",
         nulos = sum(is.na(inmigrantes_ocup$anio_llegada)), total = nrow(inmigrantes_ocup))
) |>
  mutate(pct = round(nulos / total * 100, 2)) |>
  arrange(desc(nulos))

# b) Cuántas personas (filas) tienen 0, 1, 2... nulos entre las variables anteriores
nulos_por_fila <- tibble(n_nulos = rowSums(is.na(ocupados[VARS_NULOS]))) |>
  count(n_nulos, name = "personas") |>
  mutate(pct = round(personas / sum(personas) * 100, 2))

# c) % de nulos dentro de cada origen (NA = origen sin dato)
nulos_por_origen <- ocupados |>
  group_by(origen) |>
  summarise(casos = n(),
            across(all_of(setdiff(VARS_NULOS, "origen")), ~ round(mean(is.na(.x)) * 100, 2)))

# d) Valores extremos (criterio IQR) por grupo
extremos <- ocupados |>
  group_by(origen, plataforma) |>
  summarise(casos             = n(),
            extremos_horas    = sum(extremo_horas, na.rm = TRUE),
            extremos_ingresos = sum(extremo_ingresos, na.rm = TRUE),
            .groups = "drop")

# e) Consecuencia de mantener los extremos: resultados con y sin ellos
sensibilidad <- bind_rows(
  "Con extremos" = resumen_laboral(plataformas),
  "Sin extremos" = resumen_laboral(filter(plataformas,
                                          !extremo_horas %in% TRUE,
                                          !extremo_ingresos %in% TRUE)),
  .id = "escenario"
)

mostrar("1) Nulos por variable (ocupados)", tabla_nulos)
mostrar("2) Personas según cantidad de nulos", nulos_por_fila)
mostrar("3) % de nulos por origen", nulos_por_origen)
mostrar("4) Valores extremos por grupo", extremos)
mostrar("5) Plataformas: horas e ingresos con y sin extremos", sensibilidad)

# Exportación a output/tablas y output/graficos -------------------------------
if (EXPORTAR) {
  guardar_tabla(pct_plataforma,       "A2_pct_plataformas_por_origen")
  guardar_tabla(edad_plataformas,     "A3_edad_plataformas")
  guardar_tabla(llegada,              "A4_llegada_residencia_inmigrantes")
  guardar_tabla(horas_ingresos,       "A5_horas_ingresos_plataformas")
  guardar_tabla(tabla_educacion,      "A6_educacion_plataformas")
  guardar_tabla(pct_superior,         "A6_estudios_superiores")
  guardar_tabla(codo,                 "A7_codo_kmeans")
  guardar_tabla(perfiles,             "A7_perfiles_clusters")
  guardar_tabla(composicion,          "A7_composicion_clusters")
  guardar_tabla(distribucion_origen,  "A7_distribucion_origen_clusters")
  guardar_tabla(tabla_nulos,          "A8_nulos_por_variable")
  guardar_tabla(nulos_por_fila,       "A8_nulos_por_persona")
  guardar_tabla(nulos_por_origen,     "A8_nulos_por_origen")
  guardar_tabla(extremos,             "A8_extremos_por_grupo")
  guardar_tabla(sensibilidad,         "A8_sensibilidad_extremos")
  
  guardar_grafico(g_extremos,       "gA1_valores_extremos",        11, 7)
  guardar_grafico(g_pct_plataforma, "gA2_pct_plataformas_origen",   7, 5)
  guardar_grafico(g_edad,           "gA3_edad_plataformas",         8, 5)
  guardar_grafico(g_cajas,          "gA5_horas_ingresos",           9, 5)
  guardar_grafico(g_educ,           "gA6_educacion",                9, 6)
  guardar_grafico(g_codo,           "gA7_codo_kmeans",              7, 5)
  guardar_grafico(g_composicion,    "gA7_composicion_clusters",     7, 5)
  
}
