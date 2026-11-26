# ==============================================================================
# [NUEVO] 07 - DESCRIPTIVO PREVIO (Parte F, agregado desde gráficos-previos.R):
#      estadísticas y gráficos para ver los datos antes de los tests (flujo del universo, numéricas, categóricas, tasas de
#      participación y calidad de datos). Todo ponderado por fexp salvo "casos".
# Lee:    input/eah2025_limpia.rds
# Guarda: output/tablas (F1 a F5) y output/graficos (gF1 a gF7)
# ==============================================================================

source("script/00_config.R")
cargar_datos()

VARS_NUM <- c(edad = "Edad (años)",
              horas = "Horas semanales",
              ingresos = "Ingreso laboral ($)",
              anios_escolaridad = "Años de escolaridad",
              anios_residencia = "Años de residencia (solo inmigrantes)")
VARS_CAT <- c("sexo_f", "grupo_edad", "educ_agrup", "horas_grupo")   # nombres en ETIQ_VARS


# F.1 Flujo del universo de análisis --------------------------------------------
flujo <- tibble(
  etapa     = c("Base individual", "Ocupados (fexp > 0)",
                "Con origen y plataforma informados", "Trabajan en plataformas"),
  casos     = c(nrow(datos), nrow(ocupados), nrow(universo), nrow(plataformas)),
  poblacion = c(sum(datos$fexp), sum(ocupados$fexp), sum(universo$fexp), sum(plataformas$fexp))
) |>
  mutate(pct_casos_previa = round(casos / lag(casos) * 100, 1))
mostrar("F.1 Flujo del universo de análisis", flujo)

casos_cruce <- universo |>
  count(origen_det, plataforma_f, name = "casos") |>
  filter(!is.na(origen_det)) |>
  pivot_wider(names_from = plataforma_f, values_from = casos, values_fill = 0)
mostrar("F.1 Casos muestrales: origen desagregado x plataforma", casos_cruce)


# F.2 Variables numéricas -------------------------------------------------------
num_grupo  <- resumen_numerico(universo, VARS_NUM, "plataforma_f")
num_origen <- resumen_numerico(plataformas, VARS_NUM, "origen")
mostrar("F.2 Variables numéricas: plataformas vs. otros ocupados", num_grupo)
mostrar("F.2 Variables numéricas: trabajadores de plataformas por origen", num_origen)


# F.3 Variables categóricas -----------------------------------------------------
dist_cat <- map_dfr(VARS_CAT, \(v) mutate(distribucion(universo, v),
                                          categoria = as.character(categoria))) |>
  mutate(categoria = fct_inorder(categoria))
mostrar("F.3 Distribución % dentro de cada grupo (ponderada y muestral)",
        mutate(dist_cat, across(c(pct_pond, pct_muestra), \(x) round(x, 1))))


# F.4 Tasa de participación por categoría ---------------------------------------
tasas_cat <- map_dfr(c("origen", "origen_det", "sexo_f", "grupo_edad", "educ_agrup"),
                     \(v) tasa_plataforma(universo, v)) |>
  mutate(pct_plataforma = round(pct_plataforma, 2))
mostrar("F.4 % de ocupados en plataformas por categoría", tasas_cat)


# F.5 Calidad de datos ----------------------------------------------------------
# a) Nulos: % por variable, en todos los ocupados y en plataformas
VARS_NULOS_F <- c("origen", "nivel_educativo", "anios_escolaridad", "horas", "ingresos")
nulos_f <- bind_rows("Todos los ocupados" = ocupados, "Plataformas" = plataformas, .id = "universo") |>
  group_by(universo) |>
  summarise(casos = n(), across(all_of(VARS_NULOS_F), \(x) mean(is.na(x)) * 100), .groups = "drop") |>
  pivot_longer(all_of(VARS_NULOS_F), names_to = "variable", values_to = "pct_nulos") |>
  mutate(pct_nulos = round(pct_nulos, 2))
mostrar("F.5 % de valores nulos", nulos_f)

# b) Valores extremos (criterio IQR de 01_limpieza.R) en plataformas, por origen
extremos_f <- plataformas |>
  group_by(origen) |>
  summarise(casos                = n(),
            pct_extremo_horas    = round(mean(extremo_horas, na.rm = TRUE) * 100, 1),
            pct_extremo_ingresos = round(mean(extremo_ingresos, na.rm = TRUE) * 100, 1),
            .groups = "drop")
mostrar("F.5 % de valores extremos (IQR) en plataformas", extremos_f)

# c) Factor de expansión: variabilidad de los ponderadores y efecto diseño de Kish
pesos_f <- bind_rows("Universo origen x plataforma" = universo,
                     "Plataformas"                  = plataformas,
                     .id = "grupo") |>
  group_by(grupo) |>
  summarise(casos              = n(),
            fexp_minimo        = min(fexp),
            fexp_mediana       = median(fexp),
            fexp_maximo        = max(fexp),
            cv_pesos           = sd(fexp) / mean(fexp),
            n_efectivo         = sum(fexp)^2 / sum(fexp^2),
            efecto_diseno_kish = n() / n_efectivo,
            .groups = "drop") |>
  mutate(across(c(cv_pesos, n_efectivo, efecto_diseno_kish), \(x) round(x, 2)))
mostrar("F.5 Factor de expansión (fexp) y efecto diseño de Kish", pesos_f)


# F.6 Gráficos ------------------------------------------------------------------
# Figura 1. Flujo
g_flujo <- flujo |>
  pivot_longer(c(casos, poblacion), names_to = "medida", values_to = "valor") |>
  mutate(medida = recode(medida, casos = "Casos muestrales", poblacion = "Población expandida (suma de fexp)"),
         etapa  = fct_rev(fct_inorder(etapa))) |>
  ggplot(aes(valor, etapa, fill = etapa == "Trabajan en plataformas")) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = format(round(valor), big.mark = ".", decimal.mark = ",")), hjust = -0.1, size = 3.2) +
  facet_wrap(~ medida, scales = "free_x") +
  scale_fill_manual(values = c(`TRUE` = "#d95f02", `FALSE` = "#bbbbbb")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Figura 1. Del total de la base al universo de trabajadores de plataformas",
       x = NULL, y = NULL, caption = "Fuente: EAH 2025 (DGEyC, GCBA).") +
  theme(plot.title.position = "plot")

# Figura 2. Densidades ponderadas (cada grupo suma 1); ingreso en logaritmo (log_ingresos, de 01_limpieza.R)
g_num <- densidad_pond(universo, c(edad = "Edad (años)",
                                   horas = "Horas semanales",
                                   log_ingresos = "Ingreso laboral (logaritmo)",
                                   anios_escolaridad = "Años de escolaridad")) +
  labs(title = "Figura 2. Distribución de las variables numéricas",
       subtitle = "Líneas punteadas: mediana ponderada de cada grupo. Cada grupo suma 100 %.",
       caption = FUENTE_DESC) +
  theme(legend.position = "top", plot.title.position = "plot")

# Figura 3. Variables categóricas
g_cat <- ggplot(dist_cat, aes(pct_pond, fct_rev(categoria), fill = grupo)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 0.8) +
  geom_text(aes(label = sprintf("%.0f %%  (n=%d)", pct_pond, casos)),
            position = position_dodge2(width = 0.8, reverse = TRUE), hjust = -0.08, size = 2.9) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_fill_manual(values = COLORES_PLAT) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(title = "Figura 3. Perfil de las variables categóricas",
       x = "% dentro de cada grupo", y = NULL, fill = NULL,
       caption = paste0(FUENTE_DESC, " Entre paréntesis, casos muestrales.")) +
  theme(legend.position = "top", plot.title.position = "plot",
        strip.text = element_text(face = "bold", hjust = 0))

# Figura 4. Participación en plataformas según origen
g_tasa_origen <- tasas_cat |>
  filter(variable == "Origen (3 categorías)") |>
  mutate(categoria = factor(categoria, levels = names(COLORES_DET))) |>
  ggplot(aes(categoria, pct_plataforma, fill = categoria)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f %%\n(%d de %d)", pct_plataforma, casos_plataforma, casos)),
            vjust = -0.2, size = 3) +
  scale_fill_manual(values = COLORES_DET) +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Figura 4. % de ocupados en plataformas según origen",
       x = NULL, y = "% de ocupados", caption = FUENTE_DESC) +
  theme(plot.title.position = "plot")

# Figura 5. Perfil de los trabajadores de plataformas según origen
cajas_origen <- plataformas |>
  select(origen, fexp, edad, horas, ingresos) |>
  pivot_longer(c(edad, horas, ingresos), names_to = "var", values_to = "valor") |>
  filter(!is.na(valor)) |>
  group_by(var, origen) |>
  summarise(n = n(),
            p10 = cuantil_ponderado(valor, fexp, 0.10), p25 = cuantil_ponderado(valor, fexp, 0.25),
            p50 = cuantil_ponderado(valor, fexp, 0.50), p75 = cuantil_ponderado(valor, fexp, 0.75),
            p90 = cuantil_ponderado(valor, fexp, 0.90), media = weighted.mean(valor, fexp),
            .groups = "drop") |>
  mutate(variable = unname(VARS_NUM[var]),
         etiqueta = paste0(origen, "\n(n = ", n, ")"))

g_cajas_origen <- ggplot(cajas_origen, aes(etiqueta, fill = origen,
                                           ymin = p10, lower = p25, middle = p50, upper = p75, ymax = p90)) +
  geom_boxplot(stat = "identity", width = 0.5, show.legend = FALSE) +
  geom_point(aes(y = media), shape = 23, size = 3, fill = "white") +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = COLORES) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  expand_limits(y = 0) +
  labs(title = "Figura 5. Perfil de los trabajadores de plataformas según origen",
       x = NULL, y = NULL,
       caption = "Fuente: EAH 2025 (DGEyC, GCBA). Caja: p25-p75 y mediana; bigotes: p10-p90; rombo: media (ponderados por fexp).") +
  theme(plot.title.position = "plot")

# Figura 6. Calidad de datos: nulos
g_nulos <- nulos_f |>
  mutate(variable = recode(variable, horas = "Horas", ingresos = "Ingreso laboral",
                           anios_escolaridad = "Años de escolaridad",
                           nivel_educativo = "Nivel educativo", origen = "Origen")) |>
  ggplot(aes(pct_nulos, fct_rev(fct_inorder(variable)), fill = universo)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 0.8) +
  geom_text(aes(label = sprintf("%.1f %%", pct_nulos)),
            position = position_dodge2(width = 0.8, reverse = TRUE), hjust = -0.1, size = 3) +
  scale_fill_manual(values = c("Todos los ocupados" = "#999999", "Plataformas" = "#d95f02")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Figura 6. Valores nulos", x = "% de nulos", y = NULL, fill = NULL,
       caption = "Fuente: EAH 2025 (DGEyC, GCBA).") +
  theme(legend.position = "top", plot.title.position = "plot")

# Figura 7. Calidad de datos: factor de expansión
g_pesos <- ggplot(universo, aes(fexp)) +
  geom_histogram(binwidth = 40, boundary = 0, fill = "#999999") +
  geom_histogram(data = plataformas, binwidth = 40, boundary = 0, fill = "#d95f02", alpha = 0.8) +
  geom_vline(xintercept = median(universo$fexp), linetype = "dashed") +
  labs(title = "Figura 7. Factor de expansión (fexp)",
       subtitle = "Gris: universo. Naranja: plataformas. Línea punteada: mediana.",
       x = "Personas que representa cada caso", y = "Casos",
       caption = "Fuente: EAH 2025 (DGEyC, GCBA).") +
  theme(plot.title.position = "plot")

print(g_flujo); print(g_num); print(g_cat); print(g_tasa_origen)
print(g_cajas_origen); print(g_nulos); print(g_pesos)


# Exportación a output/tablas y output/graficos ---------------------------------
if (EXPORTAR) {
  guardar_tabla(flujo,       "F1_flujo_universo")
  guardar_tabla(casos_cruce, "F1_casos_origen_det_x_plataforma")
  guardar_tabla(num_grupo,   "F2_numericas_plataformas_vs_otros")
  guardar_tabla(num_origen,  "F2_numericas_plataformas_por_origen")
  guardar_tabla(dist_cat,    "F3_categoricas_distribucion")
  guardar_tabla(tasas_cat,   "F4_tasa_plataformas_por_categoria")
  guardar_tabla(nulos_f,     "F5_nulos")
  guardar_tabla(extremos_f,  "F5_extremos_plataformas")
  guardar_tabla(pesos_f,     "F5_factor_expansion")

  guardar_grafico(g_flujo,         "gF1_flujo_universo",             11, 4)
  guardar_grafico(g_num,           "gF2_distribuciones_numericas",   10, 7)
  guardar_grafico(g_cat,           "gF3_distribuciones_categoricas", 11, 7.5)
  guardar_grafico(g_tasa_origen,   "gF4_participacion_origen",        7, 5)
  guardar_grafico(g_cajas_origen,  "gF5_perfil_origen",              11, 5)
  guardar_grafico(g_nulos,         "gF6_nulos",                       8, 4.5)
  guardar_grafico(g_pesos,         "gF7_factor_expansion",            8, 4.5)
}
