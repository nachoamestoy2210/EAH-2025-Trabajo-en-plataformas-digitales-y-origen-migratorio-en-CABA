# ==============================================================================
# 06 - MÉTODO CUANTITATIVO (Parte E): diferencia de medias de horas semanales
# Lee: input/eah2025_limpia.rds | Guarda: output/tablas y output/graficos
# ¿Los inmigrantes que trabajan en plataformas trabajan más horas que los nacidos
# en Argentina? Test t ponderado por fexp (survey::svyttest), bilateral.
#   H0: la media de horas es igual en ambos grupos.  H1: la media difiere.
# Referencia: nacidos en Argentina. La brecha es inmigrantes menos referencia.
# ==============================================================================

source("script/00_config.R")
cargar_datos()

# Diseño muestral sobre los ocupados (sin estratos ni conglomerados, como en la Parte B)
diseno_ocupados <- svydesign(ids = ~1, weights = ~fexp, data = as.data.frame(ocupados))

# Trabajadores de plataformas con origen y horas informados
diseno_horas <- subset(diseno_ocupados, plataforma %in% TRUE & !is.na(origen) & !is.na(horas))
diseno_horas <- update(diseno_horas, origen = relevel(origen, ref = "Nacidos en Argentina"))

# 1. Número de encuestados y media ponderada por grupo
print(table(diseno_horas$variables$origen))
print(svyby(~horas, ~origen, diseno_horas, svymean))

# 2. Test bilateral ponderado por fexp
print(svyttest(horas ~ origen, design = diseno_horas))

# 3. Brecha, error estándar e IC 95 %; sensibilidad repitiendo sin los extremos de horas
disenos_horas <- list("Todos los casos"        = diseno_horas,
                      "Sin extremos de horas"  = subset(diseno_horas, !extremo_horas))

comparacion_horas <- bind_rows(lapply(disenos_horas, test_horas), .id = "escenario")
mostrar("Diferencia de horas semanales (inmigrantes - nacidos en Argentina)", comparacion_horas)

# 4. Gráfico: media de cada grupo con IC 95 % y la brecha, con y sin extremos.
# Cómo leerlo: la brecha es la distancia entre la línea punteada (media de los nacidos
# en Argentina) y el punto de los inmigrantes. Si el IC 95 % de la diferencia no incluye
# el 0 (p < 0,05), se rechaza H0. Si la brecha casi no cambia sin extremos, el
# resultado no depende de esos casos.

graf_horas <- bind_rows(lapply(disenos_horas, medias_horas), .id = "escenario") |>
  mutate(escenario = factor(escenario, levels = names(disenos_horas)),
         origen    = factor(origen, levels = names(COLORES)[2:1]))

anot_horas <- comparacion_horas |>
  mutate(escenario = factor(escenario, levels = names(disenos_horas)),
         texto = paste0("Diferencia: ", ifelse(diferencia_horas > 0, "+", ""), coma1(diferencia_horas), " h\n",
                        "IC 95 %: ", coma1(ic95_inferior), " a ", coma1(ic95_superior), "\n",
                        ifelse(p_valor < 0.001, "p < 0,001",
                               paste0("p = ", format(round(p_valor, 3), decimal.mark = ",")))))

dif_horas <- comparacion_horas[1, ]
titulo_horas <- if (dif_horas$p_valor < ALFA) {
  paste0("Los inmigrantes en plataformas trabajan ", coma1(abs(dif_horas$diferencia_horas)), " horas ",
         ifelse(dif_horas$diferencia_horas > 0, "más", "menos"), " por semana que los nacidos en Argentina")
} else {
  "Las horas semanales en plataformas no difieren significativamente según el origen"
}

g_horas <- ggplot(graf_horas, aes(origen, media, color = origen)) +
  geom_hline(data = filter(graf_horas, origen == "Nacidos en Argentina"),
             aes(yintercept = media), linetype = "dashed", color = "#999999") +
  geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.12, linewidth = 0.9) +
  geom_point(size = 4.5) +
  geom_text(aes(label = paste0(coma1(media), " h\n(n = ", n, ")")),
            hjust = -0.3, size = 3.6, lineheight = 0.95) +
  geom_text(data = anot_horas, aes(x = 1.5, y = Inf, label = texto), inherit.aes = FALSE,
            vjust = 1.2, size = 3.6, color = "#444444", lineheight = 1.05) +
  facet_wrap(~ escenario) +
  scale_color_manual(values = COLORES) +
  scale_x_discrete(labels = scales::label_wrap(12), expand = expansion(add = 0.9)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) +
  expand_limits(y = 0) +
  labs(title = str_wrap(titulo_horas, 60),
       subtitle = str_wrap(paste0("Horas semanales medias de los trabajadores de plataformas según lugar de nacimiento. ",
                                  "Puntos: media ponderada. Barras: intervalo de confianza del 95 %. ",
                                  "Línea punteada: media de los nacidos en Argentina."), 90),
       x = NULL, y = "Horas semanales (media ponderada)",
       caption = str_wrap(paste0(FUENTE_DESC, " Test t ponderado (diseño sin estratos ni conglomerados). ",
                                 "«Sin extremos» excluye las horas fuera de Q1 - 1,5 IQR y Q3 + 1,5 IQR ",
                                 "de todos los ocupados."), 110)) +
  theme(legend.position = "none", plot.title.position = "plot", plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 15), strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, color = "#888888", size = 8.5),
        panel.grid.minor = element_blank())

print(g_horas)

if (EXPORTAR) {
  guardar_tabla(comparacion_horas, "E_diferencia_horas_origen")
  guardar_grafico(g_horas, "gE_diferencia_horas_origen", 9, 5.5)
}
