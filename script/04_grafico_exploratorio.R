# ==============================================================================
# 04 - GRÁFICO EXPLORATORIO (Parte C, Punto 2)
# Participación en plataformas por edad, educación y origen.
# Lee: input/eah2025_limpia.rds | Guarda: output/graficos/grupo_XX_exploratorio.jpg
# Usa el universo de la Parte A (ocupados con origen y plataforma informados);
# educacion es nivel ya recodificado (nivel 1 a 8 equivale a educacion 1 a 7).
# ==============================================================================

source("script/00_config.R")
cargar_datos()

tasa_general <- with(universo, weighted.mean(plataforma, fexp))
tramos <- c("Hasta 29", "30 a 44", "45 a 59", "60 o más")
niveles <- c("Sin superior completo", "Superior completo")
colores <- c("Inmigrantes" = "#0C826B", "Nacidos en Argentina" = "#6F62A6")

# Cada tasa usa como denominador a todos los ocupados del mismo grupo.
estratos <- universo |>
  filter(!is.na(edad), educacion %in% 1:7) |>
  mutate(
    tramo = cut(edad, c(-Inf, 29, 44, 59, Inf), labels = tramos),
    educacion = factor(
      if_else(educacion == 7, "Superior completo", "Sin superior completo"),
      levels = niveles
    ),
    origen = factor(origen, levels = names(colores))
  )

tasas <- estratos |>
  group_by(educacion, tramo, origen) |>
  summarise(
    tasa = weighted.mean(plataforma, fexp),
    casos = n(),
    casos_plataforma = sum(plataforma),
    .groups = "drop"
  ) |>
  mutate(
    pocos_casos = if_else(casos_plataforma < 5, "Menos de 5", "5 o más"),
    # Separación horizontal para que se distingan los puntos cercanos.
    posicion = as.integer(tramo) +
      if_else(origen == "Inmigrantes", -0.18, 0.18)
  )

formato_porcentaje <- scales::label_percent(
  accuracy = 0.01, decimal.mark = ",", suffix = " %"
)

g_exploratorio <- ggplot(tasas, aes(posicion, tasa)) +
  geom_hline(
    yintercept = tasa_general, linetype = "dashed",
    colour = "#77858D", linewidth = 0.55
  ) +
  geom_line(aes(group = tramo), colour = "#B6BFC5", linewidth = 0.6) +
  geom_point(aes(colour = origen, size = casos, shape = pocos_casos), stroke = 1.05) +
  facet_wrap(~ educacion, nrow = 1) +
  scale_colour_manual(values = colores) +
  scale_shape_manual(values = c("Menos de 5" = 1, "5 o más" = 16), guide = "none") +
  scale_size_area(max_size = 8, breaks = c(50, 250, 500, 900)) +
  expand_limits(y = c(0, 0.15)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.05),
    labels = scales::label_percent(accuracy = 1, suffix = " %"),
    expand = expansion(mult = c(0.025, 0.05)),
    sec.axis = dup_axis(breaks = tasa_general, labels = formato_porcentaje(tasa_general),
                        name = NULL)
  ) +
  scale_x_continuous(
    breaks = seq_along(tramos), labels = tramos,
    limits = c(0.5, length(tramos) + 0.5), expand = expansion(mult = 0)
  ) +
  guides(
    colour = guide_legend(order = 1, override.aes = list(size = 4, shape = 16)),
    size = guide_legend(order = 2, override.aes = list(colour = "#6C7981", shape = 16))
  ) +
  labs(
    title = "Trabajo en plataformas por edad, educación y origen",
    subtitle = "Personas ocupadas de CABA · EAH 2025 · Ocupación principal o secundaria",
    x = "Edad en años",
    y = "Porcentaje de ocupados que\ntrabaja en plataformas",
    colour = NULL,
    size = "Ocupados en la muestra (n)",
    caption = paste0(
      "Línea punteada en la tasa general (", formato_porcentaje(tasa_general),
      "). Círculo hueco para grupos con menos de 5 casos en plataformas.\n",
      "Fuente EAH 2025, base individual ampliada. Tasas ponderadas por fexp. ",
      format(nrow(estratos), big.mark = ".", decimal.mark = ",",
             scientific = FALSE, trim = TRUE),
      " casos con edad y educación informadas."
    )
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    plot.title = element_text(size = 19, face = "bold", colour = "#172634",
                              margin = margin(b = 6)),
    plot.subtitle = element_text(size = 11, colour = "#415362"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 9.5, hjust = 0, colour = "#415362",
                                lineheight = 1.15, margin = margin(t = 13)),
    plot.margin = margin(16, 18, 14, 16),
    strip.background = element_rect(fill = "#EDF2F4", colour = NA),
    strip.text = element_text(face = "bold", colour = "#253747", hjust = 0,
                              margin = margin(8, 8, 8, 8)),
    legend.position = "top",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.box.spacing = grid::unit(0.25, "cm"),
    legend.text = element_text(size = 10.5),
    legend.title = element_text(size = 10.5),
    panel.spacing.x = grid::unit(0.75, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E4EAEE", linewidth = 0.45),
    axis.text = element_text(colour = "#334654"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

print(g_exploratorio)  # muestra el gráfico en el panel Plots de RStudio

ggsave(
  filename = file.path(CARPETA_GRAFICOS, "grupo_13_exploratorio.jpg"),
  plot = g_exploratorio, device = "jpeg",
  width = 24, height = 15, units = "cm", dpi = 300, bg = "white", quality = 95
)
