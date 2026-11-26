# ==============================================================================
# 05 - GRÁFICO COMUNICATIVO (Parte D, Punto 3)
# Trabajo en plataformas según lugar de nacimiento.
# Lee la base cruda (raw/) y guarda output/graficos/grupo_XX_comunicativo.jpg
# Los objetos llevan sufijo _c (o nombre propio) para no pisar los de las Partes A y B.
# ==============================================================================

source("script/00_config.R")
base <- leer_base()

## D.1 Universo propio de esta parte: inmigrantes = m1 4, 5 o 98 (como en el script original)
universo_c <- base |>
  transmute(
    fexp       = as.numeric(fexp),
    origen     = case_when(m1 %in% c(4, 5, 98)       ~ "Inmigrantes",
                           m1 %in% c(1, 2, 3, 6, 97) ~ "Nacidos en Argentina"),
    plataforma = case_when(tu3 %in% 1 | tuso3 %in% 1 ~ TRUE,
                           tu3 %in% 9 | tuso3 %in% 9 ~ NA,
                           TRUE                      ~ FALSE),
    ocupado    = !tu3 %in% 0
  ) |>
  filter(ocupado, fexp > 0, !is.na(origen), !is.na(plataforma))

## D.2 Porcentajes ponderados por fexp ------------------------------------------
pct_c <- universo_c |>
  group_by(origen) |>
  summarise(pct_plataforma = sum(fexp[plataforma]) / sum(fexp) * 100)

pct_total_c <- sum(universo_c$fexp[universo_c$plataforma]) / sum(universo_c$fexp) * 100

## D.3 Chi cuadrado (Rao-Scott) y V de Cramér ponderada -------------------------
diseno_c <- svydesign(ids = ~1, weights = ~fexp,
                      data = mutate(universo_c, plataforma = factor(plataforma),
                                    origen = factor(origen)))
tabla_pond_c <- svytable(~plataforma + origen, diseno_c)

tests_c <- list(
  p_usado  = svychisq(~plataforma + origen, diseno_c, statistic = "F")$p.value,
  v_cramer = sqrt(chisq.test(as.matrix(tabla_pond_c), correct = FALSE)$statistic /
                    sum(tabla_pond_c))
)


## D.4 Gráfico comunicativo ------------------------------------------------------
pct_inm <- pct_c$pct_plataforma[pct_c$origen == "Inmigrantes"]
pct_arg <- pct_c$pct_plataforma[pct_c$origen == "Nacidos en Argentina"]
veces   <- pct_inm / pct_arg
brecha  <- pct_inm - pct_arg

g_comunicativo <- pct_c |>
  mutate(origen = fct_relevel(origen, "Nacidos en Argentina", "Inmigrantes"),
         y      = as.numeric(origen),
         color  = ifelse(origen == "Inmigrantes", "#1b9e77", "#7570b3"),
         tam    = ifelse(origen == "Inmigrantes", 9, 6)) |>
  ggplot() +
  geom_vline(xintercept = pct_total_c, linetype = "dashed", color = "#999999", linewidth = 0.7) +
  annotate("text", x = pct_total_c + 0.15, y = 2.35, hjust = 0, size = 3.2, color = "#666666",
           label = paste0("Promedio de todos\nlos ocupados: ",
                          format(round(pct_total_c, 1), decimal.mark = ","), "%")) +
  geom_segment(aes(x = pct_arg, xend = pct_inm, y = 1, yend = 2),
               color = "#c9c9c9", linewidth = 1.6, lineend = "round") +
  annotate("curve", x = pct_arg + 0.15, y = 1.12, xend = pct_inm - 0.15, yend = 1.92,
           curvature = -0.25, color = "#1b9e77", linewidth = 0.9,
           arrow = arrow(length = unit(3, "mm"), type = "closed")) +
  annotate("text", x = (pct_arg + pct_inm) / 2, y = 1.28, angle = 12, size = 3.4,
           color = "#1b9e77", fontface = "bold",
           label = paste0("brecha de +", format(round(brecha, 1), decimal.mark = ","), " p.p.")) +
  geom_point(aes(pct_plataforma, y, color = color, size = tam)) +
  scale_color_identity() +
  scale_size_identity() +
  annotate("text", x = pct_arg - 0.15, y = 1, hjust = 1, size = 4.2,
           color = "#7570b3", fontface = "bold",
           label = paste0(format(round(pct_arg, 1), decimal.mark = ","), "%")) +
  annotate("text", x = pct_inm + 0.15, y = 2, hjust = 0, size = 5.4,
           color = "#1b9e77", fontface = "bold",
           label = paste0(format(round(pct_inm, 1), decimal.mark = ","), "%")) +
  annotate("text", x = pct_inm + 1.6, y = 2, hjust = 0, size = 3.8,
           color = "#1b9e77", fontface = "bold", lineheight = 0.95,
           label = paste0(format(round(veces, 1), decimal.mark = ","), " veces más\nprobabilidad")) +
  scale_y_continuous(breaks = c(1, 2), limits = c(0.6, 2.5),
                     labels = c("Nacidos en\nArgentina", "Inmigrantes")) +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.22)),
                     labels = scales::label_number(suffix = "%", decimal.mark = ",")) +
  labs(title = str_wrap(paste0("Los inmigrantes tienen ", format(round(veces, 1), decimal.mark = ","),
                               " veces más probabilidad de trabajar en plataformas"), 55),
       subtitle = str_wrap(paste0("Porcentaje de ocupados que trabajan en plataformas digitales según lugar de nacimiento. ",
                                  "Ciudad de Buenos Aires, ", ANIO_ENCUESTA,
                                  ". Estimaciones ponderadas por factor de expansión (fexp)."), 95),
       x = NULL, y = NULL,
       caption = paste0("Fuente: EAH ", ANIO_ENCUESTA, " (DGEyC, GCBA). Chi-cuadrado (Rao-Scott): ",
                        fmt_p_es(tests_c$p_usado),
                        " | V de Cramér = ", format(round(tests_c$v_cramer, 2), decimal.mark = ","), ".")) +
  theme_minimal(base_size = 13) +
  theme(plot.title          = element_text(face = "bold", size = 17, margin = margin(b = 4)),
        plot.subtitle       = element_text(color = "#555555", size = 10.5, lineheight = 1.15,
                                           margin = margin(b = 14)),
        plot.caption        = element_text(color = "#888888", size = 8.5, hjust = 0, margin = margin(t = 12)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid          = element_blank(),
        axis.text.y         = element_text(size = 12, color = "black"),
        axis.text.x         = element_text(color = "#777777", size = 9.5),
        axis.line.x         = element_line(color = "#cccccc"),
        axis.ticks          = element_blank(),
        plot.margin         = margin(14, 18, 10, 14))

print(g_comunicativo)

ggsave(file.path(CARPETA_GRAFICOS, "grupo_13_comunicativo.jpg"), g_comunicativo,
      
       width = 22, height = 14, units = "cm", dpi = 300)
