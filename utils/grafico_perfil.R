# Gráfico de perfil: barras horizontales por variable, con el test en cada panel
grafico_perfil <- function(dist, resumen, colores, titulo, subtitulo, leyenda) {
  etiquetas <- resumen |>
    transmute(variable, panel = paste0(variable, "\nChi² (p ajustado Holm): ", fmt_p(p_holm),
                                       " | V = ", sprintf("%.2f", v_cramer)))
  dist |>
    left_join(etiquetas, by = "variable") |>
    mutate(panel = fct_inorder(panel)) |>
    ggplot(aes(pct_pond, fct_rev(categoria), fill = grupo)) +
    geom_col(position = position_dodge2(reverse = TRUE), width = 0.8) +
    geom_text(aes(label = sprintf("%.0f%%", pct_pond)),
              position = position_dodge2(width = 0.8, reverse = TRUE), hjust = -0.15, size = 3) +
    facet_wrap(~ panel, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = colores) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = titulo, subtitle = subtitulo, x = "% dentro de cada grupo", y = NULL,
         fill = leyenda, caption = FUENTE) +
    theme(legend.position = "top", strip.text = element_text(face = "bold", hjust = 0))
}
