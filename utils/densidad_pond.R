# [NUEVO] Agregado desde gráficos-previos.R (usado por script/07_descriptivo_previo.R)
# Densidades ponderadas, plataformas vs. otros ocupados, un panel por variable (cada grupo suma 1)
# vars: vector con nombre = variable y valor = etiqueta del panel
densidad_pond <- function(d, vars) {
  dd <- d |>
    select(plataforma_f, fexp, all_of(names(vars))) |>
    pivot_longer(all_of(names(vars)), names_to = "var", values_to = "valor") |>
    filter(!is.na(valor), !is.na(plataforma_f)) |>
    group_by(var, plataforma_f) |>
    mutate(peso = fexp / sum(fexp)) |>
    ungroup() |>
    mutate(variable = factor(unname(vars[var]), levels = unname(vars)))
  medianas <- dd |>
    group_by(variable, plataforma_f) |>
    summarise(med = cuantil_ponderado(valor, fexp, 0.5), .groups = "drop")
  ggplot(dd, aes(valor, weight = peso, fill = plataforma_f, color = plataforma_f)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = medianas, aes(xintercept = med, color = plataforma_f), linetype = "dashed") +
    facet_wrap(~ variable, scales = "free") +
    scale_fill_manual(values = COLORES_PLAT) +
    scale_color_manual(values = COLORES_PLAT) +
    labs(x = NULL, y = "Densidad", fill = NULL, color = NULL)
}
