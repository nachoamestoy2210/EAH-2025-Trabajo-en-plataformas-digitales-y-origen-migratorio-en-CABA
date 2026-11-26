# [NUEVO] Agregado desde gráficos-previos.R (usado por script/07_descriptivo_previo.R)
# % ponderado de ocupados en plataformas dentro de cada categoría de una variable
tasa_plataforma <- function(d, var) {
  d |>
    filter(!is.na(.data[[var]])) |>
    group_by(categoria = as.character(.data[[var]])) |>
    summarise(casos            = n(),
              casos_plataforma = sum(plataforma),
              pct_plataforma   = sum(fexp[plataforma]) / sum(fexp) * 100,
              .groups = "drop") |>
    mutate(variable = unname(ETIQ_VARS[var]), .before = 1)
}
