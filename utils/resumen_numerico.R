# [NUEVO] Agregado desde gráficos-previos.R (usado por script/07_descriptivo_previo.R)
# Resumen ponderado (media, desvío, percentiles, asimetría) de varias variables numéricas por grupo
# vars: vector con nombre = variable y valor = etiqueta
resumen_numerico <- function(d, vars, grupo) {
  d |>
    select(all_of(c(grupo, "fexp", names(vars)))) |>
    pivot_longer(all_of(names(vars)), names_to = "var", values_to = "valor") |>
    filter(!is.na(valor)) |>
    group_by(across(all_of(grupo)), var) |>
    summarise(n         = n(),
              media     = weighted.mean(valor, fexp),
              desvio    = desvio_pond(valor, fexp),
              cv        = desvio / media,
              minimo    = min(valor),
              p10       = cuantil_ponderado(valor, fexp, 0.10),
              p25       = cuantil_ponderado(valor, fexp, 0.25),
              mediana   = cuantil_ponderado(valor, fexp, 0.50),
              p75       = cuantil_ponderado(valor, fexp, 0.75),
              p90       = cuantil_ponderado(valor, fexp, 0.90),
              maximo    = max(valor),
              asimetria = asimetria_pond(valor, fexp),
              .groups = "drop") |>
    mutate(variable = unname(vars[var]), .before = 1,
           var = factor(var, levels = names(vars))) |>
    arrange(var) |>
    select(-var)
}
