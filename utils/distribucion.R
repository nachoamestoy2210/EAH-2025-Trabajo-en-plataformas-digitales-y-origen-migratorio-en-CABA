# Distribución porcentual (ponderada) de una variable dentro de cada grupo
distribucion <- function(d, var, grupo = "plataforma_f") {
  d |>
    filter(!is.na(.data[[var]]), !is.na(.data[[grupo]])) |>
    droplevels() |>
    group_by(grupo = .data[[grupo]], categoria = .data[[var]]) |>
    summarise(casos = n(), personas = sum(fexp), .groups = "drop") |>
    complete(grupo, categoria, fill = list(casos = 0L, personas = 0)) |>
    group_by(grupo) |>
    mutate(pct_pond    = personas / sum(personas) * 100,
           pct_muestra = casos / sum(casos) * 100) |>
    ungroup() |>
    mutate(variable = unname(ETIQ_VARS[var]), .before = 1)
}
