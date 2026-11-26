# Residuos estandarizados de Pearson: qué celdas explican la asociación (|z| > 1.96)
residuos_celdas <- function(d, var, grupo = "plataforma_f") {
  d <- d |> filter(!is.na(.data[[var]]), !is.na(.data[[grupo]])) |> droplevels()
  r <- suppressWarnings(chisq.test(table(d[[var]], d[[grupo]])))$stdres
  as.data.frame(as.table(r), stringsAsFactors = FALSE) |>
    as_tibble() |>
    set_names(c("categoria", "grupo", "residuo_std")) |>
    mutate(variable = unname(ETIQ_VARS[var]), relevante = abs(residuo_std) > 1.96, .before = 1)
}
