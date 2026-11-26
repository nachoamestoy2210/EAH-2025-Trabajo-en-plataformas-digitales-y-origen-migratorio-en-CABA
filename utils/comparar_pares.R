# Comparaciones por pares (tablas 2x2) entre las categorías de una variable
comparar_pares <- function(d, var, grupo = "plataforma_f") {
  d <- d |> filter(!is.na(.data[[var]]), !is.na(.data[[grupo]])) |> droplevels()
  combn(levels(d[[var]]), 2, simplify = FALSE) |>
    map_dfr(\(par) {
      dd  <- d |> filter(.data[[var]] %in% par) |> droplevels()
      tab <- table(dd[[var]], dd[[grupo]])
      ch  <- suppressWarnings(chisq.test(tab, correct = FALSE))
      p_f <- fisher.test(tab)$p.value
      bind_cols(tibble(comparacion = paste(par, collapse = " vs. "),
                       chi2 = unname(ch$statistic), p_asint = ch$p.value, p_fisher = p_f,
                       # Mismo criterio que cerrar_bloque(): exacto si alguna esperada < 5
                       p = if (min(ch$expected) < 5) p_f else ch$p.value,
                       v_cramer = sqrt(unname(ch$statistic) / sum(tab)),
                       esperado_min = min(ch$expected)),
                or_2x2(tab))
    }) |>
    mutate(p_holm = p.adjust(p, method = "holm"))
}
