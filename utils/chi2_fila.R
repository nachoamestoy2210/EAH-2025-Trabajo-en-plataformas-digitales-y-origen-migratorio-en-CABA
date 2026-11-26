# Fila de resultados del test chi-cuadrado de independencia entre dos variables
chi2_fila <- function(d, var, grupo = "plataforma_f", bloque = NA_character_) {
  d   <- d |> filter(!is.na(.data[[var]]), !is.na(.data[[grupo]])) |> droplevels()
  tab <- table(d[[var]], d[[grupo]])
  
  pearson <- suppressWarnings(chisq.test(tab, correct = FALSE))
  set.seed(SEMILLA)
  p_mc <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = B_MONTECARLO))$p.value
  p_rs <- tryCatch(
    svychisq(as.formula(paste("~", var, "+", grupo)),
             svydesign(ids = ~1, weights = ~fexp, data = d))$p.value,
    error = function(e) NA_real_)
  v <- sqrt(unname(pearson$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
  
  tibble(bloque = bloque,
         variable    = unname(ETIQ_VARS[var]),
         var         = var,
         n           = sum(tab),
         chi2        = unname(pearson$statistic),
         gl          = unname(pearson$parameter),
         p_asint     = pearson$p.value,
         p_mc        = p_mc,
         p_rao_scott = unname(p_rs),
         v_cramer    = v,
         magnitud    = magnitud_v(v),
         esperado_min   = min(pearson$expected),
         pct_celdas_lt5 = mean(pearson$expected < 5) * 100)
}
