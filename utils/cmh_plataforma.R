# Cochran-Mantel-Haenszel: asociación fila x plataforma controlando por un estrato.
# "fila" debe tener exactamente 2 categorías.
cmh_plataforma <- function(d, fila, estrato) {
  d   <- d |> filter(!is.na(.data[[fila]]), !is.na(.data[[estrato]])) |> droplevels()
  tab <- table(d[[fila]], d$plataforma_f, d[[estrato]])
  mh  <- mantelhaen.test(tab)
  por_estrato <- map_dfr(seq_len(dim(tab)[3]), \(k) {
    bind_cols(tibble(control = unname(ETIQ_VARS[estrato]), estrato = dimnames(tab)[[3]][k],
                     casos = sum(tab[, , k]), casos_plataforma = sum(tab[, 1, k])),
              or_2x2(tab[, , k]))
  })
  # Test de homogeneidad de Woolf: H0 = el OR es el mismo en todos los estratos.
  # Si se rechaza, el OR de Mantel-Haenszel esconde diferencias entre estratos.
  w      <- 1 / por_estrato$se_log^2
  lor    <- log(por_estrato$or)
  chi2_h <- sum(w * (lor - weighted.mean(lor, w))^2)
  list(
    resumen = tibble(control = unname(ETIQ_VARS[estrato]), n = sum(tab),
                     or_crudo = or_2x2(margin.table(tab, c(1, 2)))$or,
                     or_mh    = unname(mh$estimate),
                     ic_inf   = mh$conf.int[1], ic_sup = mh$conf.int[2],
                     chi2_mh  = unname(mh$statistic), p = mh$p.value,
                     p_homogeneidad = pchisq(chi2_h, df = nrow(por_estrato) - 1, lower.tail = FALSE)),
    por_estrato = por_estrato
  )
}
