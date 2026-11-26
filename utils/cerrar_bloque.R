# Define el p-valor a usar (Monte Carlo si hay esperadas < 5) y ajusta por Holm
cerrar_bloque <- function(res) {
  res |>
    mutate(p_usado = if_else(esperado_min < 5, p_mc, p_asint),
           p_holm  = p.adjust(p_usado, method = "holm"),
           significativo = p_holm < ALFA)
}
