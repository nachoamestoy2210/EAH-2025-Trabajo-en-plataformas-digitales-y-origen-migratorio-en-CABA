# Formato de p-valores para tablas y gráficos
fmt_p <- function(p) ifelse(p < 0.001, "p < 0.001", paste0("p = ", formatC(p, format = "f", digits = 3)))
