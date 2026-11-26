# Formato de p-valor con coma decimal (gráfico comunicativo)
fmt_p_es <- function(p) if (p < 0.001) "p < 0,001" else paste0("p = ", format(round(p, 3), decimal.mark = ","))
