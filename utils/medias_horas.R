# Media ponderada de cada grupo con IC 95 % y cantidad de encuestados
medias_horas <- function(diseno) {
  m  <- svyby(~horas, ~origen, diseno, svymean)
  tq <- qt(0.975, df = degf(diseno))
  data.frame(origen = m$origen, media = m$horas, ee = m$se,
             n = as.numeric(table(diseno$variables$origen)[as.character(m$origen)])) |>
    mutate(ic_inf = media - tq * ee, ic_sup = media + tq * ee)
}
