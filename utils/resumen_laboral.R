# Horas e ingresos (medias y medianas ponderadas) de un grupo, por origen
resumen_laboral <- function(d) {
  d |>
    group_by(origen) |>
    summarise(casos           = n(),
              horas_media     = round(weighted.mean(horas, fexp, na.rm = TRUE), 1),
              horas_mediana   = mediana_pond(horas, fexp),
              ingreso_medio   = round(weighted.mean(ingresos, fexp, na.rm = TRUE)),
              ingreso_mediano = mediana_pond(ingresos, fexp))
}
