# Diferencia de medias (inmigrantes - nacidos en Argentina) con IC 95 % y test t ponderado
test_horas <- function(diseno) {
  prueba <- svyttest(horas ~ origen, design = diseno)
  dif    <- as.numeric(prueba$estimate)
  ee     <- abs(dif / as.numeric(prueba$statistic))
  gl     <- unname(prueba$parameter)
  margen <- qt(0.975, df = gl) * ee
  data.frame(diferencia_horas = dif, error_estandar = ee,
             ic95_inferior = dif - margen, ic95_superior = dif + margen,
             t = unname(prueba$statistic), gl = gl, p_valor = prueba$p.value)
}
