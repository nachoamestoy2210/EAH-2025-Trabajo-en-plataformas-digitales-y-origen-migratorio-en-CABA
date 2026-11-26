# ==============================================================================
# 03 - TESTS CHI-CUADRADO (Parte B): asociación entre ser inmigrante y trabajar
#      en plataformas
# Lee: input/eah2025_limpia.rds
# ==============================================================================
#
# Criterio de inferencia (Parte B)
#   - Chi² de Pearson sobre casos muestrales (sin ponderar).
#   - Si hay celdas con frecuencia esperada < 5: se usa el p-valor Monte Carlo
#     (en las comparaciones por pares, el test exacto de Fisher).
#   - Chi² Rao-Scott (survey::svychisq) como control: incorpora el factor de
#     expansión. La base no trae estratos ni conglomerados, por lo que solo se
#     corrige por la variabilidad de los ponderadores.
#   - Dentro de cada bloque de tests se ajusta por comparaciones múltiples (Holm).
#   - Los porcentajes descriptivos están ponderados por fexp.
# ==============================================================================

source("script/00_config.R")
cargar_datos()

# 5. ASOCIACIÓN ENTRE INMIGRANTES Y PARTICIPACIÓN EN PLATAFORMAS ---------------------

# H0: ser inmigrante y trabajar en plataformas son independientes. H1: están asociados.

# 0. Creamos la variable inmigrante (Sí/No) a partir de origen
universo <- universo |>
  mutate(inmigrante = factor(origen == "Inmigrantes",
                             levels = c(TRUE, FALSE),
                             labels = c("Inmigrante", "No inmigrante")))

# 1. Chi-cuadrado muestral con chisq.test

tabla <- table(inmigrante = universo$inmigrante,
               plataforma = universo$plataforma_f)
tabla

prueba_chi <- chisq.test(tabla, correct = FALSE)
prueba_chi
round(prueba_chi$expected, 1)   # frecuencias esperadas (supuesto: >= 5)
round(prueba_chi$stdres, 2)     # residuos estandarizados (|z| > 1.96)

# 2. Chi-cuadrado con fexp: ajuste Rao-Scott
# Diseño aproximado: sin estratos ni conglomerados.
diseno <- svydesign(ids = ~1, weights = ~fexp, data = universo)

prueba_chi_ponderada <- svychisq(~inmigrante + plataforma_f,
                                 diseno, statistic = "F")
prueba_chi_ponderada

# 3. V de Cramér

tabla_ponderada <- svytable(~inmigrante + plataforma_f, diseno)

cat("\n=== V DE CRAMÉR ===\n")
cat("V de Cramér (ponderada):", round(V_cramer(tabla_ponderada), 4), "\n")
cat("V de Cramér (muestral): ", round(V_cramer(tabla), 4), "\n")
cat("Intensidad de la asociación:",
    as.character(magnitud_v(V_cramer(tabla_ponderada))), "\n")
