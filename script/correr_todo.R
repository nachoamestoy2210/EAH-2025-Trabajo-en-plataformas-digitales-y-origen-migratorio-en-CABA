# ==============================================================================
# CORRER TODO: ejecuta el proyecto en orden (abrir antes proyecto.Rproj en RStudio).
# Cada script también se puede correr por separado: carga solo su configuración
# y los datos que necesita (01_limpieza.R debe haberse corrido al menos una vez).
# Además de generar tablas y gráficos, guarda la salida de consola (tests
# chi-cuadrado, V de Cramér, etc.) en output/resultados_consola.txt
# ==============================================================================

scripts <- c("01_limpieza.R", "02_descriptivo.R", "03_tests_chi2.R",
             "04_grafico_exploratorio.R", "05_grafico_comunicativo.R",
             "06_metodo_cuantitativo.R", "07_descriptivo_previo.R")

dir.create("output", showWarnings = FALSE)
sink("output/resultados_consola.txt", split = TRUE)
tryCatch({
  for (s in scripts) {
    cat("\n>>> Corriendo", s, "\n")
    source(file.path("script", s), print.eval = TRUE)
  }
}, finally = sink())
