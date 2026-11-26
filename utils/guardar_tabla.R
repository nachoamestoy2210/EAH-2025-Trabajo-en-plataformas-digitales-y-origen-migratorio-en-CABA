# Guarda una tabla como csv (separador ; y coma decimal) en output/tablas/
guardar_tabla <- function(x, nombre) {
  write_excel_csv2(x, file.path(CARPETA_TABLAS, paste0(nombre, ".csv")))
}
