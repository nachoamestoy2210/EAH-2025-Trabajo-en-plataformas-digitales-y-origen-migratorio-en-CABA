# Carga los datos limpios que guarda script/01_limpieza.R (datos, ocupados, universo, ...)
cargar_datos <- function(ruta = RUTA_DATOS_LIMPIOS) {
  if (!file.exists(ruta)) stop("No existe ", ruta, ". Correr primero script/01_limpieza.R")
  list2env(readRDS(ruta), envir = globalenv())
  invisible(NULL)
}
