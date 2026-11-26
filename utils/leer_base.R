# Lee la base cruda (raw/) y pasa los nombres de las variables a minúscula
leer_base <- function(ruta = RUTA_BASE) {
  base <- read.delim(ruta, sep = ";")
  names(base) <- tolower(names(base))
  base
}
