# Reemplaza por NA los valores que estén en "codigos" o cumplan "imposible"
a_na <- function(x, codigos = NULL, imposible = FALSE) {
  replace(x, which(x %in% codigos | imposible), NA)
}
