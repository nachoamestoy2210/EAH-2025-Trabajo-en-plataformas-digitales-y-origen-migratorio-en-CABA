# ==============================================================================
# 00 - CONFIGURACIÓN: paquetes, rutas, constantes y funciones propias (utils/)
# Todos los scripts empiezan con source("script/00_config.R").
# Requiere R >= 4.1 y abrir el proyecto de RStudio (proyecto.Rproj) para que las
# rutas relativas funcionen. Paquetes: install.packages(c("tidyverse", "survey"))
# ==============================================================================

library(tidyverse)
library(survey)

# 0. Configuración -------------------------------------------------------------
RUTA_BASE      <- "raw/eah2025_bu_ampliada_ind.txt"
if (!file.exists(RUTA_BASE) && interactive()) RUTA_BASE <- file.choose()  # si no está, abre el explorador
ANIO_ENCUESTA  <- 2025
ALFA           <- 0.05      # nivel de significación
B_MONTECARLO   <- 10000     # simulaciones para el p-valor Monte Carlo
SEMILLA        <- 123
K_CLUSTERS     <- 3         # cantidad de clusters del k-means (ver criterio del codo en A.7)
EXPORTAR       <- TRUE      # guarda tablas (csv) y gráficos (png)
RUTA_DATOS_LIMPIOS <- "input/eah2025_limpia.rds"
CARPETA_TABLAS     <- "output/tablas"
CARPETA_GRAFICOS   <- "output/graficos"

# Códigos (verificar con el diccionario de la EAH 2025)
ARGENTINOS      <- c(1, 2, 3, 6, 97)                        # lugar de nacimiento (m1)
EXTRANJEROS     <- c(4, 5, 98)
NULOS_NUMERICOS <- c(0, 9999, 99999, 9999999, 99999999)     # horas e ingresos

# Etiquetas del origen desagregado. 
# (el código 4 y el 5 de m1 son dos grupos distintos de extranjeros).
ETIQ_EXTRANJERO_4 <- "País no limítrofe"
ETIQ_EXTRANJERO_5 <- "País limítrofe"

# Educación: 7 niveles (Parte A) y 4 grupos (Parte B). Nombres distintos para que
# una parte no pise a la otra si se corren por separado.
ETIQ_EDUC_7 <- c("Sin instrucción", "Primario incompleto", "Primario completo",
                 "Secundario incompleto", "Secundario completo",
                 "Superior/univ. incompleto", "Superior/univ. completo")
ETIQ_EDUC_4 <- c("Hasta secundario incompleto", "Secundario completo",
                 "Superior/univ. incompleto", "Superior/univ. completo")

ETIQ_VARS <- c(sexo_f = "Sexo", grupo_edad = "Grupo de edad",
               educ_agrup = "Nivel educativo", horas_grupo = "Horas semanales",
               tercil_ingreso = "Ingreso laboral (terciles)",
               origen = "Origen (2 categorías)", origen_det = "Origen (3 categorías)",
               grupo_residencia = "Años de residencia")

COLORES      <- c("Inmigrantes" = "#1b9e77", "Nacidos en Argentina" = "#7570b3")
COLORES_PLAT <- c("Plataformas" = "#d95f02", "Otros ocupados" = "#999999")
COLORES_DET  <- setNames(c("#7570b3", "#1b9e77", "#e6ab02"),
                         c("Nacidos en Argentina", ETIQ_EXTRANJERO_4, ETIQ_EXTRANJERO_5))
FUENTE_DESC  <- "Fuente: EAH 2025 (DGEyC, GCBA). Estimaciones ponderadas por fexp."
FUENTE       <- "Fuente: EAH 2025 (DGEyC, GCBA). Porcentajes ponderados por fexp; tests sobre casos muestrales."
theme_set(theme_minimal(base_size = 12))

for (d in c("input", CARPETA_TABLAS, CARPETA_GRAFICOS)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Funciones propias (una por archivo en utils/)
walk(list.files("utils", pattern = "\\.R$", full.names = TRUE), source)



