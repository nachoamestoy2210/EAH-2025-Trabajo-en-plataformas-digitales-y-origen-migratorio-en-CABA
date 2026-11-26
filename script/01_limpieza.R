# ==============================================================================
# 01 - LIMPIEZA: carga de la base cruda, valores nulos, errores y valores extremos
# Lee:    raw/eah2025_bu_ampliada_ind.txt
# Guarda: input/eah2025_limpia.rds (datos, ocupados, universo, plataformas, ...)
# ==============================================================================

source("script/00_config.R")

# 1. Carga y preparación de variables -----------------------------------------
base <- leer_base()

VARS_BASE <- c("id", "nhogar", "miembro", "edad", "sexo", "fexp", "m1", "m2_anio",
               "nivel", "sem_hs", "inglab_2", "aesc", "tu3", "tuso3")
faltan <- setdiff(VARS_BASE, names(base))
if (length(faltan) > 0) stop("Faltan variables en la base: ", paste(faltan, collapse = ", "))
stopifnot(is.numeric(base$fexp))   # si falla, leer con dec = ","

datos <- base |>
  transmute(
    id_persona = paste(id, nhogar, miembro, sep = "_"),
    edad, sexo, fexp = as.numeric(fexp),
    lugar_nac = m1, anio_llegada = m2_anio, educacion = nivel,
    horas = sem_hs, ingresos = inglab_2, anios_escolaridad = aesc,
    plat_principal = tu3, plat_secundario = tuso3
  ) |>
  mutate(
    # Origen (2 categorías y desagregado)
    lugar_nac = a_na(lugar_nac, c(98, 99)),
    origen = case_when(lugar_nac %in% EXTRANJEROS ~ "Inmigrantes",
                       lugar_nac %in% ARGENTINOS  ~ "Nacidos en Argentina"),
    origen = factor(origen, levels = names(COLORES)),
    origen_det = case_when(lugar_nac %in% ARGENTINOS ~ "Nacidos en Argentina",
                           lugar_nac == 4            ~ ETIQ_EXTRANJERO_4,
                           lugar_nac == 5            ~ ETIQ_EXTRANJERO_5),
    origen_det = factor(origen_det, levels = names(COLORES_DET)),
    
    # Plataformas (TRUE / FALSE / NA si no responde) y ocupados (tu3 distinto de 0 y no NA)
    plataforma = case_when(plat_principal %in% 1 | plat_secundario %in% 1 ~ TRUE,
                           plat_principal %in% 9 | plat_secundario %in% 9 ~ NA,
                           TRUE                                           ~ FALSE),
    plataforma_f = factor(plataforma, levels = c(TRUE, FALSE),
                          labels = c("Plataformas", "Otros ocupados")),
    ocupado = !is.na(plat_principal) & !plat_principal %in% 0,
    
    # Educación (8 = jardín se agrupa con 1 = sin instrucción)
    educacion       = a_na(educacion, c(0, 9)),
    educacion       = ifelse(educacion %in% 8, 1, educacion),
    nivel_educativo = factor(educacion, levels = 1:7, labels = ETIQ_EDUC_7),
    educ_agrup = case_when(educacion %in% 1:4 ~ ETIQ_EDUC_4[1],
                           educacion == 5     ~ ETIQ_EDUC_4[2],
                           educacion == 6     ~ ETIQ_EDUC_4[3],
                           educacion == 7     ~ ETIQ_EDUC_4[4]),
    educ_agrup = factor(educ_agrup, levels = ETIQ_EDUC_4),
    anios_escolaridad = a_na(anios_escolaridad, 99),
    
    # Año de llegada y años de residencia
    anio_llegada     = a_na(anio_llegada, c(0, 9999)),
    anios_residencia = ANIO_ENCUESTA - anio_llegada,
    anios_residencia = a_na(anios_residencia,
                            imposible = anios_residencia < 0 | anios_residencia > edad),
    anio_llegada     = if_else(is.na(anios_residencia), NA_real_, as.numeric(anio_llegada)),
    grupo_residencia = cut(anios_residencia, c(-Inf, 4, 14, Inf),
                           labels = c("Menos de 5 años", "5 a 14 años", "15 años o más")),
    
    # Horas e ingresos (una semana tiene 168 horas)
    horas        = a_na(horas, NULOS_NUMERICOS, horas > 168),
    ingresos     = a_na(ingresos, NULOS_NUMERICOS, ingresos < 0),
    log_ingresos = log(ingresos),
    
    # Variables categóricas para el chi-cuadrado
    sexo_f      = factor(sexo, levels = c(1, 2), labels = c("Varón", "Mujer")),
    grupo_edad  = cut(edad, c(-Inf, 29, 44, 59, Inf),
                      labels = c("Hasta 29 años", "30 a 44 años", "45 a 59 años", "60 años o más")),
    horas_grupo = cut(horas, c(-Inf, 35, 48, Inf),
                      labels = c("Hasta 35 hs", "36 a 48 hs", "Más de 48 hs"))
  )

if (anyDuplicated(datos$id_persona) > 0) warning("Hay personas duplicadas.")

# Control de codificación: contrastar con el diccionario de la EAH 2025 antes de interpretar
cat("\n--- Control de codificación (valores originales de la base) ---\n")
for (v in c("m1", "tu3", "tuso3", "nivel")) {
  cat("\n", v, ":\n", sep = ""); print(table(base[[v]], useNA = "ifany"))
}
cat("\nm2_anio (resumen):\n"); print(summary(base$m2_anio))
cat("\naesc (resumen):\n");    print(summary(base$aesc))
cat("\nPoblación ocupada (suma de fexp):",
    format(round(sum(datos$fexp[datos$ocupado], na.rm = TRUE)), big.mark = ".", decimal.mark = ","), "\n")

# Ocupados, valores extremos (IQR) y terciles de ingreso laboral.
# Los terciles se calculan entre TODOS los ocupados (ponderados), así los
# trabajadores de plataformas se ubican dentro de la distribución general.
ocupados <- datos |> filter(ocupado, fexp > 0)

lim_horas    <- limites_iqr(ocupados$horas, ocupados$fexp)
lim_ingresos <- limites_iqr(ocupados$ingresos, ocupados$fexp)
cortes_ing   <- sapply(c(1/3, 2/3), \(p) cuantil_ponderado(ocupados$ingresos, ocupados$fexp, p))

ocupados <- ocupados |>
  mutate(extremo_horas    = horas < lim_horas[["inf"]] | horas > lim_horas[["sup"]],
         extremo_ingresos = ingresos < lim_ingresos[["inf"]] | ingresos > lim_ingresos[["sup"]],
         tercil_ingreso   = cut(ingresos, c(-Inf, cortes_ing, Inf),
                                labels = c("Tercil 1 (bajo)", "Tercil 2", "Tercil 3 (alto)")))

# Universos de análisis
universo    <- ocupados |> filter(!is.na(origen), !is.na(plataforma))   # origen x plataforma
plataformas <- universo |> filter(plataforma)                           # solo plataformas

cat("\nOcupados:", nrow(ocupados), "| Universo origen x plataforma:", nrow(universo),
    "| Trabajadores de plataformas:", nrow(plataformas), "\n")
cat("Cortes de terciles de ingreso laboral ($):", format(round(cortes_ing), big.mark = ".", decimal.mark = ","), "\n")


# 2. Guardado de los datos limpios (input/) para el resto de los scripts ---------
saveRDS(list(datos = datos, ocupados = ocupados, universo = universo, plataformas = plataformas,
             lim_horas = lim_horas, lim_ingresos = lim_ingresos, cortes_ing = cortes_ing),
        RUTA_DATOS_LIMPIOS)
