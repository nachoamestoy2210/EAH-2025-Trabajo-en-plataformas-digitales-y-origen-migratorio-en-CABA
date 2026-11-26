# INMIGRACIÓN Y TRABAJO EN PLATAFORMAS

## Integrantes

- Amestoy, Ignacio
- Natale, Ignacio
- Trovato, Maximiliano Adrián

## Objetivo

El eje central de este trabajo es explorar la supuesta correlación entre la condición inmigratoria de una persona y si trabaja en alguna de las aplicaciones (tal como PedidoYa). Por ello, queremos dar respuesta a nuestra hipótesis principal: "Entre las personas ocupadas de CABA, ¿la proporción que trabaja en plataformas digitales (en la ocupación principal o en la secundaria) es mayor entre inmigrantes que entre nacidos en Argentina?" Para ello, entendemos que la base de datos que mejor se puede adaptar a este trabajo es la Encuesta Anual de Hogares producida por el instituto público de estadística DGEyC en base a 12.457 registros. No vamos a concentrar en el año 2025 y con el sector de individuos que tiene la base

## Datos

- **Fuente principal:** [EAH — IDECBA](https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2026/04/eah2025_bu_ampliada.zip)
- **Período:** 2025
- **Unidad de análisis:** individuos

## Análisis realizado

1.  Análisis estadístico y gráfico de las variables con sus respectivas características muestrales
2.  Limpieza de los respectivos valores faltantes o extremos
3.  Utilización de métodos cuantitativos (clusters, v de Cramer y diferencia de medias)
4.  Armado de un gráfico exploratorio que resumiera lo más posible el análisis anterior
5.  Gráfico comunicativo a partir de todos los hallazgos encontrados.
6.  Formulación de la conclusión y aceptación de la hipótesis.

## Estructura del repositorio

```         
proyecto/
├── raw/          # Base cruda, tal como se descargó. No se modifica.
├── auxiliar/     # Diccionario de variables y archivos de apoyo
├── input/        # Datos limpios para el análisis (los genera 01_limpieza.R)
├── output/
│   ├── tablas/   # Tablas de resultados (csv)
│   └── graficos/ # Gráficos (png y jpg)
├── script/       # Un script por etapa del trabajo
├── utils/        # Funciones propias (un archivo por función)
├── proyecto.Rproj
└── README.md
```

## Pasos para su correcta reproducción

1.  Abrir `proyecto.Rproj` con RStudio (así las rutas relativas funcionan).
2.  Instalar los paquetes: `install.packages(c("tidyverse", "survey"))`.
3.  Correr `script/correr_todo.R`, o cada script por separado en el orden de la tabla.

(En caso que no corra correctamente, por favor chequear que estás posicionado sobre el directorio correcto con getwd())

## Scripts

| Script | Qué hace | Lee | Guarda |
|----|----|----|----|
| `00_config.R` | Paquetes, rutas, constantes y carga de `utils/` (lo usan todos) | — | — |
| `01_limpieza.R` | Carga la base, convierte nulos y errores en NA, marca valores extremos | `raw/` | `input/eah2025_limpia.rds` |
| `02_descriptivo.R` | Plataformas por origen, edad, llegada, horas e ingresos, educación, clusters (k-means), tablas de nulos y extremos | `input/` | `output/tablas`, `output/graficos` |
| `03_tests_chi2.R` | Chi-cuadrado (Pearson y Rao-Scott) y V de Cramér: origen y plataformas | `input/` | consola |
| `04_grafico_exploratorio.R` | Gráfico exploratorio: participación por edad, educación y origen | `input/` | `output/graficos` |
| `05_grafico_comunicativo.R` | Gráfico comunicativo: trabajo en plataformas según origen | `raw/` | `output/graficos` |
| `06_metodo_cuantitativo.R` | Test t ponderado de horas semanales según origen, con y sin extremos | `input/` | `output/tablas`, `output/graficos` |
| `07_descriptivo_previo.R` | Estadísticas y gráficos previos a los tests: flujo del universo, variables numéricas y categóricas, tasa de participación por categoría y calidad de datos (nulos, extremos, factor de expansión) | `input/` | `output/tablas`, `output/graficos` |

`01_limpieza.R` tiene que correrse al menos una vez antes de los scripts 02, 03, 04, 06 y 07.

## Aclaraciones metodológicas

- **La ponderación de los datos:** todas las estimaciones usan `fexp`; las tablas informan también la cantidad de casos de la muestra.
- **Valores nulos:** los códigos de no respuesta se convierten en NA variable por variable. No se imputa ni se elimina ninguna variable: cada cálculo excluye solo a quienes no tienen el dato que necesita.
- **Valores extremos:** se detectan con el rango intercuartílico (1,5 × IQR, cuartiles ponderados). Los datos imposibles pasan a NA y los extremos posibles se mantienen y se marcan. Se compara el resultado con y sin extremos.

## Conclusiones finales

En conclusión, la evidencia respalda nuestra hipótesis: los ocupados inmigrantes participan casi tres veces más en plataformas que los nacidos en Argentina. Por medio del test de Chi-cuadrado pudimos demostrar que ser inmigrante está asociado a trabajar en plataformas. Además, hallamos que los inmigrantes sostienen jornadas semanales más extensas para obtener un salario total menor. Por lo que su remuneración por hora trabajada también es menor. También determinamos que el grupo extranjero se concentra especialmente en un perfil de alta escolaridad y carga horaria. A pesar de todo lo mencionado, como contamos con una V de Cramér débil y el diseño observacional de la encuesta, no podemos atribuir estas diferencias causalmente al origen migratorio.
