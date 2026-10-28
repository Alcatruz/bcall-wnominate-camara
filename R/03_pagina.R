# =============================================================================
# 03_pagina.R — arma datos/datos.js, lo que lee la página. Los votos de cada
# período van aparte, en datos/votos_<período>.json (02_votos.R), y la página
# los carga cuando se elige el período.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_comparar.R:
#   Rscript post_bcall_wnominate/R/03_pagina.R
#
# ESCRIBE
#   post_bcall_wnominate/datos/datos.js   (CC BY-NC-SA 4.0, trae estimaciones
#                                          de Fábrega)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

DIR <- "post_bcall_wnominate/datos"
comparacion <- read_csv(file.path(DIR, "comparacion.csv"), show_col_types = FALSE, col_types = cols(id_parlamentario = "c"))
resumen     <- read_csv(file.path(DIR, "resumen_periodos.csv"), show_col_types = FALSE)

# nombre en orden natural para la página
nombre_natural <- function(x) {
  partes <- str_split_fixed(x, ", ", 2)
  if_else(partes[, 2] == "", x, paste(partes[, 2], partes[, 1]))
}

pagina <- list(
  dip = comparacion %>%
    transmute(p = periodo, n = nombre_natural(nombre_completo), pa = replace_na(partido, "IND"), b = bloque,
              d1 = round(d1, 3), d2 = round(d2, 3), w = round(wnom, 3), x = extremo_d2_max),
  ext = comparacion %>%
    filter(extremo_d2_max) %>%
    # ap = primer apellido, para rotular el punto en la Figura 3
    transmute(p = periodo, n = nombre_natural(nombre_completo), pa = replace_na(partido, "IND"),
              ap = str_remove(str_extract(nombre_completo, "^[^,]+"), " (?:(?:Del|De la|De los|De las|De|Van|Von) )?\\S+$"),
              w = round(wnom, 3), d2 = round(d2, 3), cb = round(contra_bloque, 4), ca = round(contra_acuerdo, 4)),
  per = resumen %>%
    mutate(across(c(r_wnom, r_bay, r_dyn, rho_d2_centro), ~ round(.x, 3)),
           across(c(med_contra_bloque, med_contra_acuerdo), ~ round(.x, 4)))
)
writeLines(paste0("window.BW = ", toJSON(pagina, dataframe = "rows", na = "null", auto_unbox = TRUE), ";"),
           file.path(DIR, "datos.js"), useBytes = TRUE)
cat("listo |", round(file.size(file.path(DIR, "datos.js")) / 1024), "KB\n")
