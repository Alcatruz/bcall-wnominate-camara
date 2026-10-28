# =============================================================================
# 01_comparar.R — B-Call por período contra las estimaciones de Fábrega (2025)
# para la Cámara de Diputados, 2002-2026.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/:
#   Rscript post_bcall_wnominate/R/01_comparar.R
#
# FUENTES
#   data/fabrega_2025/                 depósito de Fábrega en Harvard Dataverse,
#                                      doi:10.7910/DVN/FOXOIT, versión 2.0, sin
#                                      modificar (CC BY-NC-SA 4.0)
#   post_bcall_periodos/datos/bcall_periodos.csv
#                                      d1, d2 y bloque B-Call por diputado y período
#   outputs/data/post_bcall/votaciones_periodo.rds, votos_periodo.rds
#                                      las votaciones y votos con que se estimó B-Call
#                                      (los arma post_bcall_periodos/R/01_base.R)
#
# DECISIONES
#   1. Se compara dentro de cada período, diputado por diputado, con el id de
#      la Cámara. 2026-2030 queda fuera porque Fábrega no lo cubre.
#   2. Pablo Prieto Lorca figura en Fábrega con el id 209 también en 2018-2022,
#      donde la Cámara usa el 1064. Se recodifica a 1064 en ese período.
#   3. En sus matrices, Afirmativo = 1, En Contra = -1, Abstención = 0, igual
#      que en B-Call. Dispensado, No Vota y NA cuentan como sin voto.
#   4. d1 se compara con Pearson. d2 contra la distancia al centro de
#      W-NOMINATE, |wnom|, con Spearman.
#
# ESCRIBE (en post_bcall_wnominate/datos/)
#   comparacion.csv        una fila por diputado x período, con las
#                          estimaciones de los dos lados (CC BY-NC-SA 4.0)
#   resumen_periodos.csv   una fila por período
#   datos.js               lo que lee la página (CC BY-NC-SA 4.0)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

if (!file.exists("data/votaciones.rds")) stop("Correr desde la raíz de bcall_nominate_chile/.")
DIR_FAB <- "data/fabrega_2025"
DIR_OUT <- "post_bcall_wnominate/datos"
if (!dir.exists(DIR_FAB)) stop("Falta ", DIR_FAB, ". Bajar el depósito de https://doi.org/10.7910/DVN/FOXOIT.")

PER <- c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022", "2022-2026")
ARCH_MATRIZ <- c("2002_06", "2006_10", "2010_14", "2014_18", "2018_22", "2022_26")


# ---------------------------------------------------------------------------
# PASO 1 — estimaciones de los dos lados
# ---------------------------------------------------------------------------
# El formato largo de Fábrega trae una fila por diputado y período aunque el
# diputado no haya estado en el período; esas filas no tienen estimación.
fab <- read_tsv(file.path(DIR_FAB, "ideologia_congreso_chile_2002_2026_long_format.tab"),
                na = c("", "NA"), show_col_types = FALSE) %>%
  transmute(id_parlamentario = as.character(Id),
            periodo = PER[as.integer(sub("Periodo_", "", Period))],
            wnom = wnom_ideology, bay = bay_ideology, dyn = dim_ideology) %>%
  filter(!(is.na(wnom) & is.na(bay) & is.na(dyn))) %>%
  mutate(id_parlamentario = if_else(id_parlamentario == "209" & periodo == "2018-2022", "1064", id_parlamentario))

bc <- read_csv("post_bcall_periodos/datos/bcall_periodos.csv", show_col_types = FALSE,
               col_types = cols(id_parlamentario = "c")) %>%
  filter(periodo %in% PER)

cmp <- inner_join(bc, fab, by = c("id_parlamentario", "periodo"))
cat("diputado x período comparados:", nrow(cmp), "\n")

solo_bc  <- anti_join(bc, fab, by = c("id_parlamentario", "periodo"))
solo_fab <- anti_join(fab, bc, by = c("id_parlamentario", "periodo"))
cat("solo en B-Call:", nrow(solo_bc), "| solo en Fábrega:", nrow(solo_fab), "\n")

# votos emitidos por quienes Fábrega estima y B-Call no
votos_bc <- read_rds("outputs/data/post_bcall/votos_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion), id_parlamentario = as.character(id_parlamentario))
print.data.frame(solo_fab %>%
  left_join(votos_bc %>% filter(!is.na(voto_num)) %>% count(periodo, id_parlamentario, name = "votos_emitidos"),
            by = c("periodo", "id_parlamentario")), row.names = FALSE)


# ---------------------------------------------------------------------------
# PASO 2 — votaciones y votos
# ---------------------------------------------------------------------------
lee_matriz <- function(k) {
  read_tsv(file.path(DIR_FAB, paste0("matriz__periodo_", ARCH_MATRIZ[k], ".tab")),
           col_types = cols(.default = "c"), na = character(), show_col_types = FALSE) %>%
    select(-1, -Nombre, -ApellidoPaterno, -ApellidoMaterno) %>%
    pivot_longer(-DiputadoId, names_to = "id_votacion", values_to = "voto_fab") %>%
    mutate(periodo = PER[k])
}
mf <- map_dfr(seq_along(PER), lee_matriz)

vot_fab <- distinct(mf, periodo, id_votacion)
vot_bc  <- read_rds("outputs/data/post_bcall/votaciones_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion)) %>%
  filter(periodo %in% PER)

solo_vf <- anti_join(vot_fab, vot_bc, by = "id_votacion")
if (nrow(solo_vf)) warning(nrow(solo_vf), " votaciones de Fábrega no están en la base de B-Call")
solo_vb <- anti_join(vot_bc, vot_fab, by = "id_votacion") %>%
  mutate(despues_fin_fabrega = fecha > max(semi_join(vot_bc, vot_fab, by = "id_votacion")$fecha))

celdas <- mf %>%
  semi_join(vot_bc, by = "id_votacion") %>%
  mutate(fab_num = case_when(voto_fab == "Afirmativo" ~ 1,
                             voto_fab == "En Contra"  ~ -1,
                             voto_fab %in% c("Abstencion", "Abstención") ~ 0,
                             TRUE ~ NA_real_)) %>%
  left_join(votos_bc %>% select(id_votacion, id_parlamentario, voto_num),
            by = c("id_votacion", "DiputadoId" = "id_parlamentario"))

votos_per <- celdas %>%
  group_by(periodo) %>%
  summarise(votos_iguales   = sum(!is.na(fab_num) & !is.na(voto_num) & fab_num == voto_num),
            votos_distintos = sum(!is.na(fab_num) & !is.na(voto_num) & fab_num != voto_num),
            solo_un_lado    = sum(is.na(fab_num) != is.na(voto_num)),
            .groups = "drop")


# ---------------------------------------------------------------------------
# PASO 3 — resumen por período
# ---------------------------------------------------------------------------
corr <- cmp %>%
  group_by(periodo) %>%
  summarise(diputados = n(),
            r_wnom = cor(d1, wnom, use = "complete.obs"),
            r_bay  = cor(d1, bay,  use = "complete.obs"),
            r_dyn  = cor(d1, dyn,  use = "complete.obs"),
            rho_d2_centro = cor(d2, abs(wnom), use = "complete.obs", method = "spearman"),
            topan_wnom = sum(abs(wnom) >= 0.99, na.rm = TRUE),
            .groups = "drop")

resumen <- count(vot_fab, periodo, name = "votaciones_fabrega") %>%
  left_join(count(vot_bc, periodo, name = "votaciones_bcall"), by = "periodo") %>%
  left_join(count(semi_join(vot_fab, vot_bc, by = "id_votacion"), periodo, name = "votaciones_comunes"), by = "periodo") %>%
  left_join(count(solo_vb, periodo, despues_fin_fabrega) %>%
              pivot_wider(names_from = despues_fin_fabrega, values_from = n, values_fill = 0) %>%
              transmute(periodo, solo_bcall_antes = `FALSE`, solo_bcall_despues = if ("TRUE" %in% names(.)) `TRUE` else 0L),
            by = "periodo") %>%
  left_join(votos_per, by = "periodo") %>%
  left_join(corr, by = "periodo") %>%
  mutate(across(c(solo_bcall_antes, solo_bcall_despues), ~ replace_na(.x, 0L)))

print.data.frame(resumen %>% mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)
cat("\núltima votación de Fábrega:", format(max(semi_join(vot_bc, vot_fab, by = "id_votacion")$fecha)), "\n")
cat("totales: votaciones comunes", sum(resumen$votaciones_comunes),
    "| votos iguales", sum(resumen$votos_iguales),
    "| distintos", sum(resumen$votos_distintos),
    "| solo un lado", sum(resumen$solo_un_lado),
    "| solo B-Call antes", sum(resumen$solo_bcall_antes),
    "| solo B-Call después", sum(resumen$solo_bcall_despues), "\n")


# ---------------------------------------------------------------------------
# PASO 4 — exportar
# ---------------------------------------------------------------------------
comparacion <- cmp %>%
  select(periodo, id_parlamentario, nombre_completo, partido, bloque, d1, d2, wnom, bay, dyn) %>%
  arrange(periodo, d1)
write_csv(comparacion, file.path(DIR_OUT, "comparacion.csv"), na = "")
write_csv(resumen, file.path(DIR_OUT, "resumen_periodos.csv"), na = "")

# nombre en orden natural para la página
nombre_natural <- function(x) {
  partes <- str_split_fixed(x, ", ", 2)
  if_else(partes[, 2] == "", x, paste(partes[, 2], partes[, 1]))
}
pagina <- list(
  dip = comparacion %>%
    transmute(p = periodo, n = nombre_natural(nombre_completo), pa = replace_na(partido, "IND"), b = bloque,
              d1 = round(d1, 3), d2 = round(d2, 3), w = round(wnom, 3)),
  per = resumen %>%
    mutate(across(c(r_wnom, r_bay, r_dyn, rho_d2_centro), ~ round(.x, 3)))
)
writeLines(paste0("window.BW = ", toJSON(pagina, dataframe = "rows", na = "null", auto_unbox = TRUE), ";"),
           file.path(DIR_OUT, "datos.js"), useBytes = TRUE)
cat("\nlisto\n")
