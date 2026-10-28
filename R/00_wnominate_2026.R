# =============================================================================
# 00_wnominate_2026.R — W-NOMINATE para 2026-2030, que la base de Fábrega
# (2025) no cubre, con la misma especificación de su script de replicación.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, antes de
# 01_comparar.R:
#   Rscript post_bcall_wnominate/R/00_wnominate_2026.R
#
# FUENTES
#   outputs/data/post_bcall/votos_periodo.rds   los votos con que se estimó
#                                               B-Call (post_bcall_periodos/R/01_base.R),
#                                               hasta el 9 de septiembre de 2026
#   data/fabrega_2025/A_Ideology_Estimation.R   la especificación que se copia
#
# DECISIONES (las de Fábrega, salvo el ancla)
#   1. A favor = 1, en contra = 0. Abstención, dispensa, pareo y ausencia
#      quedan como dato faltante.
#   2. wnominate() en una dimensión, con los valores por defecto del paquete
#      (minvotes = 20, lop = 0.025, trials = 3).
#   3. Ancla de polaridad. Fábrega usa el primero que esté en la matriz de
#      Urrutia, Melero, Kast, Macaya, Von Mühlenbrock y Kaiser. Ninguno es
#      diputado en 2026-2030, así que se usa a Jorge Alessandri (1009), el
#      mismo ancla de B-Call desde 2018.
#   4. Como prueba, la misma función se corre sobre los votos de 2022-2026 de
#      la base de B-Call y se compara con la estimación publicada de Fábrega.
#
# ESCRIBE
#   post_bcall_wnominate/datos/wnominate_2026_2030.csv   id_parlamentario, wnom
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(wnominate)
})

if (!file.exists("data/votaciones.rds")) stop("Correr desde la raíz de bcall_nominate_chile/.")
DIR_OUT <- "post_bcall_wnominate/datos"
ANCLAS  <- c("908", "865", "857", "942", "917", "1135", "1009")

votos <- read_rds("outputs/data/post_bcall/votos_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion), id_parlamentario = as.character(id_parlamentario))

estimar_wnom <- function(per) {
  m <- votos %>%
    filter(periodo == per) %>%
    transmute(id_parlamentario, id_votacion,
              v = case_when(voto_num == 1 ~ 1, voto_num == -1 ~ 0, TRUE ~ NA_real_)) %>%
    pivot_wider(names_from = id_votacion, values_from = v) %>%
    column_to_rownames("id_parlamentario") %>%
    as.matrix()
  ancla <- intersect(rownames(m), ANCLAS)[1]
  cat(per, "| matriz", nrow(m), "x", ncol(m), "| ancla", ancla, "\n")
  rc <- rollcall(m, yea = 1, nay = 0, missing = NA, legis.names = rownames(m),
                 vote.names = colnames(m), legis.data = NULL, desc = per)
  r <- wnominate(rc, dims = 1, polarity = ancla, verbose = FALSE)
  tibble(id_parlamentario = rownames(r$legislators), wnom = r$legislators$coord1D) %>%
    filter(!is.na(wnom))
}


# ---------------------------------------------------------------------------
# PASO 1 — prueba con 2022-2026
# ---------------------------------------------------------------------------
fab <- read_tsv("data/fabrega_2025/ideologia_congreso_chile_2002_2026_long_format.tab",
                na = c("", "NA"), show_col_types = FALSE) %>%
  filter(Period == "Periodo_6", !is.na(wnom_ideology)) %>%
  transmute(id_parlamentario = as.character(Id), wnom_fab = wnom_ideology)
prueba <- estimar_wnom("2022-2026") %>% inner_join(fab, by = "id_parlamentario")
cat("prueba 2022-2026 |", nrow(prueba), "diputados | r con Fábrega",
    round(cor(prueba$wnom, prueba$wnom_fab), 4), "\n")


# ---------------------------------------------------------------------------
# PASO 2 — 2026-2030
# ---------------------------------------------------------------------------
w26 <- estimar_wnom("2026-2030")
cat("2026-2030 |", nrow(w26), "diputados con estimación | en el tope:", sum(abs(w26$wnom) >= 0.99), "\n")
write_csv(w26 %>% mutate(wnom = round(wnom, 4)), file.path(DIR_OUT, "wnominate_2026_2030.csv"))
cat("listo\n")
