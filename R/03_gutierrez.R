# =============================================================================
# 03_gutierrez.R — el caso de Hugo Gutiérrez en 2010-2014, votación por
# votación, junto a los otros dos diputados del PC y un diputado de
# comparación.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_comparar.R:
#   Rscript post_bcall_wnominate/R/03_gutierrez.R
#
# FUENTES
#   post_bcall_wnominate/datos/comparacion.csv   bloque B-Call y d2
#   outputs/data/post_bcall/votos_periodo.rds y votaciones_periodo.rds
#   observatorio/parquet_respaldo_20260913/proyecto.parquet
#                                                títulos de los proyectos
#                                                (tramitacion.senado.cl)
#
# DECISIONES
#   1. Cada votación se ubica por el porcentaje que votó a favor en cada
#      bloque, entre quienes votaron a favor o en contra. Así la línea del
#      50% separa exactamente el lado de cada bloque.
#   2. El lado del bloque y "apartarse" se definen como en 01_comparar.R.
#   3. Diputados que se muestran: los del PC en 2010-2014 y, para comparar,
#      el diputado PS o PPD del bloque de izquierda con el d2 más cercano a
#      la mediana de ese bloque.
#
# ESCRIBE
#   post_bcall_wnominate/datos/gutierrez.csv   una fila por votación
#   post_bcall_wnominate/datos/gutierrez_diputados.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

PER <- "2010-2014"
DIR_OUT <- "post_bcall_wnominate/datos"

cmp <- read_csv(file.path(DIR_OUT, "comparacion.csv"), show_col_types = FALSE,
                col_types = cols(id_parlamentario = "c")) %>%
  filter(periodo == PER)

pc <- cmp %>% filter(partido == "PC")
med_izq <- median(cmp$d2[cmp$bloque == "left"])
comparacion <- cmp %>%
  filter(bloque == "left", partido %in% c("PS", "PPD")) %>%
  slice_min(abs(d2 - med_izq), n = 1, with_ties = FALSE)
dips <- bind_rows(pc %>% arrange(desc(d2)), comparacion) %>%
  select(id_parlamentario, nombre_completo, partido, wnom, d1, d2, contra_bloque, contra_acuerdo)
cat("diputados del PC en", PER, ":", nrow(pc), "\n")
print.data.frame(dips %>% mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)

v <- read_rds("outputs/data/post_bcall/votos_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion), id_parlamentario = as.character(id_parlamentario)) %>%
  filter(periodo == PER, !is.na(voto_num)) %>%
  inner_join(cmp %>% select(id_parlamentario, bloque), by = "id_parlamentario")

porvot <- v %>%
  filter(voto_num %in% c(1, -1)) %>%
  group_by(id_votacion, bloque) %>%
  summarise(si = sum(voto_num == 1), no = sum(voto_num == -1), .groups = "drop") %>%
  mutate(pct_si = si / (si + no), lado = case_when(si > no ~ 1, no > si ~ -1)) %>%
  select(id_votacion, bloque, pct_si, lado) %>%
  pivot_wider(names_from = bloque, values_from = c(pct_si, lado))

vot <- read_rds("outputs/data/post_bcall/votaciones_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion)) %>%
  filter(periodo == PER) %>%
  mutate(boletin = str_extract(numero_boletin, "^[0-9]+-[0-9]+"))
proy <- read_parquet("observatorio/parquet_respaldo_20260913/proyecto.parquet") %>%
  select(boletin, titulo) %>% distinct(boletin, .keep_all = TRUE)

por_dip <- v %>%
  filter(id_parlamentario %in% dips$id_parlamentario) %>%
  select(id_votacion, id_parlamentario, voto_num) %>%
  mutate(id_parlamentario = paste0("v_", id_parlamentario)) %>%
  pivot_wider(names_from = id_parlamentario, values_from = voto_num)

gut <- vot %>%
  select(id_votacion, fecha, boletin) %>%
  left_join(proy, by = "boletin") %>%
  inner_join(porvot, by = "id_votacion") %>%
  filter(!is.na(lado_left), !is.na(lado_right)) %>%
  mutate(acuerdo = lado_left == lado_right) %>%
  left_join(por_dip, by = "id_votacion") %>%
  arrange(fecha)

# resumen por diputado sobre estas votaciones, para el texto
res <- map_dfr(dips$id_parlamentario, function(id) {
  x <- gut[[paste0("v_", id)]]
  rompe <- !is.na(x) & x != gut$lado_left
  tibble(id_parlamentario = id, votaciones = sum(!is.na(x)), se_aparta = sum(rompe),
         en_acuerdos = sum(rompe & gut$acuerdo), acuerdos_votados = sum(!is.na(x) & gut$acuerdo),
         en_contra = sum(rompe & gut$acuerdo & x == -1), abstencion = sum(rompe & gut$acuerdo & x == 0))
})
dips <- dips %>% left_join(res, by = "id_parlamentario")
print.data.frame(dips %>% select(nombre_completo, votaciones, se_aparta, en_acuerdos, acuerdos_votados, en_contra, abstencion), row.names = FALSE)
cat("votaciones en el gráfico:", nrow(gut), "| con acuerdo de ambos bloques:", sum(gut$acuerdo), "\n")

# proyectos con más rupturas de Gutiérrez en acuerdos
gid <- dips$id_parlamentario[grepl("Gutiérrez Gálvez", dips$nombre_completo)]
xg <- gut[[paste0("v_", gid)]]
print.data.frame(gut %>% filter(acuerdo, !is.na(xg), xg != lado_left) %>%
                   count(boletin, titulo, sort = TRUE) %>% head(12) %>% mutate(titulo = str_trunc(titulo, 90)),
                 row.names = FALSE)

write_csv(gut %>% mutate(across(starts_with("pct_si"), ~ round(.x, 4))), file.path(DIR_OUT, "gutierrez.csv"), na = "")
write_csv(dips, file.path(DIR_OUT, "gutierrez_diputados.csv"), na = "")
cat("listo\n")
