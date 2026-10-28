# =============================================================================
# 02_votos.R — voto por voto, para la figura en que se elige período y
# diputado.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_comparar.R:
#   Rscript post_bcall_wnominate/R/02_votos.R
#
# FUENTES
#   post_bcall_wnominate/datos/comparacion.csv   bloque B-Call, d1, d2 y W-NOMINATE
#   outputs/data/post_bcall/votos_periodo.rds y votaciones_periodo.rds
#   observatorio/parquet_respaldo_20260913/proyecto.parquet
#                                                títulos de los proyectos
#                                                (tramitacion.senado.cl)
#
# DECISIONES
#   1. Solo votaciones en que los dos bloques B-Call tienen lado: la opción
#      (a favor o en contra) que eligió la mayoría de sus miembros. Si empatan,
#      la votación no entra. Igual que en 01_comparar.R.
#   2. Cada votación se ubica por el porcentaje que votó a favor en cada
#      bloque, entre quienes votaron a favor o en contra.
#   3. Apartarse del bloque es no votar su lado, abstenerse incluido. Se
#      guarda cada votación en que el diputado se apartó, con su voto.
#   4. Para el panel mensual, cuántas veces votó el diputado cada mes y, por
#      bloque, la mediana mensual del porcentaje de votos apartados, entre
#      diputados con al menos 5 votos ese mes.
#
# ESCRIBE
#   post_bcall_wnominate/datos/votos_<período>.json   uno por período
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(jsonlite)
})

DIR_OUT <- "post_bcall_wnominate/datos"
PER <- c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022", "2022-2026")

cmp <- read_csv(file.path(DIR_OUT, "comparacion.csv"), show_col_types = FALSE,
                col_types = cols(id_parlamentario = "c")) %>%
  group_by(periodo) %>% mutate(d2_pct = percent_rank(d2)) %>% ungroup()
votos <- read_rds("outputs/data/post_bcall/votos_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion), id_parlamentario = as.character(id_parlamentario)) %>%
  filter(periodo %in% PER, !is.na(voto_num))
vot <- read_rds("outputs/data/post_bcall/votaciones_periodo.rds") %>%
  mutate(id_votacion = as.character(id_votacion), boletin = str_extract(numero_boletin, "^[0-9]+-[0-9]+"))
proy <- read_parquet("observatorio/parquet_respaldo_20260913/proyecto.parquet") %>%
  select(boletin, titulo) %>% distinct(boletin, .keep_all = TRUE)

# nombre en orden natural
nombre_natural <- function(x) {
  partes <- str_split_fixed(x, ", ", 2)
  if_else(partes[, 2] == "", x, paste(partes[, 2], partes[, 1]))
}

for (p in PER) {
  dips <- cmp %>% filter(periodo == p)
  v <- votos %>% filter(periodo == p) %>%
    inner_join(dips %>% select(id_parlamentario, bloque), by = "id_parlamentario")

  lados <- v %>%
    filter(voto_num %in% c(1, -1)) %>%
    group_by(id_votacion, bloque) %>%
    summarise(si = sum(voto_num == 1), no = sum(voto_num == -1), .groups = "drop") %>%
    mutate(pct = si / (si + no), lado = case_when(si > no ~ 1, no > si ~ -1)) %>%
    select(id_votacion, bloque, pct, lado) %>%
    pivot_wider(names_from = bloque, values_from = c(pct, lado)) %>%
    filter(!is.na(lado_left), !is.na(lado_right))

  vv <- vot %>% filter(periodo == p) %>% select(id_votacion, fecha, boletin) %>%
    inner_join(lados, by = "id_votacion") %>%
    arrange(fecha, id_votacion) %>%
    mutate(i = row_number() - 1, mes = format(fecha, "%Y-%m"))
  meses <- sort(unique(vv$mes))

  vd <- v %>% inner_join(vv %>% select(id_votacion, i, mes, lado_left, lado_right), by = "id_votacion") %>%
    mutate(propio = if_else(bloque == "left", lado_left, lado_right),
           aparta = voto_num != propio,
           acuerdo = lado_left == lado_right)

  # resumen por diputado
  res <- vd %>% group_by(id_parlamentario) %>%
    summarise(n = n(), ap = sum(aparta), ac = sum(acuerdo), ap_ac = sum(aparta & acuerdo), .groups = "drop")

  # mediana mensual de votos apartados por bloque
  mens_bloque <- vd %>% group_by(bloque, mes, id_parlamentario) %>%
    summarise(n = n(), tasa = mean(aparta), .groups = "drop") %>%
    filter(n >= 5) %>%
    group_by(bloque, mes) %>% summarise(med = round(median(tasa), 4), .groups = "drop")

  lista_dips <- dips %>% left_join(res, by = "id_parlamentario") %>% arrange(nombre_completo)
  salida <- list(
    periodo = p,
    meses = meses,
    vot = vv %>% transmute(x = round(100 * pct_left, 1), y = round(100 * pct_right, 1),
                           l = lado_left, r = lado_right, b = boletin, m = match(mes, meses) - 1,
                           f = format(fecha, "%d-%m-%Y")),
    tit = vv %>% distinct(boletin) %>% filter(!is.na(boletin)) %>% left_join(proy, by = "boletin") %>%
      { setNames(as.list(str_trunc(coalesce(.$titulo, ""), 110)), .$boletin) },
    bloque_mes = list(
      left  = mens_bloque %>% filter(bloque == "left") %>% { setNames(as.list(.$med), .$mes) },
      right = mens_bloque %>% filter(bloque == "right") %>% { setNames(as.list(.$med), .$mes) }
    ),
    dips = map(seq_len(nrow(lista_dips)), function(k) {
      d <- lista_dips[k, ]
      mio <- vd %>% filter(id_parlamentario == d$id_parlamentario)
      apar <- mio %>% filter(aparta) %>% arrange(i)
      porm <- mio %>% count(mes) %>% mutate(m = match(mes, meses) - 1)
      list(id = d$id_parlamentario, n = nombre_natural(d$nombre_completo), pa = replace_na(d$partido, "IND"),
           b = d$bloque, d1 = round(d$d1, 2), d2 = round(d$d2, 2), d2p = round(d$d2_pct, 2), w = round(d$wnom, 2),
           nv = d$n, ap = d$ap, ac = d$ac, apac = d$ap_ac,
           # votaciones en que se apartó: índice y voto
           a = map2(apar$i, apar$voto_num, ~ c(.x, .y)),
           # votos emitidos por mes: índice del mes y cantidad
           vm = map2(porm$m, porm$n, ~ c(.x, .y)))
    })
  )
  f <- file.path(DIR_OUT, paste0("votos_", p, ".json"))
  writeLines(toJSON(salida, auto_unbox = TRUE, na = "null", digits = NA), f, useBytes = TRUE)
  cat(p, "| votaciones", nrow(vv), "| diputados", nrow(lista_dips), "|", round(file.size(f) / 1024), "KB\n")
}

# cifras para el texto
cat("\n")
casos <- c("Gutiérrez Gálvez, Hugo", "Boric Font, Gabriel", "Kast Rist, José Antonio", "De la Carrera Correa, Gonzalo")
print.data.frame(cmp %>% filter(nombre_completo %in% casos) %>%
                   select(periodo, nombre_completo, partido, bloque, d2, d2_pct, contra_bloque, contra_acuerdo) %>%
                   mutate(across(where(is.double), ~ round(.x, 3))) %>% arrange(nombre_completo, periodo), row.names = FALSE)
