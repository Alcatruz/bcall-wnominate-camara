# =============================================================================
# 02_simulacion.R — ¿sube d2 en el centro por construcción?
#
# Se corre desde cualquier carpeta (no lee datos):
#   Rscript post_bcall_wnominate/R/02_simulacion.R
#
# B-Call estandariza cada votación, le da signo según el bloque y calcula d2
# como la desviación estándar de esos valores por diputado. Un diputado de
# centro que vota siempre según su posición vota unas veces con un bloque y
# otras con el otro, así que su d2 puede subir sin error alguno. Esta
# simulación mide cuánto.
#
# DISEÑO
#   - 155 diputados en una dimensión. 65 por bloque con posiciones
#     N(-0,6; 0,15) y N(0,6; 0,15) ("extremos"), 15 "moderados" y 10
#     "erráticos", ambos con posiciones uniformes entre -0,25 y 0,25.
#   - 1.500 votaciones. Voto probit, P(lado derecho) = Phi(beta * (x - c)),
#     con beta = 8 para extremos y moderados (poco error) y beta = 1 para
#     los erráticos. El lado del sí se sortea en cada votación.
#   - Tres escenarios para los puntos de corte c: "bloques" N(0; 0,25), la
#     mayoría de las votaciones corta entre los bloques; "repartidos"
#     uniformes entre -0,9 y 0,9; "mezcla", mitad y mitad.
#   - 20 repeticiones por escenario. d1 y d2 con bcall_auto(), threshold 0,1,
#     igual que en el análisis por período.
#
# ESCRIBE
#   post_bcall_wnominate/datos/simulacion.csv   una fila por escenario y tipo
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(bcall)
})
set.seed(20261028)

DIR_OUT <- if (dir.exists("post_bcall_wnominate/datos")) "post_bcall_wnominate/datos" else "datos"
N_REP <- 20

una_camara <- function(cortes, n_vot = 1500) {
  pos  <- c(rnorm(65, -0.6, 0.15), rnorm(65, 0.6, 0.15), runif(15, -0.25, 0.25), runif(10, -0.25, 0.25))
  tipo <- c(rep("extremo", 130), rep("moderado", 15), rep("erratico", 10))
  beta <- ifelse(tipo == "erratico", 1, 8)
  c_j <- switch(cortes,
                bloques    = rnorm(n_vot, 0, 0.25),
                repartidos = runif(n_vot, -0.9, 0.9),
                mezcla     = ifelse(runif(n_vot) < 0.5, rnorm(n_vot, 0, 0.25), runif(n_vot, -0.9, 0.9)))
  lado <- sample(c(-1, 1), n_vot, replace = TRUE)
  p <- pnorm(outer(beta * pos, rep(1, n_vot)) - outer(beta, c_j))
  m <- ifelse(matrix(rbinom(length(p), 1, p), nrow = length(pos)) == 1, 1, -1) * rep(lado, each = length(pos))
  rownames(m) <- paste0("L", seq_along(pos))
  colnames(m) <- paste0("v", seq_len(n_vot))
  r <- bcall_auto(rollcall = as.data.frame(m), pivot = rownames(m)[which.max(pos)],
                  threshold = 0.1, verbose = FALSE)$results
  tibble(id = rownames(m), pos, tipo) %>%
    left_join(as.data.frame(r) %>% rownames_to_column("id") %>% select(id, d1, d2), by = "id") %>%
    mutate(d2_pct = percent_rank(d2))
}

sims <- expand_grid(cortes = c("bloques", "mezcla", "repartidos"), rep = seq_len(N_REP)) %>%
  mutate(res = map(cortes, una_camara)) %>%
  unnest(res)

resumen <- sims %>%
  group_by(cortes, tipo) %>%
  summarise(d2_pct_medio = mean(d2_pct), .groups = "drop") %>%
  left_join(sims %>% filter(tipo != "erratico") %>%
              group_by(cortes, rep) %>%
              summarise(rho = cor(d2, abs(d1), method = "spearman"), .groups = "drop") %>%
              group_by(cortes) %>%
              summarise(rho_coherentes = mean(rho), .groups = "drop"),
            by = "cortes")

print.data.frame(resumen %>% mutate(across(where(is.double), ~ round(.x, 3))), row.names = FALSE)
write_csv(resumen, file.path(DIR_OUT, "simulacion.csv"))
