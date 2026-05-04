# =============================================================================
# Diagnostico: por que jan/fev 2026 mudaram quando IBGE divulgou mar/2026?
# =============================================================================
#
# Script descartavel para Fase 1 do plano em
# .claude/plans/marcos-meu-co-autor-detectou-iridescent-pumpkin.md
#
# Constroi 3 toy examples para isolar a causa:
#   Toy 1 - cumsum puro (sintetico): mostra invariancia perfeita
#   Toy 2 - algoritmo do pacote (sintetico): mostra +delta identico no trio
#   Toy 3 - replica numerica do caso Marcos (com xlsx reais)
#
# Saida em texto vai para .claude/plans/diagnostico_marcos_toys_output.txt
# =============================================================================

suppressPackageStartupMessages({
  library(PNADCperiods)
  library(data.table)
})

stopifnot(packageVersion("PNADCperiods") == "0.1.2")

# Redireciona stdout para arquivo de log E console
log_path <- "C:/Users/antro/.claude/plans/diagnostico_marcos_toys_output.txt"
dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
sink(log_path, split = TRUE)
on.exit(sink(NULL), add = TRUE)

cat("============================================================\n")
cat(" DIAGNOSTICO MARCOS - Toy examples\n")
cat(" Data:", format(Sys.time(), "%Y-%m-%d %H:%M:%S BRT"), "\n")
cat(" PNADCperiods:", as.character(packageVersion("PNADCperiods")), "\n")
cat("============================================================\n\n")

# =============================================================================
# TOY 1 - cumsum puro 100% sintetico
# =============================================================================
cat("===== TOY 1: cumsum puro (sem .apply_final_adjustment) =====\n\n")

# 9 meses sinteticos com m_true conhecido (Jan 2012 ate Set 2012)
m_true <- c(100, 105, 110, 115, 120, 125, 130, 135, 140)
yyyymm <- c(201201, 201202, 201203, 201204, 201205, 201206,
            201207, 201208, 201209)
mesnotrim <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)

# Define rolling quarter pela definicao IBGE: rq[t] = (m[t-2]+m[t-1]+m[t])/3
# Primeiros 2 meses NAO tem rq (precisariam de m[Nov 2011] e m[Dez 2011])
rq_true <- rep(NA_real_, length(m_true))
for (t in 3:length(m_true)) {
  rq_true[t] <- mean(m_true[(t-2):t])
}

# Cumsum puro: m[t] = y0[mesnotrim] + cumsum(d3) por mesnotrim
# onde d3 = 3*(rq[t] - rq[t-1])
pure_cumsum <- function(rq, mesnotrim, y0) {
  n <- length(rq)
  d3 <- 3 * (rq - data.table::shift(rq, 1L))

  # Zera o primeiro de cada mesnotrim (sem incremento na origem)
  first_pos <- vapply(1:3, function(p) which(mesnotrim == p)[1], integer(1))
  d3[first_pos] <- NA_real_

  # Cumsum por mesnotrim
  cum <- rep(NA_real_, n)
  for (p in 1:3) {
    idx <- which(mesnotrim == p)
    d3p <- d3[idx]
    d3p[is.na(d3p)] <- 0
    cum[idx] <- cumsum(d3p)
  }

  m <- y0[mesnotrim] + cum
  m
}

# y0 = m_true[1:3] (definicao do procedimento)
y0 <- m_true[1:3]

cenario_A_rq <- rq_true[1:6]   # ate Jun 2012 (sem Jul, Ago, Set)
cenario_A_mn <- mesnotrim[1:6]

cenario_B_rq <- rq_true        # ate Set 2012 (acrescentamos Jul, Ago, Set)
cenario_B_mn <- mesnotrim

m_A <- pure_cumsum(cenario_A_rq, cenario_A_mn, y0)
m_B <- pure_cumsum(cenario_B_rq, cenario_B_mn, y0)

cat("m_true (Jan..Set 2012):\n")
print(setNames(m_true, yyyymm))
cat("\nCenario A - cumsum puro com rq ate Jun 2012:\n")
print(setNames(m_A, yyyymm[1:6]))
cat("\nCenario B - cumsum puro com rq ate Set 2012 (Jul/Ago/Set ADICIONADOS):\n")
print(setNames(m_B, yyyymm))
cat("\nDelta nos meses Jan..Jun 2012 (B - A):\n")
delta_AB <- m_B[1:6] - m_A
print(setNames(delta_AB, yyyymm[1:6]))
cat("\nCONCLUSAO TOY 1: maximo absoluto |delta| =",
    max(abs(delta_AB), na.rm = TRUE), "\n")
cat("                  m_A == m_true[1:6]? ",
    all(abs(m_A - m_true[1:6]) < 1e-12), "\n")
cat("                  m_B[1:6] == m_A?    ",
    all(abs(m_B[1:6] - m_A) < 1e-12), "\n\n")


# =============================================================================
# TOY 2 - algoritmo real do pacote com mesmo input
# =============================================================================
cat("===== TOY 2: algoritmo do pacote (com .apply_final_adjustment) =====\n\n")

# Constroi rolling_quarters no formato esperado pelo pacote
# Precisa de: anomesfinaltrimmovel, mesnotrim, e a serie
# Ex: serie sintetica chamada 'toyserie'.
# starting_points precisa de series_name, mesnotrim, y0.

build_dt <- function(rq, yyyymm_vec, mesnotrim_vec, series_name = "toyserie") {
  dt <- data.table(
    anomesfinaltrimmovel = yyyymm_vec,
    mesnotrim = as.integer(mesnotrim_vec)
  )
  dt[, (series_name) := rq]
  dt
}

sp_toy <- data.table(
  series_name = rep("toyserie", 3),
  mesnotrim = 1:3,
  y0 = m_true[1:3]
)

# Cenario A: rq ate Jun 2012 (3 trimestres completos: 201203, 201204, 201205, 201206)
# Para o pacote, precisa incluir TODOS os meses (com NA onde rq nao existe)
dt_A <- build_dt(rq_true[1:6], yyyymm[1:6], mesnotrim[1:6])
res_A <- mensalize_sidra_series(dt_A, starting_points = sp_toy,
                                 verbose = FALSE, compute_derived = FALSE)

# Cenario B: rq ate Set 2012
dt_B <- build_dt(rq_true, yyyymm, mesnotrim)
res_B <- mensalize_sidra_series(dt_B, starting_points = sp_toy,
                                 verbose = FALSE, compute_derived = FALSE)

cat("Cenario A - mensalize_sidra_series com rq ate Jun 2012:\n")
print(res_A[, .(anomesexato, m_toyserie)])
cat("\nCenario B - mensalize_sidra_series com rq ate Set 2012:\n")
print(res_B[, .(anomesexato, m_toyserie)])

# Compara meses comuns
common <- intersect(res_A$anomesexato, res_B$anomesexato)
m_A_pkg <- res_A[anomesexato %in% common, m_toyserie]
m_B_pkg <- res_B[anomesexato %in% common, m_toyserie]
cat("\nDelta nos meses comuns (B - A):\n")
delta_pkg <- m_B_pkg - m_A_pkg
print(setNames(round(delta_pkg, 6), common))
cat("\nCONCLUSAO TOY 2:\n")
cat("  Modulo maximo do delta:", max(abs(delta_pkg), na.rm = TRUE), "\n")
cat("  Delta e exatamente zero?",
    all(abs(delta_pkg) < 1e-9, na.rm = TRUE), "\n")

# Verifica se o delta nos 3 meses do trimestre 'novo' e identico
# (i.e., trimestre ate jun 2012: meses Apr, May, Jun -> tem trimestre completo
#  em ambos cenarios. Os trimestres a frente de Jun foram parcialmente
#  preenchidos: Jul/Ago tem rq mas sao 1o e 2o mes do proximo trimestre)
cat("\n")


# =============================================================================
# TOY 2b - simula EXATAMENTE o caso Marcos (sintetico)
# =============================================================================
cat("===== TOY 2b: simula 29/04 vs 30/04 (sintetico) =====\n\n")

# Ate Fev 2026 vs ate Mar 2026
# Para isso, criamos uma serie longa: 201201 a 202603 (171 meses)
n_meses <- 14*12 + 3  # Jan 2012 a Mar 2026 = 14 anos + Jan/Fev/Mar 2026 = 171
yyyymm_long <- integer(n_meses)
mesnotrim_long <- integer(n_meses)
for (i in seq_len(n_meses)) {
  ano <- 2012 + (i - 1) %/% 12
  mes <- ((i - 1) %% 12) + 1
  yyyymm_long[i] <- ano * 100 + mes
  mesnotrim_long[i] <- ((mes - 1) %% 3) + 1
}

# Gera m_true sintetico com tendencia + sazonal + ruido fixo (reproducivel)
set.seed(42)
m_true_long <- 100 + 0.5 * seq_len(n_meses) +
               2 * sin(2 * pi * seq_len(n_meses) / 12) +
               rnorm(n_meses, sd = 0.3)

# rq pela definicao
rq_true_long <- rep(NA_real_, n_meses)
for (t in 3:n_meses) rq_true_long[t] <- mean(m_true_long[(t-2):t])

# Pacote requer y0 das 3 mesnotrim
sp_long <- data.table(
  series_name = rep("toyserie", 3),
  mesnotrim = 1:3,
  y0 = m_true_long[1:3]
)

# Cenario "29/04": rq[Mar 2026] = NA (ainda nao publicado)
rq_29 <- rq_true_long
rq_29[n_meses] <- NA_real_  # Mar 2026 NA

dt_29 <- data.table(anomesfinaltrimmovel = yyyymm_long,
                    mesnotrim = mesnotrim_long,
                    toyserie = rq_29)
res_29 <- mensalize_sidra_series(dt_29, starting_points = sp_long,
                                  verbose = FALSE, compute_derived = FALSE)

# Cenario "30/04": rq[Mar 2026] disponivel
dt_30 <- data.table(anomesfinaltrimmovel = yyyymm_long,
                    mesnotrim = mesnotrim_long,
                    toyserie = rq_true_long)
res_30 <- mensalize_sidra_series(dt_30, starting_points = sp_long,
                                  verbose = FALSE, compute_derived = FALSE)

# Compara Jan 2026, Fev 2026, Mar 2026
focus <- c(202601, 202602, 202603)
m_29 <- res_29[anomesexato %in% focus, m_toyserie]
m_30 <- res_30[anomesexato %in% focus, m_toyserie]
cat("Mes,           29/04 (rq[Mar]=NA),  30/04 (rq[Mar] OK),   delta\n")
for (i in seq_along(focus)) {
  cat(sprintf("%6d         %15.10f   %15.10f   %+15.10f\n",
              focus[i],
              ifelse(length(m_29) >= i, m_29[i], NA_real_),
              ifelse(length(m_30) >= i, m_30[i], NA_real_),
              ifelse(length(m_29) >= i & length(m_30) >= i,
                     m_30[i] - m_29[i], NA_real_)))
}

# E os meses ANTERIORES (deveriam ser invariantes em ambos)
cat("\nMeses anteriores (Out 2025, Nov 2025, Dez 2025):\n")
prev <- c(202510, 202511, 202512)
m_29p <- res_29[anomesexato %in% prev, m_toyserie]
m_30p <- res_30[anomesexato %in% prev, m_toyserie]
cat("Mes,           29/04,                30/04,                 delta\n")
for (i in seq_along(prev)) {
  cat(sprintf("%6d         %15.10f   %15.10f   %+15.10f\n",
              prev[i], m_29p[i], m_30p[i], m_30p[i] - m_29p[i]))
}

cat("\nCONCLUSAO TOY 2b:\n")
delta_focus <- m_30 - m_29
cat("  Os 3 deltas em Jan/Fev/Mar 2026 sao identicos? ",
    diff(range(delta_focus, na.rm = TRUE)) < 1e-9, "\n", sep="")
cat("  Delta predito = rq[Mar] - (y_Jan+y_Fev+y_Mar)/3\n")

# Calcula manualmente o predito:
# y[t] = y0[mn[t]] + cumsum(d3)[t] -> isso e o m_29 (fallback no cenario A)
# avg_y = mean(y_Jan, y_Fev, y_Mar) = mean(m_29)
# rq[Mar] = rq_true_long[n_meses]
predito <- rq_true_long[n_meses] - mean(m_29)
cat("  Predito :", sprintf("%+15.10f", predito), "\n")
cat("  Observado (Jan):", sprintf("%+15.10f", delta_focus[1]), "\n")
cat("  Observado (Fev):", sprintf("%+15.10f", delta_focus[2]), "\n")
cat("  Observado (Mar):", sprintf("%+15.10f", delta_focus[3]), "\n\n")


# =============================================================================
# TOY 3 - replica numerica do caso Marcos (xlsx reais)
# =============================================================================
cat("===== TOY 3: replica numerica do caso Marcos =====\n\n")

xlsx_dir <- "C:/Users/antro/Downloads/dashboard-diagnostico-marcos"

if (!dir.exists(xlsx_dir)) {
  cat("DIR nao encontrado:", xlsx_dir, "- pulando Toy 3\n\n")
} else {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cat("readxl nao instalado - tentando install.packages('readxl', quiet=TRUE)\n")
    install.packages("readxl", quiet = TRUE,
                     repos = "https://cran.r-project.org")
  }

  read_marcos <- function(path) {
    df <- as.data.table(readxl::read_excel(path))
    df[, anomesexato := as.integer(anomesexato)]
    df
  }

  pop_29 <- read_marcos(file.path(xlsx_dir,
    "Baixadas_2026_04_29/pnadcperiods_populacao_2026-04-29.xlsx"))
  pop_30 <- read_marcos(file.path(xlsx_dir,
    "Baixadas_2026_04_30/pnadcperiods_populacao_2026-04-30.xlsx"))

  cat("Populacao - Jan/Fev/Mar 2026:\n")
  cat("Mes,    29/04,                  30/04,                  delta\n")
  m_29p <- pop_29[anomesexato %in% c(202601, 202602, 202603), .(anomesexato, monthly)]
  m_30p <- pop_30[anomesexato %in% c(202601, 202602, 202603), .(anomesexato, monthly)]
  cmp <- merge(m_29p, m_30p, by = "anomesexato",
               suffixes = c("_29", "_30"), all = TRUE)
  cmp[, delta := monthly_30 - monthly_29]
  print(cmp)

  # delta predito = rq[Mar] - mean(y_Jan, y_Fev, y_Mar)
  # No xlsx, 'quarterly' = rq do pacote para o mes correspondente
  # quarterly[Mar 2026] no arquivo de 30/04 deve ser 3*rq[Mar 2026]?
  # Ou e a media? Depende do que o dashboard mostra.
  cat("\n\nPara verificar identidade matematica do delta:\n")
  cat("  mean(monthly_30 em Jan/Fev/Mar 2026):",
      mean(m_30p$monthly[m_30p$anomesexato %in% c(202601, 202602, 202603)],
           na.rm = TRUE), "\n")
  cat("  Se monthly_30 satisfaz a restricao trimestral, ",
      "este valor deve ser igual ao rq[Mar 2026] do IBGE.\n")
}

cat("\n============================================================\n")
cat(" FIM DO DIAGNOSTICO\n")
cat("============================================================\n")
