# ==============================================================================
# Precompute Inequality Aggregates for Dashboard
# ==============================================================================
#
# Reads prepared microdata (from precompute_microdata_base.R) and computes:
#   A. Time series: Gini, Palma, percentile ratios, mean/median income
#   B. Income shares by quintile and decile
#   C. Lorenz curve points (sampled at 100 points per period)
#   D. Gini decomposition by income source
#
# All measures computed for each (month x demographic breakdown).
#
# Output files:
#   data/inequality_data.rds
#   data/income_shares_data.rds
#   data/lorenz_data.rds
#   data/income_decomposition_data.rds
#
# ==============================================================================

rm(list = ls())
gc()
options(scipen = 999)

library(data.table)
library(fst)

# ==============================================================================
# Paths
# ==============================================================================

project_dir   <- "d:/Dropbox/Artigos/mensalizacao_pnad"
dashboard_dir <- file.path(project_dir, "PNADCperiods-dashboard")
data_dir      <- file.path(dashboard_dir, "data")

# Source utility functions
source(file.path(dashboard_dir, "R", "utils_inequality.R"))

# ==============================================================================
# Load prepared microdata
# ==============================================================================

cache_file <- file.path(data_dir, "prepared_microdata.fst")
if (!file.exists(cache_file)) {
  stop("Prepared microdata not found. Run precompute_microdata_base.R first.")
}

message("Loading prepared microdata...")
d <- read_fst(cache_file, as.data.table = TRUE)
message(sprintf("Loaded %s rows, %d months.",
                format(nrow(d), big.mark = ","),
                uniqueN(d$ref_month_yyyymm)))

# ==============================================================================
# Define breakdown groups
# ==============================================================================

# Each breakdown is: (breakdown_type, column_name)
# For "overall", we compute once for the full population.
breakdown_specs <- list(
  list(type = "overall",      col = NULL),
  list(type = "sex",          col = "sexo"),
  list(type = "race",         col = "raca"),
  list(type = "education",    col = "faixa_educ"),
  list(type = "region",       col = "regiao"),
  list(type = "uf",           col = "uf_abbrev"),
  list(type = "urban_rural",  col = "urbano"),
  list(type = "age_group",    col = "faixa_idade")
)

# Income component columns for decomposition
income_vars <- c("rendaTrab_ha_pc", "rendaPrevid_pc", "rendaBPC_pc",
                 "rendaBolsaFam_pc", "rendaOutProgs_pc", "rendaSegDesemp_pc",
                 "rendaAlugueis_pc", "rendaOutros_pc")

# Income source labels
income_source_labels <- data.table(
  var = income_vars,
  source_id = c("labor", "pension", "bpc", "bolsa_familia",
                "other_programs", "unemployment_insurance", "rental", "other")
)

# ==============================================================================
# Helper: Compute inequality measures for a data subset
# ==============================================================================

compute_inequality_measures <- function(x, w) {
  list(
    gini         = weighted_gini(x, w),
    palma        = palma_ratio(x, w),
    p90p10       = percentile_ratio(x, w, 0.9, 0.1),
    p90p50       = percentile_ratio(x, w, 0.9, 0.5),
    p50p10       = percentile_ratio(x, w, 0.5, 0.1),
    top1_share   = top_share(x, w, 1),
    top5_share   = top_share(x, w, 5),
    top10_share  = top_share(x, w, 10),
    bottom50_share = bottom_share(x, w, 50),
    mean_income  = if (sum(w, na.rm = TRUE) > 0)
      sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE) else NA_real_,
    median_income = weighted_quantile(x, w, probs = 0.5)
  )
}

# ==============================================================================
# SECTION A: Time Series Aggregates
# ==============================================================================

message("\n=== Computing time series aggregates ===\n")

all_inequality <- list()
counter <- 0L

months <- sort(unique(d$ref_month_yyyymm))
n_months <- length(months)

for (spec in breakdown_specs) {
  btype <- spec$type
  bcol  <- spec$col

  message(sprintf("  Breakdown: %s", btype))

  if (btype == "overall") {
    # Compute for all months at once
    for (i in seq_along(months)) {
      m <- months[i]
      sub <- d[ref_month_yyyymm == m]
      if (nrow(sub) < 30) next

      x <- sub$hhinc_pc
      w <- sub$weight_monthly

      measures <- compute_inequality_measures(x, w)

      for (mname in names(measures)) {
        counter <- counter + 1L
        all_inequality[[counter]] <- data.table(
          ref_month_yyyymm = m,
          breakdown_type = "overall",
          breakdown_value = "Nacional",
          measure = mname,
          value = measures[[mname]],
          n_obs = nrow(sub)
        )
      }
    }
  } else {
    # Compute by group x month
    groups <- d[!is.na(get(bcol)), unique(get(bcol))]
    groups <- groups[!is.na(groups)]

    for (grp in groups) {
      for (i in seq_along(months)) {
        m <- months[i]
        sub <- d[ref_month_yyyymm == m & get(bcol) == grp]
        if (nrow(sub) < 30) next

        x <- sub$hhinc_pc
        w <- sub$weight_monthly

        measures <- compute_inequality_measures(x, w)

        for (mname in names(measures)) {
          counter <- counter + 1L
          all_inequality[[counter]] <- data.table(
            ref_month_yyyymm = m,
            breakdown_type = btype,
            breakdown_value = grp,
            measure = mname,
            value = measures[[mname]],
            n_obs = nrow(sub)
          )
        }
      }
    }
  }
}

inequality_data <- rbindlist(all_inequality)

# Add date column
inequality_data[, period := as.Date(paste0(
  ref_month_yyyymm %/% 100, "-",
  sprintf("%02d", ref_month_yyyymm %% 100), "-15"
))]

message(sprintf("  Total rows: %s", format(nrow(inequality_data), big.mark = ",")))

# Save
saveRDS(inequality_data, file.path(data_dir, "inequality_data.rds"))
message("  Saved: inequality_data.rds")

rm(all_inequality)
gc()

# ==============================================================================
# SECTION B: Income Shares (Quintile & Decile)
# ==============================================================================

message("\n=== Computing income shares ===\n")

all_shares <- list()
counter <- 0L

for (spec in breakdown_specs) {
  btype <- spec$type
  bcol  <- spec$col

  # Skip UF-level for shares (too many combinations)
  if (btype == "uf") next

  message(sprintf("  Breakdown: %s", btype))

  if (btype == "overall") {
    for (m in months) {
      sub <- d[ref_month_yyyymm == m]
      if (nrow(sub) < 50) next

      x <- sub$hhinc_pc
      w <- sub$weight_monthly

      for (n_groups in c(5, 10)) {
        shares <- income_shares(x, w, groups = n_groups)
        group_type <- if (n_groups == 5) "quintile" else "decile"

        counter <- counter + 1L
        all_shares[[counter]] <- data.table(
          ref_month_yyyymm = m,
          breakdown_type = "overall",
          breakdown_value = "Nacional",
          group_type = group_type,
          group_label = shares$group_label,
          share = shares$share
        )
      }
    }
  } else {
    groups <- d[!is.na(get(bcol)), unique(get(bcol))]
    groups <- groups[!is.na(groups)]

    for (grp in groups) {
      for (m in months) {
        sub <- d[ref_month_yyyymm == m & get(bcol) == grp]
        if (nrow(sub) < 50) next

        x <- sub$hhinc_pc
        w <- sub$weight_monthly

        for (n_groups in c(5, 10)) {
          shares <- income_shares(x, w, groups = n_groups)
          group_type <- if (n_groups == 5) "quintile" else "decile"

          counter <- counter + 1L
          all_shares[[counter]] <- data.table(
            ref_month_yyyymm = m,
            breakdown_type = btype,
            breakdown_value = grp,
            group_type = group_type,
            group_label = shares$group_label,
            share = shares$share
          )
        }
      }
    }
  }
}

shares_data <- rbindlist(all_shares)
shares_data[, period := as.Date(paste0(
  ref_month_yyyymm %/% 100, "-",
  sprintf("%02d", ref_month_yyyymm %% 100), "-15"
))]

message(sprintf("  Total rows: %s", format(nrow(shares_data), big.mark = ",")))

saveRDS(shares_data, file.path(data_dir, "income_shares_data.rds"))
message("  Saved: income_shares_data.rds")

rm(all_shares)
gc()

# ==============================================================================
# SECTION C: Lorenz Curve Points
# ==============================================================================

message("\n=== Computing Lorenz curve points ===\n")

# For Lorenz, compute only for "overall" (national) to keep file size manageable.
# For breakdowns, compute only by region and by race (most useful comparisons).
lorenz_breakdowns <- list(
  list(type = "overall", col = NULL),
  list(type = "race",    col = "raca"),
  list(type = "region",  col = "regiao")
)

all_lorenz <- list()
counter <- 0L

for (spec in lorenz_breakdowns) {
  btype <- spec$type
  bcol  <- spec$col

  message(sprintf("  Breakdown: %s", btype))

  if (btype == "overall") {
    for (m in months) {
      sub <- d[ref_month_yyyymm == m]
      if (nrow(sub) < 50) next

      lp <- lorenz_points(sub$hhinc_pc, sub$weight_monthly, n = 100)
      lp[, ref_month_yyyymm := m]
      lp[, breakdown_type := "overall"]
      lp[, breakdown_value := "Nacional"]

      counter <- counter + 1L
      all_lorenz[[counter]] <- lp
    }
  } else {
    groups <- d[!is.na(get(bcol)), unique(get(bcol))]
    groups <- groups[!is.na(groups)]

    for (grp in groups) {
      for (m in months) {
        sub <- d[ref_month_yyyymm == m & get(bcol) == grp]
        if (nrow(sub) < 50) next

        lp <- lorenz_points(sub$hhinc_pc, sub$weight_monthly, n = 100)
        lp[, ref_month_yyyymm := m]
        lp[, breakdown_type := btype]
        lp[, breakdown_value := grp]

        counter <- counter + 1L
        all_lorenz[[counter]] <- lp
      }
    }
  }
}

lorenz_data <- rbindlist(all_lorenz)
message(sprintf("  Total rows: %s", format(nrow(lorenz_data), big.mark = ",")))

saveRDS(lorenz_data, file.path(data_dir, "lorenz_data.rds"))
message("  Saved: lorenz_data.rds")

rm(all_lorenz)
gc()

# ==============================================================================
# SECTION D: Gini Decomposition by Income Source
# ==============================================================================

message("\n=== Computing Gini decomposition by income source ===\n")

# Only for national level (overall), monthly
all_decomp <- list()
counter <- 0L

for (m in months) {
  sub <- d[ref_month_yyyymm == m]
  if (nrow(sub) < 100) next

  # Use hhinc_pc as total (from VD5008, deflated)
  # But for decomposition, use component sum
  decomp <- gini_decomposition(
    dt = sub,
    income_vars = income_vars,
    total_var = "hhinc_pc",
    weight_var = "weight_monthly"
  )

  # Map variable names to source IDs
  decomp <- merge(decomp, income_source_labels,
                   by.x = "income_source", by.y = "var", all.x = TRUE)
  decomp[, income_source := source_id]
  decomp[, source_id := NULL]
  decomp[, ref_month_yyyymm := m]

  counter <- counter + 1L
  all_decomp[[counter]] <- decomp
}

decomp_data <- rbindlist(all_decomp)
decomp_data[, period := as.Date(paste0(
  ref_month_yyyymm %/% 100, "-",
  sprintf("%02d", ref_month_yyyymm %% 100), "-15"
))]

message(sprintf("  Total rows: %s", format(nrow(decomp_data), big.mark = ",")))

saveRDS(decomp_data, file.path(data_dir, "income_decomposition_data.rds"))
message("  Saved: income_decomposition_data.rds")

# ==============================================================================
# Summary
# ==============================================================================

message("\n=== Inequality Precompute Complete ===")
message(sprintf("  inequality_data.rds: %s rows",
                format(nrow(inequality_data), big.mark = ",")))
message(sprintf("  income_shares_data.rds: %s rows",
                format(nrow(shares_data), big.mark = ",")))
message(sprintf("  lorenz_data.rds: %s rows",
                format(nrow(lorenz_data), big.mark = ",")))
message(sprintf("  income_decomposition_data.rds: %s rows",
                format(nrow(decomp_data), big.mark = ",")))
message("\nDone.")
