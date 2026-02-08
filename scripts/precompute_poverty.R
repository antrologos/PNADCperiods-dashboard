# ==============================================================================
# Precompute Poverty Aggregates for Dashboard
# ==============================================================================
#
# Reads prepared microdata (from precompute_microdata_base.R) and computes:
#   - FGT-0 (headcount), FGT-1 (gap), FGT-2 (severity) for each poverty line
#   - Number of poor, total population, mean income of poor
#   - 3-month centered rolling averages
#   - All measures by (month x poverty_line x demographic breakdown)
#
# Poverty lines:
#   - World Bank (June 2025 update, 2021 PPP): $3.00, $4.20, $8.30/day
#   - Brazil official: 1/4 MW, 1/2 MW (historical values, deflated)
#
# Output:
#   data/poverty_data.rds
#
# ==============================================================================

rm(list = ls())
gc()
options(scipen = 999)

library(data.table)
library(fst)
library(deflateBR)

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

cache_file <- file.path(project_dir, "data", "processed", "prepared_microdata.fst")
if (!file.exists(cache_file)) {
  stop("Prepared microdata not found. Run precompute_microdata_base.R first.")
}

message("Loading prepared microdata...")
d <- read_fst(cache_file, as.data.table = TRUE)
message(sprintf("Loaded %s rows, %d months.",
                format(nrow(d), big.mark = ","),
                uniqueN(d$ref_month_yyyymm)))

# ==============================================================================
# Define Poverty Lines
# ==============================================================================

message("\n=== Defining poverty lines ===\n")

# --- World Bank lines (June 2025 update, 2021 PPP) ---
wb_lines <- get_wb_poverty_lines(reference_date = "12/2025")
message("World Bank poverty lines (monthly BRL, Dec 2025):")
for (i in seq_len(nrow(wb_lines))) {
  message(sprintf("  %s: $%.2f/day PPP -> R$ %.2f/month",
                  wb_lines$line_id[i],
                  wb_lines$usd_per_day[i],
                  wb_lines$brl_monthly_ref[i]))
}

# --- Brazilian minimum wage lines ---
mw_table <- get_historical_minimum_wage()
message("\nBrazilian MW-based lines (deflated to Dec 2025):")

# For each year, compute 1/4 MW and 1/2 MW in Dec 2025 BRL
mw_table[, mw_quarter := salario_minimo / 4]
mw_table[, mw_half := salario_minimo / 2]

# Deflate each year's MW to Dec 2025
# Use mid-year as nominal date
mw_table[, nominal_date := as.Date(sprintf("%d-07-01", ano))]
mw_table[, mw_quarter_dec2025 := deflateBR::inpc(
  mw_quarter,
  nominal_dates = nominal_date,
  real_date = "12/2025"
)]
mw_table[, mw_half_dec2025 := deflateBR::inpc(
  mw_half,
  nominal_dates = nominal_date,
  real_date = "12/2025"
)]

message(sprintf("  1/4 MW range: R$ %.2f - R$ %.2f (Dec 2025 BRL)",
                min(mw_table$mw_quarter_dec2025),
                max(mw_table$mw_quarter_dec2025)))
message(sprintf("  1/2 MW range: R$ %.2f - R$ %.2f (Dec 2025 BRL)",
                min(mw_table$mw_half_dec2025),
                max(mw_table$mw_half_dec2025)))

# Build combined poverty line table
# WB lines: single value (constant across all periods)
# MW lines: varies by year (but all deflated to Dec 2025)
poverty_lines <- list()

# WB lines
for (i in seq_len(nrow(wb_lines))) {
  poverty_lines[[wb_lines$line_id[i]]] <- list(
    line_id = wb_lines$line_id[i],
    type = "fixed",  # same value for all periods
    value = wb_lines$brl_monthly_ref[i]
  )
}

# MW lines (year-varying)
poverty_lines[["br_quarter_mw"]] <- list(
  line_id = "br_quarter_mw",
  type = "by_year",
  values = mw_table[, .(ano, value = mw_quarter_dec2025)]
)

poverty_lines[["br_half_mw"]] <- list(
  line_id = "br_half_mw",
  type = "by_year",
  values = mw_table[, .(ano, value = mw_half_dec2025)]
)

# ==============================================================================
# Define breakdown groups
# ==============================================================================

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

# ==============================================================================
# Compute Poverty Measures
# ==============================================================================

message("\n=== Computing poverty measures ===\n")

months <- sort(unique(d$ref_month_yyyymm))
all_poverty <- list()
counter <- 0L

for (line_name in names(poverty_lines)) {
  pline <- poverty_lines[[line_name]]

  message(sprintf("  Poverty line: %s", line_name))

  for (spec in breakdown_specs) {
    btype <- spec$type
    bcol  <- spec$col

    if (btype == "overall") {
      group_values <- list("Nacional")
    } else {
      group_values <- as.list(d[!is.na(get(bcol)), unique(get(bcol))])
      group_values <- group_values[!sapply(group_values, is.na)]
    }

    for (grp in group_values) {
      for (m in months) {
        # Subset data
        if (btype == "overall") {
          sub <- d[ref_month_yyyymm == m]
        } else {
          sub <- d[ref_month_yyyymm == m & get(bcol) == grp]
        }

        if (nrow(sub) < 10) next

        # Get poverty line value
        if (pline$type == "fixed") {
          z <- pline$value
        } else {
          # Year-varying (MW-based)
          yr <- m %/% 100
          val_row <- pline$values[ano == yr]
          if (nrow(val_row) == 0) next
          z <- val_row$value[1]
        }

        # Compute FGT measures
        x <- sub$hhinc_pc
        w <- sub$weight_monthly
        fgt_result <- fgt_all(x, z, w)

        counter <- counter + 1L
        all_poverty[[counter]] <- data.table(
          ref_month_yyyymm = m,
          poverty_line_id = line_name,
          poverty_line_value = z,
          breakdown_type = btype,
          breakdown_value = as.character(grp),
          fgt0 = fgt_result$fgt0,
          fgt1 = fgt_result$fgt1,
          fgt2 = fgt_result$fgt2,
          n_poor = fgt_result$n_poor,
          total_pop = fgt_result$total_pop,
          mean_income_poor = fgt_result$mean_income_poor,
          n_obs = nrow(sub)
        )
      }
    }
  }
}

poverty_data <- rbindlist(all_poverty)

# Add date column
poverty_data[, period := as.Date(paste0(
  ref_month_yyyymm %/% 100, "-",
  sprintf("%02d", ref_month_yyyymm %% 100), "-15"
))]

message(sprintf("  Total rows: %s", format(nrow(poverty_data), big.mark = ",")))

rm(all_poverty)
gc()

# ==============================================================================
# Compute 3-month Rolling Averages
# ==============================================================================

message("\n=== Computing 3-month rolling averages ===\n")

# Sort for rolling computation
setorder(poverty_data, poverty_line_id, breakdown_type, breakdown_value,
         ref_month_yyyymm)

# Compute rolling averages within each group
poverty_data[, `:=`(
  fgt0_smooth = frollmean(fgt0, 3, align = "center"),
  fgt1_smooth = frollmean(fgt1, 3, align = "center"),
  fgt2_smooth = frollmean(fgt2, 3, align = "center")
), by = .(poverty_line_id, breakdown_type, breakdown_value)]

message("  Rolling averages computed.")

# ==============================================================================
# Save
# ==============================================================================

saveRDS(poverty_data, file.path(data_dir, "poverty_data.rds"))
message(sprintf("\nSaved: poverty_data.rds (%s rows)",
                format(nrow(poverty_data), big.mark = ",")))

# ==============================================================================
# Summary
# ==============================================================================

message("\n=== Poverty Precompute Complete ===")
message(sprintf("  Poverty lines: %d", length(poverty_lines)))
message(sprintf("  Breakdowns: %d", length(breakdown_specs)))
message(sprintf("  Months: %d", length(months)))
message(sprintf("  Total rows: %s", format(nrow(poverty_data), big.mark = ",")))

# Show sample of results
message("\nSample (overall, wb_830):")
sample <- poverty_data[breakdown_type == "overall" & poverty_line_id == "wb_830"]
if (nrow(sample) > 0) {
  setorder(sample, ref_month_yyyymm)
  message(sprintf("  First month (%d): FGT-0 = %.3f",
                  sample$ref_month_yyyymm[1], sample$fgt0[1]))
  message(sprintf("  Last month (%d):  FGT-0 = %.3f",
                  sample$ref_month_yyyymm[nrow(sample)],
                  sample$fgt0[nrow(sample)]))
}

message("\nDone.")
